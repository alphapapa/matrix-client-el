;;; matrix-client.el --- A Matrix client  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2016  Ryan Rix
;; Copyright (C) 2017-2019  Jay Kamat
;; Copyright (C) 2017-2019  Adam Porter

;; Original Author: Ryan Rix
;; Author/Maintainer: Adam Porter <adam@alphapapa.net>
;; Maintainer: Jay Kamat <jaygkamat@gmail.com>

;; Keywords: web, comm
;; Homepage: https://github.com/alphapapa/matrix-client.el

;; Package-Version: 0.2.0-pre
;; Package-Requires: ((emacs "25.1") (anaphora "1.0.0") (dash "2.13.0") (dash-functional "1.2.0") (f "0.17.2") (request "0.2.0") (a "0.1.0") (ov "1.0.6") (s "1.12.0") (tracking "2.9") (esxml "0.3.4") (ht "2.2") (rainbow-identifiers "0.2.2") (frame-purpose "1.2"))

;; This file is not part of GNU Emacs.

;;; License:

;; matrix-client.el is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; matrix-client.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; What started as a fork of, but became a rewrite of, Ryan Rix's
;; original matrix.el client.

;;; Code:

;; ARGH EMACS 26 WHY
(unless (fboundp 'if-let)
  (defalias 'if-let 'if-let*)
  (defalias 'when-let 'when-let*))

;;;; Requirements

(require 'cl-lib)
(require 'calendar)

(require 'anaphora)
(require 'f)
(require 'ov)
(require 'tracking)

(require 'matrix-api-r0.3.0)
(require 'matrix-client-faces)
(require 'matrix-client-room)
(require 'matrix-notifications)
(require 'matrix-client-images)
(require 'matrix-client-frame)
(require 'matrix-client-room-list)

;;;; Variables

(defvar matrix-client-sessions nil
  "List of active sessions.")

(defvar matrix-client-input-prompt "▶ ")

(defvar matrix-client-midnight-timer nil
  "Timer used to update date headers at midnight.")

(defgroup matrix-client nil
  "Options for Matrix Client."
  :group 'applications)

(defcustom matrix-client-mark-modified-rooms nil
  "Mark room buffers as modified when new events arrive.
This is disabled by default, because marking room buffers as
not modified when the user sees them requires adding a function
to `buffer-list-modified-hook', which carries some risk of
interfering with Emacs behavior and degrading its performance."
  :type 'boolean)

(defcustom matrix-client-render-presence t
  "Show presence changes in the main buffer windows."
  :type 'boolean)

(defcustom matrix-client-render-membership t
  "Show membership changes in the main buffer windows."
  :type 'boolean)

(defcustom matrix-client-render-html (featurep 'shr)
  "Render HTML messages in buffers. These are currently the
ad-hoc 'org.matrix.custom.html' messages that Vector emits."
  :type 'boolean)

(defcustom matrix-client-save-token nil
  "Save username and access token upon successful login."
  :type 'boolean)

(defcustom matrix-client-save-token-file "~/.cache/matrix-client.el.token"
  "Save username and access token to this file."
  :type 'file)

(defcustom matrix-client-send-typing-notifications-p t
  "Whether to send typing notifications.
On by default, as this is the typical behavior for a Matrix client."
  :type 'boolean)

(defcustom matrix-client-show-room-avatars t
  "Download and show room avatars."
  :type 'boolean)

(defcustom matrix-client-show-room-avatars-in-buffer-names t
  "Show room avatars in buffer names."
  :type 'boolean)

(defcustom matrix-client-room-avatar-in-buffer-name-size (default-font-height)
  "Size of room avatars in buffer names."
  :type '(choice (const :tag "Default font height" default-font-height)
                 (integer :tag "Size in pixels")
                 (function :tag "Custom function (should return integer)"))
  :set (lambda (option value)
         (set-default option (cl-typecase value
                               (function (funcall value))
                               (integer value)))))

(defcustom matrix-client-use-tracking nil
  "Enable tracking.el support in matrix-client."
  :type 'boolean)

;;;; Classes

(matrix-defclass matrix-client-session-client-data ()
  ((watchdog :initarg :watchdog
             :documentation "Watchdog timer used to restart stuck syncs."))
  "Client data stored in session objects.")

(matrix-defclass matrix-room-client-data ()
  ((buffer :initarg :buffer)
   (notification-rules :initarg :notification-rules
                       :documentation "List of functions to test events against; if one returns non-nil, a notification should be displayed.")
   (is-typing-timer
    :documentation "Timer that repeatedly sends is-typing notifications to the server while the user is typing.")
   (not-typing-timer
    :documentation "Timer that sends not-typing notification to the server after the user stops typing."))
  "Client data stored in room objects.")

;;;; Mode

(define-derived-mode matrix-client-mode fundamental-mode "Matrix"
  "Mode for Matrix room buffers."
  :group 'matrix-client
  ;; TODO: Add a new abbrev table that uses usernames, rooms, etc.
  :keymap matrix-client-mode-map)

;;;; Connect / disconnect

;;;###autoload
(defun matrix-client-connect (&optional user password access-token server)
  "Connect to the Matrix.
If already connected, `already-active' is returned; if
connecting, non-nil."
  (interactive)
  (if matrix-client-sessions
      ;; TODO: Already have active session: display list of buffers
      ;; FIXME: If login fails, it still shows as active.
      (progn
        (message "Already active")
        'already-active)
    ;; No existing session
    (if-let ((enabled matrix-client-save-token)
             (saved (matrix-client-load-token)))
        ;; Use saved token
        ;; FIXME: Change "username" to "user" when we no longer need compatibility with old code
        (setq user (a-get saved 'username)
              server (a-get saved 'server)
              access-token (a-get saved 'token)
              txn-id (a-get saved 'txn-id))
      ;; Not saved: prompt for username and password
      (setq user (or user (read-string "User ID: "))
            password (or password (read-passwd "Password: "))
            server (or server (read-string "Server: "
                                           (or (awhen (string-match (rx ":" (group (1+ anything))) user)
                                                 (match-string 1 user))
                                               "matrix.org")))))
    (if access-token
        ;; Use saved token and call post-login hook
        (matrix-client-login-hook (matrix-session :user user
                                                  :server server
                                                  :access-token access-token
                                                  :txn-id txn-id
                                                  :initial-sync-p t
                                                  :client-data (matrix-client-session-client-data)))
      ;; Log in with username and password
      (matrix-login (matrix-session :user user
                                    :server server
                                    :initial-sync-p t
                                    :client-data (matrix-client-session-client-data))
                    password))))

(defun matrix-client-login-hook (session)
  "Callback for successful login.
Add session to sessions list and run initial sync."
  (push session matrix-client-sessions)
  (matrix-client-sync session)
  (when matrix-client-save-token
    (matrix-client-save-token session))
  ;; NOTE: What happens if the system is asleep at midnight?
  (setq matrix-client-midnight-timer (run-at-time "00:00" 86400 #'matrix-client-update-all-date-headers))
  (message "Jacked in to %s.  Syncing..." (oref session server)))

(add-hook 'matrix-login-hook #'matrix-client-login-hook)

(defun matrix-client-watchdog-watch (session)
  "Start or reschedule watchdog timer for SESSION."
  (with-slots* (((client-data) session)
                ((watchdog) client-data))
    (when (timerp watchdog)
      (cancel-timer watchdog))
    (let ((fn (apply-partially #'matrix-client-watchdog-alert session)))
      (setf watchdog (run-at-time 40 nil fn)))))

(defun matrix-client-watchdog-alert (session)
  "Call `matrix-sync' again for SESSION.
To be called by watchdog timer."
  (with-slots (user) session
    (matrix-log (a-list 'fn 'matrix-client-watchdog-alert
                        'message (format$ "Watchdog alerted for session: $user")))
    (matrix-client-sync session)))

(defun matrix-client-sync (session)
  "Sync SESSION and start watchdog timer.
Deletes all outstanding sync request processes and buffers."
  (with-slots (pending-syncs) session
    (dolist (buffer pending-syncs)
      ;; Delete any outstanding syncs.  There can be only one.  I wish these hacky fixes weren't necessary,
      ;; but it seems that they are.  Without code like this, eventually some processes don't get their
      ;; sentinels called, and/or their buffers get left behind, and/or sync requests get duplicated, which
      ;; then race with each other and overwrite the sync token...such a mess.
      (when (buffer-live-p buffer)
        (when (get-buffer-process buffer)
          ;; Somehow a network process's buffer can remain after it has been deleted, so
          ;; we have to make sure the process is still alive before trying to delete it.
          (delete-process buffer))
        ;; Then we have to be sure to kill its buffer, too, because it seems that
        ;; sometimes the sentinel doesn't get called or doesn't kill the buffer.
        (kill-buffer buffer)))
    (setf pending-syncs nil))
  (matrix-sync session)
  (matrix-client-watchdog-watch session))

(add-hook 'matrix-after-sync-hook #'matrix-client-sync)

(defun matrix-client-save-token (session)
  "Save username and access token for session SESSION to file."
  ;; FIXME: This does not work with multiple sessions.
  ;; MAYBE: Could we use `savehist-additional-variables' instead of our own code for this?
  ;; TODO: Check if file exists; if so, ensure it has a proper header so we know it's ours.
  (with-temp-file matrix-client-save-token-file
    (with-slots (user server access-token txn-id) session
      ;; FIXME: Change "username" to "user" when we no longer need compatibility with old code
      ;; FIXME: Change token to access-token for clarity.
      (let ((print-level nil)
            (print-length nil))
        (prin1 (a-list 'username user
                       'server server
                       'token access-token
                       'txn-id txn-id)
               (current-buffer)))))
  ;; Ensure permissions are safe
  (chmod matrix-client-save-token-file #o600))

(defun matrix-client-load-token ()
  "Return saved username and access token from file."
  (when (f-exists? matrix-client-save-token-file)
    (read (f-read matrix-client-save-token-file))))

(defun matrix-client-disconnect (&optional logout)
  "Unplug from the Matrix.
If LOGOUT is non-nil, actually log out, canceling access
tokens (username and password will be required again)."
  (interactive "P")
  (cond (logout (seq-do #'matrix-logout matrix-client-sessions)
                ;; Remove saved token
                (f-delete matrix-client-save-token-file))
        ;; FIXME: This does not work for multiple sessions.
        (t (matrix-client-save-token (car matrix-client-sessions))))
  (--each matrix-client-sessions
    ;; Kill pending sync response buffer processes
    (with-slots* (((pending-syncs disconnect client-data) it)
                  ((watchdog) client-data))
      (when (timerp watchdog)
        (cancel-timer watchdog)
        (setf watchdog nil))
      (setq disconnect t)
      (ignore-errors
        ;; Ignore errors in case of "Attempt to get process for a dead buffer"
        (seq-do #'delete-process pending-syncs)))
    ;; Kill buffers
    (with-slots (rooms) it
      (--each rooms
        (kill-buffer (oref* it client-data buffer))))
    ;; Try to GC the session object.  Hopefully no timers or processes or buffers still hold a ref...
    (setf it nil))
  (cancel-timer matrix-client-midnight-timer)
  (setq matrix-client-midnight-timer nil)
  (setq matrix-client-sessions nil))

;;;; Commands

(defun matrix-client-switch-buffer ()
  "Prompt for a Matrix buffer and switch to it."
  (interactive)
  (--when-let (completing-read "Room: " (->> (oref* (car matrix-client-sessions) rooms)
                                             (--map (buffer-name (oref* it client-data buffer)))
                                             (-sort #'string-collate-lessp)))
    (switch-to-buffer it)))

;;;; Rooms

(defun matrix-client-update-all-date-headers ()
  "Update date headers in all rooms.
Intended to be called from a timer that runs at midnight."
  (dolist (session matrix-client-sessions)
    (dolist (room (oref session rooms))
      (with-room-buffer room
        (with-silent-modifications
          (matrix-client--update-date-headers))))))

;;;;; Timeline

(defun matrix-client-timeline (room event)
  "Process EVENT in ROOM."
  (pcase-let* (((map type) event))
    (if-fn-apply (concat "matrix-client-event-" type)
        (list room event)
      (matrix-unimplemented (format$ "Unimplemented client method: $fn-name")))))

;;;; Helper functions

(defun matrix-client-resize-avatar (avatar-string)
  "Return new avatar string for AVATAR-STRING, sized accordingly.
Sized according to `matrix-client-room-avatar-in-buffer-name-size'."
  ;; Make a new image to avoid modifying the avatar in the header.
  (if matrix-client-can-show-images
      (let ((avatar (cl-copy-list (get-text-property 0 'display avatar-string))))
        (setf (image-property avatar :max-width) matrix-client-room-avatar-in-buffer-name-size
              (image-property avatar :max-height) matrix-client-room-avatar-in-buffer-name-size)
        (concat (propertize " " 'display avatar) " "))
    (error "Emacs doesn't support images manipulation")))


(defun matrix-client-event-timestamp (data)
  "Return timestamp of event DATA."
  (let ((server-ts (float (a-get* data 'origin_server_ts)))
        (event-age (float (or (a-get* data 'unsigned 'age)
                              0))))
    ;; The timestamp and the age are in milliseconds.  We need
    ;; millisecond precision in case of two messages sent/received
    ;; within one second, but we need to return seconds, not
    ;; milliseconds.  So we divide by 1000 to get the timestamp in
    ;; seconds, but we keep millisecond resolution by using floats.
    (/ (- server-ts event-age) 1000)))

(defun matrix-client-linkify-urls (text)
  "Return TEXT with URLs in it made clickable."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (cl-loop while (re-search-forward (rx bow "http" (optional "s") "://" (1+ (not space))) nil 'noerror)
             do (make-text-button (match-beginning 0) (match-end 0)
                                  'mouse-face 'highlight
                                  'face 'link
                                  'help-echo (match-string 0)
                                  'action #'browse-url-at-mouse
                                  'follow-link t))
    (buffer-string)))

(defun matrix-client-buffer-visible-p (&optional buffer)
  "Return non-nil if BUFFER is currently visible.
If BUFFER is nil, use the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (or (eq buffer (window-buffer (selected-window)))
        (get-buffer-window buffer))))

(defun matrix--calendar-absolute-to-timestamp (absolute)
  "Convert ABSOLUTE day number to Unix timestamp.
Does not account for leap seconds.  ABSOLUTE should be the number
of days since 0000-12-31, e.g. as returned by
`calendar-absolute-from-gregorian'."
  ;; NOTE: This function should come with Emacs!
  (let* ((gregorian (calendar-gregorian-from-absolute absolute))
         (days-between (1+ (days-between (format "%s-%02d-%02d 00:00" (cl-caddr gregorian) (car gregorian) (cadr gregorian))
                                         "1970-01-01 00:00")))
         (seconds-between (* 86400 days-between)))
    (string-to-number (format-time-string "%s" seconds-between))))

(defun matrix-client--human-format-date (date)
  "Return human-formatted DATE.
DATE should be either an integer timestamp, or a string in
\"YYYY-MM-DD HH:MM:SS\" format.  The \"HH:MM:SS\" part is
optional."
  ;; NOTE: This seems to be the only format that `days-between' (and `date-to-time') accept.  This
  ;; appears to be undocumented; it just says, "date-time strings" without specifying what KIND of
  ;; date-time strings, leaving you to assume it pl.  I only found this out by trial-and-error.
  (setq date (cl-typecase date
               (integer (format-time-string "%F" (seconds-to-time date)))
               (float (format-time-string "%F" (seconds-to-time date)))
               (list (format-time-string "%F" (seconds-to-time date)))
               (string date)))
  (unless (string-match (rx (= 2 digit) ":" (= 2 digit) eos) date)
    (setq date (concat date " 00:00")))
  (let* ((difference (days-between (format-time-string "%F %T") date)))
    (cond ((= 0 difference) "Today")
          ((= 1 difference) "Yesterday")
          ((< difference 7) (format-time-string "%A" (date-to-time date)))
          (t (format-time-string "%A, %B %d, %Y" (date-to-time date))))))

;;;; Footer

(provide 'matrix-client)

;;; matrix-client.el ends here
