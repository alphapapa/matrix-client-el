;;; matrix-client.el --- A minimal chat client for the Matrix.org RPC

;; Copyright (C) 2017-2018 Jay Kamat
;; Copyright (C) 2015 Ryan Rix
;; Author: Ryan Rix <ryan@whatthefuck.computer>
;; Maintainer: Jay Kamat <jaygkamat@gmail.com>
;; Created: 21 June 2015
;; Keywords: web, comm
;; Homepage: https://github.com/jgkamat/matrix-client-el
;; Package-Version: 0.1.2
;; Package-Requires: ((emacs "25.1") (dash "2.13.0") (f "0.17.2") (request "0.2.0") (a "0.1.0") (ov "1.0.6") (rainbow-identifiers "0.2.2") (s "1.12.0") (tracking "2.9"))

;; This file is not part of GNU Emacs.

;; matrix-client.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.
;;
;; matrix-client.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `matrix-client' is a chat client and API library for the Matrix.org decentralized RPC
;; system. `(package-install 'matrix-client)' and then either deploy your own homeserver or register
;; an account on the public homeserver https://matrix.org/beta/#/login . After you've done that, M-x
;; matrix-client will set you up with buffers corresponding to your Matrix rooms. You can join new
;; ones with /join, leave with /leave or /part, and hook in to the custom functions provided by
;; =matrix-client=.

;; Implementation-wise `matrix-client' itself provides most of the core plumbing for
;; an interactive Matrix chat client. It uses the Matrix event stream framework
;; to dispatch a global event stream to individual rooms. There are a set of
;; 'event handlers' and 'input filters' in `matrix-client-handlers' which are used to
;; implement the render flow of the various event types and actions a user can
;; take.

;;; Code:

(require 'matrix-api)
(require 'cl-lib)
(require 'seq)
(require 'url)

(require 'dash)
(require 'f)
(require 'ov)
(require 's)
(require 'tracking)

(require 'matrix-utils)

(defgroup matrix-client nil
  "Settings for `matrix-client'."
  :group 'communication
  :link '(url-link "https://github.com/jgkamat/matrix-client-legacy-el"))

(defcustom matrix-client-debug-events nil
  "When non-nil, log raw events to *matrix-events* buffer."
  :type 'boolean)

(defcustom matrix-client-event-poll-timeout 30000
  "How long to wait, in milliseconds, for a Matrix event in the EventStream before timing out and trying again."
  :type 'integer)

(defcustom matrix-client-backfill-count 10
  "How many messages to backfill at a time when scrolling."
  :type 'integer)

(defcustom matrix-client-backfill-threshold 5
  "How close to the top of a buffer point needs to be before backfilling events."
  :type 'integer)

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

(defcustom matrix-client-enable-watchdog t
  "If enabled, a timer will be run after twice the interval of
`matrix-client-event-poll-timeout'."
  :type 'boolean)

(defcustom matrix-client-show-room-avatars nil
  "Download and show room avatars."
  :type 'boolean)

(defcustom matrix-client-mark-modified-rooms t
  ;; This actually only controls whether a function is added to a hook
  ;; in each room's buffer.
  "Mark rooms with new messages as modified, and unmark them when their buffers are seen."
  :type 'boolean)

(defcustom matrix-client-hide-own-name nil
  "Hide your own username in room buffers.
Since you already know your name, you might want to hide your
name to make your own messages stand out more.  Or, you might
prefer to keep your name visible so you can see what your display
name is in each room."
  :type 'boolean)

(defcustom matrix-client-save-token nil
  "Save username and access token upon successful login."
  :type 'boolean)

(defcustom matrix-client-save-token-file "~/.cache/matrix-client.el.token"
  "Save username and access token to this file."
  :type 'file)

(defvar matrix-client-event-handlers '()
  "An alist of (type . function) handler definitions for various matrix types.

Each of these receives the raw event as a single DATA argument.
See `defmatrix-client-handler'. This value is used as the default
for every `matrix-client-connection' and can be overridden on a
connection basis.")

(defcustom matrix-client-use-tracking nil
  "Enable tracking.el support in matrix-client."
  :type 'boolean)

(defcustom matrix-client-save-outgoing-messages t
  "Save outgoing messages in kill ring before sending.
This way, in the event that a message gets lost in transit, the
user can recover it from the kill ring instead of retyping it."
  :type 'boolean)

;;;###autoload
(defclass matrix-client-connection (matrix-connection)
  ((running :initarg :running
            :initform nil
            :documentation "BOOL specifiying if the event listener is currently running.")
   (connect-ts :initarg :connect-ts
               :initform nil
               :documentation "Timestamp when this connection was established.  Used to hide uninteresting events that happened before the user connected.")
   (rooms :initarg :rooms
          :initform nil
          :documentation "List of matrix-room objects")
   (end-token :initarg :end-token
              :initform nil)
   (event-handlers :initarg :event-handlers
                   :initform nil
                   :documentation "An alist of (type . function) handler definitions for various matrix types.

Each of these receives the raw event as a single DATA argument.
See `defmatrix-client-handler'.")
   (event-hook :initarg :event-hook
               :initform nil
               :documentation "A lists of functions that are evaluated when a new event comes in.")
   (username :initarg :username
             :initform nil
             :documentation "Your Matrix username.")
   (displayname :initarg :displayname
                :initform nil
                :documentation "User's displayname.")
   (input-filters :initarg :input-filters
                  :initform nil
                  :documentation "List of functions to run input through.

Each of these functions take a single argument, the TEXT the user
inputs.  They can modify that text and return a new version of
it, or they can return nil to prevent further processing of it.")
   (watchdog-timer :initarg :watchdog-timer
                   :initform nil)
   (last-event-ts :initarg :last-event-ts
                  :initform 0))
  :documentation "This is the basic UI encapsulation of a Matrix connection.

To build a UI on top of `matrix-api' start here, wire up
event-handlers and input-filters.")

(defvar-local matrix-client-room-connection nil
  "`matrix-client-connection' object for the current buffer")

(defvar-local matrix-client-room-object nil
  "`matrix-client-room' object for the current buffer")

;; (defvar matrix-client-event-stream-end-token nil)

(defclass matrix-client-room ()
  ((con :initarg :con
        :initform nil)
   (buffer :initarg :buffer
           :initform nil
           :documentation "The buffer that contains the room's chat session")
   (name :initarg :room-name
         :initform nil
         :documentation "The name of the buffer's room.")
   (aliases :initarg :aliases
            :initform nil
            :documentation "The aliases of the buffer's room.")
   (topic :initarg :topic
          :initform nil
          :documentation "The topic of the buffer's room.")
   (avatar :initarg :avatar
           :initform nil
           :documentation "The room avatar.  This should be a string containing an image in its display properties.")
   (id :initarg :id
       :initform nil
       :documentation "The Matrix ID of the buffer's room.")
   (typers :initarg :typers
           :initform nil)
   (membership :initarg :membership
               :initform nil
               :documentation "The list of members of the buffer's room.")
   (start-token :initarg :start-token
                :initform nil
                :documentation "The earliest event-id in a room, used to fetch earlier events.")
   (end-token :initarg :end-token
              :initform nil
              :documentation "The most recent event-id in a room, used to push read-receipts to the server.")))

(cl-defmethod matrix-client-update-name ((room matrix-client-room))
  "Update ROOM's buffer's name.
If it only has two members, use the name of the other member.
Otherwise, use the room name or alias."
  (with-slots (con membership name aliases id) room
    (when-let ((username (oref con :username))
               ;; TODO: Make this a preference.  Some users might want
               ;; 1-1 chats always named after the other user, while
               ;; others might want them named with the room name.
               (buffer-name (cond ((> (length aliases) 0)
                                   ;; First, if the room has a friendly alias (e.g. #room), use it.
                                   (elt aliases 0)) ; The JSON list is converted to a vector.
                                  ((when membership
                                     (eq 2 (length membership)))
                                   ;; Next, if the room is a 1-1 chat, use the other member's name.
                                   (when-let ((username (cl-loop for member in membership
                                                                 ;; Get non-self member
                                                                 when (not (equal username (car member)))
                                                                 return (or (map-elt member 'displayname)
                                                                            (car member)))))
                                     (if (eq (current-buffer) (get-buffer username))
                                         username
                                       (generate-new-buffer-name username))))
                                  ;; Next, use the room's "name".
                                  (name)
                                  ;; Finally, use the plain room ID.
                                  (id)
                                  ;; If all else fails, use this string and give a warning.
                                  (t (progn
                                       (warn "Unknown room name for room: %s" room)
                                       "[unknown]")))))
      (rename-buffer buffer-name))))

(defvar-local matrix-client-room-typers nil
  "The list of members of the buffer's room who are currently typing.")

(defvar matrix-client-connections '()
  "Alist of (username . connection)")

(defvar matrix-client-after-connect-hooks
  '(matrix-client--set-own-displayname)
  "A list of functions to run when a new Matrix Client connection occurs.")

(defvar matrix-client-initial-sync nil)

(require 'matrix-client-handlers)
(require 'matrix-client-modes)

;;;###autoload
(defun matrix-client (username)
  "Connect to Matrix."
  (interactive "i")
  (let* ((base-url matrix-homeserver-base-url)
         ;; Pass a username in to get an existing connection
         (con (if username
                  (map-elt matrix-client-connections username)
                (matrix-client-connection matrix-homeserver-base-url
                                          :base-url matrix-homeserver-base-url
                                          :connect-ts (float-time)))))
    (when-let ((enabled matrix-client-save-token)
               (saved (matrix-client-load-token)))
      ;; Use saved token
      (oset con :username (a-get saved 'username))
      (oset con :token (a-get saved 'token)))
    (unless (oref con :token)
      ;; No access token: log in with username and password
      (matrix-client-login con username))
    (unless (oref con :running)
      ;; Disable notifications for first sync
      (setq matrix-client-initial-sync t)
      (matrix-client-start-watchdog con nil 120)
      (matrix-client-inject-event-listeners con)
      (matrix-client-handlers-init con)
      (matrix-sync con nil t matrix-client-event-poll-timeout
                   (apply-partially #'matrix-client-sync-handler con))
      (run-hook-with-args 'matrix-client-after-connect-hooks con))
    (map-put matrix-client-connections (oref con :username) con)
    (oset con :running t)
    (message "You're jacked in, welcome to Matrix. Your messages will arrive momentarily.")))

(cl-defmethod matrix-client-login ((con matrix-client-connection) &optional username)
  "Login to Matrix connection CON and get a token.
If [`matrix-client-use-auth-source'] is non-nil, attempt to log
in using data from auth-source.  Otherwise, prompt for username
and password."
  (let* ((username (or username (read-from-minibuffer "Username: ")))
         (password (read-passwd "Password: ")))
    (unless (string-match-p (rx "@" (1+ (not (any ":"))) ":" (1+ anything))
                            username)
      ;; Canonicalize username
      ;; TODO: "user-id" would be more descriptive than "username"
      (setq username (format "@%s:%s" username
                             (url-host (url-generic-parse-url matrix-homeserver-base-url)))))
    (when (matrix-login-with-password con username password)
      (oset con :username username)
      (when matrix-client-save-token
        (matrix-client-save-token con)))))

(cl-defmethod matrix-client-save-token ((con matrix-client-connection))
  "Save username and access token for connection CON to file."
  (with-temp-file matrix-client-save-token-file
    (with-slots (username token) con
      (prin1 (a-list 'username username
                     'token token)
             (current-buffer))))
  ;; Ensure permissions are safe
  (chmod matrix-client-save-token-file #o600))

(defun matrix-client-load-token ()
  "Return saved username and access token from file."
  (when (f-exists? matrix-client-save-token-file)
    (read (f-read matrix-client-save-token-file))))

(cl-defmethod matrix-client--set-own-displayname ((con matrix-client-connection))
  "Get and set own displayname for connection CON."
  (matrix-get-displayname con (oref con username)
                          (apply-partially (lambda (con data)
                                             (with-slots (displayname) con
                                               (if-let ((name (map-elt data 'displayname)))
                                                   (setq displayname name)
                                                 (warn "Unable to get displayname from server"))))
                                           con)))

(defun matrix-client-disconnect (&optional connection)
  "Disconnect from CONNECTION or all Matrix connections, killing room buffers."
  (interactive)
  (let ((connections (if connection
                         (list (cons nil connection))
                       matrix-client-connections)))
    (cl-loop for (_ . con) in connections
             do (progn
                  ;; TODO: Improve the structure of these lists.  It
                  ;; feels inconsistent and confusing.
                  (cl-loop for (_ . room) in (oref con :rooms)
                           do (kill-buffer (oref room :buffer)))
                  (oset con :running nil)))
    (setq matrix-client-connections (seq-difference matrix-client-connections
                                                    connections
                                                    (-lambda ((_ . a-con) (_ . b-con))
                                                      (equal (oref a-con :token) (oref b-con :token)))))))

(cl-defmethod matrix-client-start-watchdog ((con matrix-client-connection) &optional force timer-secs)
  (when (or force matrix-client-enable-watchdog)
    (let ((last-ts (oref con :last-event-ts))
          (next (oref con :end-token))
          (timer (oref con :watchdog-timer)))
      (if timer
          (if (> (* 1000 (- (float-time) last-ts))
                 matrix-client-event-poll-timeout)
              (progn
                ;; Timed out: resync
                (cancel-timer timer)
                ;; XXX Pull these fucking syncs out and bar them on (oref con :running)
                (when (oref con :running)
                  (message "Reconnecting you to Matrix, one moment please...")
                  (cancel-timer timer)
                  (matrix-sync con next nil matrix-client-event-poll-timeout
                               (apply-partially #'matrix-client-sync-handler con))))
            ;; Not timed out: just cancel timer
            (cancel-timer timer)))
      (let ((timer-secs (or timer-secs (/ (* 2 matrix-client-event-poll-timeout) 1000))))
        (oset con :watchdog-timer (run-with-timer timer-secs timer-secs
                                                  (apply-partially #'matrix-client-start-watchdog con)))))))

(cl-defmethod matrix-client-setup-room ((con matrix-client-connection) room-id)
  "Prepare and switch to buffer for ROOM-ID, and return room object."
  (when (get-buffer room-id)
    (kill-buffer room-id))
  (let* ((room-buf (get-buffer-create room-id))
         (room-obj (matrix-client-room room-id :buffer room-buf :con con)))
    (with-current-buffer room-buf
      (matrix-client-mode)
      (visual-line-mode 1)
      (setq buffer-undo-list t)
      ;; Unset buffer's modified status when it's selected
      (when matrix-client-mark-modified-rooms
        (add-hook 'buffer-list-update-hook #'matrix-client-buffer-list-update-hook 'append 'local))
      (erase-buffer)
      (matrix-client-render-message-line room-obj)
      (matrix-client-insert-last-seen-overlay)
      (when matrix-client-use-tracking
        (tracking-mode 1)))
    (switch-to-buffer room-buf)
    (set (make-local-variable 'matrix-client-room-connection) con)
    (set (make-local-variable 'matrix-client-room-object) room-obj)
    (push (cons room-id room-obj) (oref con :rooms))
    (oset-multi room-obj
      :id room-id
      :buffer room-buf)
    room-obj))

(defun matrix-client-room--insert-date-header (timestamp)
  "Insert date header for TIMESTAMP at current position in current buffer."
  (let* ((visible-header (propertize (concat " " (format-time-string "%Y-%m-%d" timestamp) "\n")
                                     'face 'matrix-client-date-header))
         (whole-header (propertize (concat "\n"
                                           visible-header
                                           "\n")
                                   'matrix-client-date-header t
                                   'matrix-header-day-number (time-to-days timestamp)
                                   'read-only t)))
    (insert whole-header)))

(cl-defmethod matrix-client-fetch-history ((room matrix-client-room))
  "Load earlier messages for ROOM."
  (with-slots (con start-token id) room
    (matrix-client-room-banner room "Loading history...")
    (matrix-get-messages con id
                         :from start-token
                         :callback (apply-partially #'matrix-client-fetch-history-handler room))))

(cl-defmethod matrix-client-fetch-history-handler ((room matrix-client-room) data)
  (pcase-let* (((map start end chunk) data)
               (matrix-client-enable-notifications nil)) ; Silence notifications for old messages
    (cl-loop for event across chunk
             do (matrix-client-room-event room event))
    ;; NOTE: When direction is "b", as it is when fetching earlier messages, the "end" token is the
    ;; earliest chronologically, so it becomes the room's new "start" token.  Not confusing at
    ;; all... (maybe API 0.3.0 is better)
    (oset room :start-token end)
    (matrix-client-room-banner room nil)))

(defun matrix-client-scroll-down ()
  "Call `scroll-down-command'.  If point is at the top of the buffer, load history."
  (interactive)
  (if (= (line-number-at-pos (point)) 1)
      (matrix-client-fetch-history matrix-client-room-object)
    (let ((scroll-error-top-bottom t))
      (scroll-down-command))))

(cl-defmethod matrix-client-room-banner ((room matrix-client-room) message)
  "Display MESSAGE in a banner overlay at top of ROOM's buffer.
If MESSAGE is nil, clear existing message."
  (with-current-buffer (oref room :buffer)
    (let ((ov (or (car (ov-in 'matrix-client-banner))
                  (ov (point-min) (point-min)
                      'matrix-client-banner t)))
          (message (when message
                     (propertize message
                                 'face 'font-lock-comment-face))))
      (ov-set ov 'before-string message))))

(cl-defmethod matrix-client-sync-handler ((con matrix-client-connection) data)
  ;; NOTE: This function, in addition to `matrix-client-handlers-init', roughly corresponds with the Python SDK at
  ;; <https://github.com/matrix-org/matrix-python-sdk/blob/master/matrix_client/client.py#L486>.
  (when (oref con :running)

    ;; Kill buffers for left rooms
    (cl-loop for room in (a-get* data 'rooms 'leave)
             do (let* ((room-id (symbol-name (car room)))
                       (room (matrix-client-room-for-id con room-id)))
                  (when (and room (oref room :buffer))
                    (kill-buffer (oref room :buffer)))))

    ;; Join joined rooms
    (cl-loop for room-data in (a-get* data 'rooms 'join)
             do (let* ((room-id (symbol-name (car room-data)))
                       (room (or (a-get (oref con :rooms) room-id)
                                 (matrix-client-setup-room con room-id)))
                       (room-events (cdr room-data))
                       (prev-batch (a-get* room-events 'timeline 'prev_batch)))
                  ;; For some reason, the events are in arrays instead of lists.
                  (cl-loop for event across (a-get* room-events 'ephemeral 'events)
                           ;; e.g. typing
                           do (matrix-client-room-event room event))
                  (cl-loop for event across (a-get* room-events 'state 'events)
                           do (matrix-client-room-event room event))
                  (cl-loop for event across (a-get* room-events 'timeline 'events)
                           do (matrix-client-room-event room event))
                  (oset room :start-token prev-batch)))

    ;; FIXME: `matrix-client-invite-room' is unimplemented.  Looking
    ;; at the API <https://matrix.org/docs/spec/client_server/r0.2.0.html#post-matrix-client-r0-rooms-roomid-invite>,
    ;; I'm not sure this is even necessary.
    ;; Process invitations
    ;; (--each (a-get* data 'rooms 'invite)
    ;;   (matrix-client-invite-room con data))

    ;; Process next batch
    (let ((next (map-elt data 'next_batch)))
      (oset-multi con
        :end-token next
        :last-event-ts (float-time))
      (matrix-client-start-watchdog con)
      (matrix-sync con next nil matrix-client-event-poll-timeout
                   (apply-partially #'matrix-client-sync-handler con)))
    (when matrix-client-initial-sync
      (setq matrix-client-initial-sync nil))))

(cl-defmethod matrix-client-room-event ((room matrix-client-room) event)
  "Handle state events from a sync."
  (when-let ((con (oref room :con))
             (fns (oref con :event-hook)))
    (--each fns
      (funcall it con room event))))

(cl-defmethod matrix-client-render-event-to-room ((con matrix-client-connection) room item)
  "Feed ITEM in to its proper `matrix-client-event-handlers' handler."
  ;; NOTE: It's tempting to use `map-elt' here, but it uses `eql' to
  ;; compare keys, and since the keys are strings, that doesn't work.
  ;; MAYBE: Perhaps we should change the keys to symbols someday...
  (when-let ((type (a-get item 'type))
             (handler (a-get (oref con :event-handlers) type)))
    (funcall handler con room item)))

(defun matrix-client--get-date-header (timestamp)
  "Return position of appropriate date header in current buffer for TIMESTAMP.
Creates a new header if necessary."
  (cl-labels ((prev-header-pos () (matrix--prev-property-change (point) 'matrix-header-day-number))
              (current-header-day-number () (get-text-property (point) 'matrix-header-day-number)))
    (let* ((target-day-number (time-to-days timestamp)))
      (goto-char (1- (matrix-client--prompt-position)))
      (catch 'found
        (while t
          (if-let ((prev-header-pos (prev-header-pos)))
              (progn
                ;; Found a previous header
                (goto-char prev-header-pos)
                (let ((current-header-day-number (current-header-day-number)))
                  (cond ((= current-header-day-number target-day-number)
                         ;; Found correct header
                         (throw 'found prev-header-pos))
                        ((< current-header-day-number target-day-number)
                         ;; Found earlier header: insert new one after current header's position
                         (goto-char (matrix--next-property-change (point) 'matrix-header-day-number
                                                                  (1- (matrix-client--prompt-position))))
                         (matrix-client-room--insert-date-header timestamp)
                         ;; Return position after new header
                         (throw 'found (point)))))
                ;; Wrong header: keep looking
                )
            ;; No more headers found: insert new header here
            (matrix-client-room--insert-date-header timestamp)
            ;; Return one character before the end of the new header.  This is sort of a tiny hack
            ;; that is simpler than handling the logic in `matrix-client-insert'.  It fixes the case
            ;; when a new header for older messages is first inserted.
            (throw 'found (1- (point)))))))))

(defun matrix-client--prompt-position ()
  "Return position of prompt in current buffer."
  (ov-beg (car (ov-in 'matrix-client-prompt))))

(cl-defmethod matrix-client-insert ((room matrix-client-room) string)
  "Insert STRING into ROOM's buffer.
STRING should have a `timestamp' text-property."
  (with-current-buffer (oref room :buffer)
    (let* ((inhibit-read-only t)
           (timestamp (get-text-property 0 'timestamp string))
           (day-number (time-to-days timestamp))
           (event-id (get-text-property 0 'event_id string))
           (header-pos (matrix-client--get-date-header timestamp))
           (limit (or (matrix--next-property-change header-pos 'matrix-header-day-number)
                      (1- (matrix-client--prompt-position))))
           (insertion-pos (if-let ((next-event-pos (matrix--next-property-change header-pos 'timestamp limit)))
                              ;; Existing messages found beneath header
                              (catch 'stop
                                (while (< next-event-pos limit)
                                  (when (> (get-text-property next-event-pos 'timestamp) timestamp)
                                    ;; Found newer timestamp: insert message here
                                    (throw 'stop next-event-pos))
                                  (setq next-event-pos (matrix--next-property-change next-event-pos 'timestamp limit)))
                                ;; No more messages beneath this header: insert message here
                                next-event-pos)
                            ;; No messages under this header: return limit, which is next header pos or end of buffer
                            limit))
           (non-face-properties (cl-loop for (key val) on (text-properties-at 0 string) by #'cddr
                                         unless (eq key 'face)
                                         append (list key val))))
      (goto-char insertion-pos)
      ;; Ensure event before point doesn't have the same ID
      (unless (when-let ((previous-event-pos (matrix--prev-property-change (point) 'timestamp)))
                (string-equal event-id (get-text-property previous-event-pos 'event_id)))
        ;; Insert the message
        (insert (apply #'propertize (concat string "\n") 'read-only t non-face-properties))
        ;; Update tracking
        (unless (matrix-client-buffer-visible-p)
          (set-buffer-modified-p t)
          (when matrix-client-use-tracking
            ;; TODO handle faces when receving highlights
            (tracking-add-buffer (current-buffer))))))))

(cl-defmethod matrix-client-inject-event-listeners ((con matrix-client-connection))
  "Inject the standard event listeners."
  (unless (oref con :event-hook)
    (oset con :event-hook '(matrix-client-debug-event-maybe
                            matrix-client-render-event-to-room))))

(cl-defmethod matrix-client-debug-event-maybe ((con matrix-client-connection) room data)
  "Debug DATA to *matrix-events* if `matrix-client-debug-events' is non-nil."
  (when matrix-client-debug-events
    (with-current-buffer (get-buffer-create "*matrix-events*")
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "\n" (prin1-to-string data))))))

(defun matrix-client-update-header-line (room)
  "Update the header line of the current buffer for ROOM.
Also update prompt with typers."
  ;; Disable when tabbar mode is on
  (unless (and (boundp 'tabbar-mode) tabbar-mode)
    (pcase-let* (((eieio avatar typers name topic buffer) room)
                 (name (when name
                         (propertize name 'face 'font-lock-keyword-face)))
                 (ov (car (ov-in 'matrix-client-prompt)))
                 (typers-string (s-join ", " (cl-loop for user across typers
                                                      collect (matrix-client-displayname-from-user-id room user))))
                 (prompt (if (> (length typers) 0)
                             (concat (propertize (concat "Typing: " typers-string)
                                                 'face 'font-lock-comment-face)
                                     "\n" matrix-client-input-prompt)
                           matrix-client-input-prompt)))
      (with-current-buffer buffer
        (ov-set ov 'before-string prompt)
        (setq header-line-format (concat avatar (format "%s: %s" name topic)))))))

(defvar matrix-client-input-prompt "▶ ")

(cl-defmethod matrix-client-render-message-line ((room matrix-client-room room))
  ;; FIXME: Why is there an extra "room" the arg list?  EIEIO docs
  ;; don't seem to mention this.
  "Insert a message input at the end of the buffer."
  (goto-char (point-max))
  (let ((inhibit-read-only t)
        (ov-sticky-front t))
    (insert (propertize "\n" 'read-only t)
            "\n")
    (ov (point) (point)
        'before-string (concat (propertize "\n" 'face '(:height 0.1))
                               matrix-client-input-prompt)
        'matrix-client-prompt t)))

(defun matrix-client-send-active-line ()
  "Send the current message-line text after running it through input-filters."
  (interactive)
  (goto-char (ov-end (car (ov-in 'matrix-client-prompt t))))
  (let* ((input (prog1
                    (buffer-substring-no-properties (point) (point-max))
                  (delete-region (point) (point-max))))
         (room matrix-client-room-object)
         (con (oref room :con))
         (input-filters (oref con :input-filters)))
    (unless (s-blank-str? input)
      (when matrix-client-save-outgoing-messages
        (push input kill-ring))
      (cl-reduce 'matrix-client-run-through-input-filter
                 input-filters
                 :initial-value input)
      (matrix-client-update-last-seen room))))

(defun matrix-client-run-through-input-filter (text filter)
  "Run each TEXT through a single FILTER.  Used by `matrix-client-send-active-line'."
  (when text
    (funcall filter
             (oref matrix-client-room-object :con)
             text)))

(defun matrix-client-send-to-current-room (con message)
  "Send a string TEXT to the current buffer's room."
  (let* (;; FIXME: Setting `room' here seems to be unnecessary,
         ;; because the function is called in the context of `room'.
         ;; Until adding this comment, the `let*' was `let', so `room'
         ;; was coming from the surrounding context, not this.
         (room matrix-client-room-object)
         ;; FIXME: We shouldn't need to get `con' again here, because
         ;; it's passed to the function.
         (con (when room
                (oref room :con)))
         (room-id (when room
                    (oref room :id))))
    (matrix-send-message con room-id message)))

(cl-defmethod matrix-client-update-last-seen ((room matrix-client-room) &rest _)
  "Move the last-seen overlay to after the last message in ROOM."
  (with-slots (buffer) room
    (with-current-buffer buffer
      ;; FIXME: Does this need to be when-let?  Shouldn't these always be found?
      (when-let ((seen-ov (car (ov-in 'matrix-client-last-seen)))
                 (target-pos (1- (matrix-client--prompt-position))))
        (ov-move seen-ov target-pos target-pos)))))

(defun matrix-client-window-change-hook ()
  "Send a read receipt if necessary."
  ;; FIXME: Unimplemented.
  ;; (when (and matrix-client-room-id matrix-client-room-end-token)
  ;;   (message "%s as read from %s" matrix-client-room-end-token matrix-client-room-id)
  ;;   (matrix-mark-as-read matrix-client-room-id matrix-client-room-end-token))
  )

(provide 'matrix-client)

;;; matrix-client.el ends here
