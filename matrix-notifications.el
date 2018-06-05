;;; matrix-notifications.el --- Notifications support for matrix-client  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018 Adam Porter
;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Jay Kamat <jaygkamat@gmail.com>
;; Created: 14 November 2017
;; Keywords: web, comm
;; Homepage: https://github.com/jgkamat/matrix-client-el
;; Package-Version: 0.3.0
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

(require 'notifications)

;;;; Customization

(defgroup matrix-client-notifications nil
  "Notification settings."
  :group 'matrix-client)

(defcustom matrix-client-enable-notifications t
  "Enable notifications for incoming messages in uncustomized rooms.
Notifications for individual rooms can be overridden in
`matrix-client-notifications-rooms'."
  :type 'boolean)

(defcustom matrix-client-notify-dispatcher #'matrix-client-notify-dispatch
  "Default function to send notifications for events.
This function is called for events in rooms for which
notifications are enabled.  It is not responsible for respecting
rooms' notification settings."
  :type 'function)

(defcustom matrix-client-notifications-rooms nil
  "Per-room notification settings."
  :type '(alist :key-type (string :tag "Room ID or alias")
                :value-type (list (choice :tag "Notifications"
                                          (const :tag "Enabled" :value t)
                                          (const :tag "Disabled" nil)
                                          (function :tag "For messages mentioning user displayname"
                                                    matrix-client--event-mentions-user-p)
                                          (function :tag "Predicate" :help-echo "Function called with event's ROOM, TYPE and DATA.  Returns non-nil when notification should be sent."))
                                  (choice :tag "Dispatch function"
                                          (const :tag "Default" nil)
                                          (function :tag "Custom function" :help-echo "Called with event's ROOM, TYPE and DATA.")))))

;;;; Variables

(defvar matrix-client-notify-hook nil
  "List of functions called for events.
Each is called with the room, event-type and the event data.")

(defvar matrix-client-recent-notifications nil
  "Alist of recent notifications mapping notification ID to related buffer.
Automatically trimmed to last 20 notifications.")

;;;; Commands

(defun matrix-client-set-room-notifications (room)
  "Set notifications for ROOM.
ROOM should be a `matrix-client' room object."
  (interactive (list matrix-client-room-object))
  (pcase-let* (((eieio name aliases id) room)
               (room-display-name (or (car aliases) name id))
               (`(,key . (,enabled ,fn)) (matrix-client--room-notification-settings room))
               (choices (a-list "on" t
                                "off" nil
                                "mentions" #'matrix-client--event-mentions-user-p))
               (choice-string)
               (choice))
    (unless key
      ;; No room-specific setting yet: invert default
      (setq key (or (elt aliases 0) id)
            enabled (not matrix-client-enable-notifications)))
    (when fn
      ;; Add custom function to choices
      (setq choices (append choices (a-list (concat "Custom function: %s" (symbol-name fn)) fn))))
    (setq choice-string (completing-read (format "Notifications for %s: " room-display-name) (mapcar #'car choices)))
    (setq choice (a-get choices choice-string))
    (map-put matrix-client-notifications-rooms key (list choice fn))
    ;; Save setting
    ;; NOTE: Saving the setting now seems like the right thing to do, but it does cause a short
    ;; delay while Emacs writes the custom file.  Is this the best way to handle this?
    (customize-save-variable 'matrix-client-notifications-rooms matrix-client-notifications-rooms)
    (message "Notifications for %s: %s" room-display-name choice-string)))

;;;; Functions

(cl-defmethod matrix-client-notify ((room matrix-client-room) event-type data)
  "When ROOM's notifications are enabled, run notify hooks and dispatcher for EVENT-TYPE with DATA."
  ;; MAYBE: I think event-type is in DATA, so maybe we should get it there instead of passing it.
  (unless matrix-client-initial-sync
    (pcase-let* ((`(,key . (,enabled ,dispatch-fn)) (matrix-client--room-notification-settings room))
                 (dispatch-fn (or dispatch-fn matrix-client-notify-dispatcher))
                 (notify-this-event-p (cl-case key
                                        ('nil  ;; No room-specific setting: use default
                                         matrix-client-enable-notifications)
                                        (t ;; Room-specific setting
                                         (if (functionp enabled)
                                             (funcall enabled room event-type data)
                                           enabled)))))
      (when notify-this-event-p
        (run-hook-with-args 'matrix-client-notify-hook room event-type data)
        (funcall dispatch-fn room event-type data)))))

(cl-defmethod matrix-client--event-mentions-user-p ((room matrix-client-room) event-type data)
  "Return non-nil if message mentions current user.
Checks message in event DATA for current user's display name."
  (pcase-let* (((eieio con) room)
               ((eieio displayname) con)
               ((map content) data)
               ((map body) content))
    (when (and displayname body)
      (string-match displayname body))))

(cl-defmethod matrix-client-notify-dispatch ((room matrix-client-room) event-type data)
  "Dispatch DATA and REST to appropriate notification function for EVENT-TYPE.
If no such function is defined, nothing happens."
  (let ((fn (intern-soft (concat "matrix-client-notify-" event-type))))
    (when (functionp fn)
      (funcall fn room data))))

(cl-defmethod matrix-client--room-notification-settings ((room matrix-client-room))
  "Return room-specific notification settings for ROOM.
Returns a cons with the car being the room ID or alias used for
the setting, and the cdr being the settings."
  (with-slots (aliases id) room
    (or (when-let ((settings (map-elt matrix-client-notifications-rooms id)))
          (cons id settings))
        (cl-loop for alias across aliases
                 when (map-elt matrix-client-notifications-rooms alias)
                 return (cons alias it)))))

;; MAYBE: Use a macro to define the handlers, because they have to
;; define their arg lists in a certain way, and the macro would take
;; care of that automatically.

(cl-defun matrix-client-notify-m.room.message (room data)
  "Show notification for m.room.message events.
DATA should be the `data' variable from the
`defmatrix-client-handler'.  ROOM should be the room object."
  (pcase-let* (((map content sender event_id) data)
               ((map body) content)
               (display-name (matrix-client-displayname-from-user-id room sender))
               (buffer (oref room :buffer))
               (id (notifications-notify :title (format "<b>%s</b>" display-name)
                                         ;; Encode the message as ASCII because dbus-notify
                                         ;; can't handle some Unicode chars.  And
                                         ;; `ignore-errors' doesn't work to ignore errors
                                         ;; from it.  Don't ask me why.
                                         :body (encode-coding-string body 'us-ascii)
                                         :category "im.received"
                                         :timeout 5000
                                         :app-icon nil
                                         :actions '("default" "Show")
                                         :on-action #'matrix-client-notification-show)))
    (map-put matrix-client-recent-notifications id (a-list 'buffer buffer
                                                           'event_id event_id))
    ;; Trim the list
    (setq matrix-client-recent-notifications (-take 20 matrix-client-recent-notifications))))

(defun matrix-client-notification-show (id key)
  "Show the buffer for a notification.
This function is called by `notifications-notify' when the user
activates a notification."
  (pcase-let* (((map (id notification)) matrix-client-recent-notifications)
               ((map buffer event_id) notification)
               (window (get-buffer-window buffer)))
    (raise-frame)
    (if window
        (select-window window)
      (switch-to-buffer buffer))
    ;; Go to relevant event
    (or (cl-loop initially do (goto-char (point-max))
                 for value = (get-text-property (point) 'event_id)
                 thereis (equal value event_id)
                 while (when-let (pos (previous-single-property-change (point) 'event_id))
                         (goto-char pos)))
        ;; Not found: go to last line
        (goto-char (point-max)))))

;;;; Footer

(provide 'matrix-notifications)

;;; matrix-notifications.el ends here
