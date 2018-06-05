;;; matrix-client-handlers.el --- Event handlers for Matrix.org RPC

;; Copyright (C) 2017-2018 Jay Kamat
;; Copyright (C) 2015 Ryan Rix
;; Author: Ryan Rix <ryan@whatthefuck.computer>
;; Maintainer: Ryan Rix <ryan@whatthefuck.computer>
;; Created: 21 June 2015
;; Keywords: web, comm
;; Homepage: https://github.com/jgkamat/matrix-client-el
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;; matrix-client-handlers.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.
;;
;; matrix-client-handlers.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file holds the standard matrix-client handlers and input filters. See the docstring of
;; [`matrix-client-handlers-init'] and [`defmatrix-client-handler'] for information about these.

;;; Code:

(require 'browse-url)
(require 'rainbow-identifiers)

(require 'matrix-client-images)
(require 'matrix-notifications)

(defcustom matrix-client-rainbow t
  "Colorize other users' names and messages uniquely.
Uses the `rainbow-identifiers' package to choose colors.
Customize that package's options to adjust color selection.")

(cl-defmethod matrix-client-handlers-init ((con matrix-client-connection))
  "Set up all the matrix-client event type handlers.

Each matrix-client-event-handler is an alist of matrix message type and
the function that handles them.  Currently only a single handler
for each event is supported.  The handler takes a single argument,
DATA, which is a `json-read' object from the Event stream.  See
the Matrix spec for more information about its format."
  ;; NOTE: Roughly corresponds with the Matrix Python SDK here:
  ;; <https://github.com/matrix-org/matrix-python-sdk/blob/master/matrix_client/client.py#L486>
  ;; FIXME: `matrix-client-window-change-hook' should be renamed, and
  ;; is currently unimplemented anyway.
  (push 'matrix-client-window-change-hook window-configuration-change-hook)
  (with-slots (event-handlers input-filters) con
    (unless event-handlers
      (setq event-handlers (a-list "m.room.message" 'matrix-client-handler-m.room.message
                                   "m.lightrix.pattern" 'matrix-client-handler-m.lightrix.pattern
                                   "m.room.topic" 'matrix-client-handler-m.room.topic
                                   "m.room.name" 'matrix-client-handler-m.room.name
                                   "m.room.member" 'matrix-client-handler-m.room.member
                                   "m.room.aliases" 'matrix-client-handler-m.room.aliases
                                   "m.room.avatar" 'matrix-client-handler-m.room.avatar
                                   "m.presence" 'matrix-client-handler-m.presence
                                   "m.typing" 'matrix-client-handler-m.typing)))
    (unless input-filters
      (setq input-filters '(matrix-client-input-filter-who
                            matrix-client-input-filter-emote
                            matrix-client-input-filter-join
                            matrix-client-input-filter-leave
                            matrix-client-send-to-current-room)))))

(defun matrix-client-handler-m.room.avatar (con room data)
  (when matrix-client-show-room-avatars
    (pcase-let* (((map sender content) data)
                 ((map url) content)
                 (username (matrix-client-displayname-from-user-id room sender))
                 (own-username (oref* con :username))
                 (timestamp (matrix-client-event-data-timestamp data))
                 (time-string (format-time-string "[%T]" (seconds-to-time timestamp)))
                 (action (if url "changed" "removed"))
                 (msg (when (>= timestamp (oref* room :con :connect-ts))
                        (propertize (format "%s %s %s the room avatar" time-string username action)
                                    'timestamp timestamp
                                    'face 'matrix-client-notice))))
      (if url
          ;; New avatar
          ;; TODO: Maybe display the new avatar in the chat list, like Riot.
          (request (matrix-transform-mxc-uri url)
                   :parser (apply-partially #'matrix-client-parse-image room :max-width 32 :max-height 32)
                   :success (apply-partially #'matrix-client-room-avatar-callback
                                             :room room
                                             :message msg
                                             :max-width 32
                                             :max-height 32))
        ;; Avatar removed
        (oset room avatar nil)
        ;; TODO: A function to automatically propertize a string with its related event data would be nice.
        (when (>= timestamp (oref* room :con :connect-ts))
          (matrix-client-insert room msg))
        (matrix-client-update-header-line room))

      ;; Move last-seen line if it's our own message
      (when (equal own-username username)
        (matrix-client-update-last-seen room)))))

(cl-defmethod matrix-client-room-avatar-callback (&key (room matrix-client-room) message
                                                       data error-thrown symbol-status response
                                                       &allow-other-keys)
  "Set avatar for ROOM.
Image is passed from parser as DATA, which should be an image
object made with `create-image'.  This function should be called
as an async callback when the image is downloaded."
  (with-slots (avatar) room
    (when-let ((str (with-temp-buffer
                      (insert " ")
                      (insert-image data)
                      (insert " ")
                      (buffer-string))))
      (setq avatar str)
      (matrix-client-update-header-line room)
      (when message
        (matrix-client-insert room message)))))

(defmacro defmatrix-client-handler (msgtype varlist body)
  "Create an matrix-client-handler.

This macro generates a standard function which provides some
standard variables that each event handler can use to render an
event sanely.  It also sets [`inhibit-read-only'] to true to
allow you to freely render in to the buffer.

MSGTYPE is the type of the message to handle.

Provided Variables:

- `data': data from the event.
- `room': the `matrix-client-room' object that represents the room.
- `room-id': the Matrix room id the message is intended for
- `room-buf': the buffer tied to the Matrix room which the
  message is intended for.
- Any other variables in VARLIST are provided as well.

BODY is the function itself.  See, for example,
[`matrix-client-handler-m.presence'] for an example of what this looks
like."
  (let ((fname (intern (format "matrix-client-handler-%s" msgtype))))
    `(defun ,fname (con room data)
       (let* ((inhibit-read-only t)     ; FIXME: Can probably remove this now.
              (room-id (oref room :id))
              (room-buf (oref room :buffer))
              ,@varlist)
         (with-current-buffer room-buf
           (save-excursion
             (goto-char (point-max))
             (forward-line -1)
             (end-of-line)
             ,@body))))))

(defun matrix-client-linkify-urls (text)
  "Return TEXT with URLs in it made clickable."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (cl-loop while (re-search-forward (rx bow "http" (optional "s") "://" (1+ (not space))) nil 'noerror)
             do (make-text-button (match-beginning 0) (match-end 0)
                                  'mouse-face 'highlight
                                  'face 'matrix-client-link
                                  'help-echo (match-string 0)
                                  'action #'browse-url-at-mouse
                                  'follow-link t))
    (buffer-string)))

(defun matrix-client--user-id-face (user-id)
  "Return face for USER-ID."
  (if matrix-client-rainbow
      (let* ((hash (rainbow-identifiers--hash-function user-id))
             (face (rainbow-identifiers-cie-l*a*b*-choose-face hash)))
        face)
    'default))

(defmatrix-client-handler "m.room.message"
  ((content (map-elt data 'content))
   (msgtype (map-elt content 'msgtype))
   (format (map-elt content 'format))
   ;; We don't use `matrix-client-event-data-timestamp', because for
   ;; room messages, the origin_server_ts is the actual message time.
   (timestamp (/ (a-get* data 'origin_server_ts) 1000))
   (sender (map-elt data 'sender))
   (display-name (matrix-client-displayname-from-user-id room (map-elt data 'sender)))
   (own-user-id (oref* room :con :username)))

  ((when content
     ;; Redacted messages have no content, so we should do nothing for them.
     (pcase-let (((map event_id) data)
                 (metadata) (message) (matrix-image-url))
       (setq metadata (format "%s %s"
                              (format-time-string "[%T]" (seconds-to-time timestamp))
                              (if (and matrix-client-hide-own-name
                                       (equal display-name own-user-id))
                                  ""
                                (concat display-name " "))))
       (setq message (string-trim
                      ;; Trim messages because HTML ones can have extra newlines
                      (pcase msgtype
                        ("m.emote"
                         (concat "* " (map-elt content 'body)))
                        ((guard (and matrix-client-render-html (string= "org.matrix.custom.html" format)))
                         (with-temp-buffer
                           (insert (map-elt content 'formatted_body))
                           (goto-char (point-min))
                           (while (re-search-forward "\\(<br />\\)+" nil t)
                             (replace-match "<br />"))
                           (let ((document (libxml-parse-html-region (point) (point-max))))
                             (erase-buffer)
                             (shr-insert-document document)
                             (goto-char (point-min))
                             (delete-blank-lines)
                             (buffer-string))))
                        ("m.image"
                         (setq matrix-image-url (matrix-transform-mxc-uri (or (map-elt content 'url)
                                                                              (map-elt content 'thumbnail_url))))
                         (concat (map-elt content 'body)
                                 ": "
                                 (matrix-client-linkify-urls matrix-image-url)))
                        (_ (matrix-client-linkify-urls (map-elt content 'body))))))

       ;; Apply face for own messages
       (let (metadata-face message-face)
         (cond ((string= sender own-user-id)
                (setq metadata-face 'matrix-client-own-metadata
                      message-face 'matrix-client-own-messages))
               ((string= msgtype "m.notice")
                (setq metadata-face 'matrix-client-notice-metadata
                      message-face 'matrix-client-notice))
               (t
                (setq metadata-face 'matrix-client-metadata
                      message-face (matrix-client--user-id-face sender))))
         ;; Use 'append so that link faces are not overridden.
         ;; Also apply message-face to metadata to colorize username.
         (add-face-text-property 0 (length metadata) metadata-face 'append metadata)
         (add-face-text-property 0 (length metadata) message-face 'append metadata)
         (add-face-text-property 0 (length message) message-face 'append message))

       ;; Insert metadata with message and add text properties
       (matrix-client-insert room (propertize (concat metadata message)
                                              'timestamp timestamp
                                              'display-name display-name
                                              'sender sender
                                              'event_id event_id))

       ;; Start image insertion if necessary
       (when matrix-client-show-images
         (cl-loop for url in (-non-nil (append (matrix-client--image-urls message)
                                               (list matrix-image-url)))
                  do (matrix-client-insert-image room event_id url)))

       ;; Move last-seen line if it's our own message
       (when (equal own-user-id sender)
         (matrix-client-update-last-seen room))

       ;; Notification
       (unless (equal own-user-id sender)
         (matrix-client-notify room "m.room.message" data))))))

(defun insert-read-only (text &rest extra-props)
  ;; NOTE: The "m.lightrix.pattern" handler is the only one that uses this now.
  "Insert a block of TEXT as read-only, with the ability to add EXTRA-PROPS such as face."
  (insert (apply #'propertize text 'read-only t extra-props)))

(defmatrix-client-handler "m.lightrix.pattern"
  ;; FIXME: Move this to separate file for Ryan.  :)
  ((content (map-elt data 'content)))
  ((insert "\n")
   (insert-read-only (format "🌄 %s --> " (matrix-client-displayname-from-user-id room (map-elt data 'user_id)))
                     face matrix-client-metadata)
   (insert-read-only (map-elt content 'pattern))))

(defmatrix-client-handler "m.room.member"
  ((content (map-elt data 'content))
   (user-id (map-elt data 'sender))
   (membership (map-elt content 'membership))
   (room-membership (oref room :membership))
   (display-name (map-elt content 'displayname)))
  ((assq-delete-all user-id room-membership)
   (let* ((timestamp (matrix-client-event-data-timestamp data))
          (action (pcase membership
                    ("join" (progn
                              ;; FIXME: Probably don't need all of content.
                              (push (cons user-id content) room-membership)
                              "Joined"))
                    ("leave" (progn
                               (cl-delete user-id room-membership :test #'string= :key #'car)
                               "Left"))
                    (_ (format "Unknown membership message: %s" membership))))
          ;; FIXME: display-name may be nil on "Left" events.
          (msg (propertize (format "%s: %s (%s)" action display-name user-id)
                           'timestamp timestamp
                           'face 'matrix-client-metadata)))
     (when (and matrix-client-render-membership
                (>= timestamp (oref* room :con :connect-ts)))
       (matrix-client-insert room msg)))
   (oset room :membership room-membership)
   (matrix-client-update-name room)))

(defun matrix-client-handler-m.presence (data)
  "Insert presence message into events buffer for DATA."
  (when matrix-client-render-presence
    (let* ((timestamp (matrix-client-event-data-timestamp data))
           (content (map-elt data 'content))
           (user-id (map-elt content 'user_id))
           (presence (map-elt content 'presence))
           (display-name (map-elt content 'displayname)))
      (with-current-buffer (get-buffer-create "*matrix-events*")
        (goto-char (point-max))
        (matrix-client-insert room (propertize (format "%s (%s) --> %s" display-name user-id presence)
                                               'face 'matrix-client-metadata
                                               'timestamp timestamp))))))

(defun matrix-client-event-data-timestamp (data)
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

(defmatrix-client-handler "m.room.name"
  ()
  ((oset room :room-name (a-get* data 'content 'name))
   (matrix-client-update-name room)
   (matrix-client-update-header-line room)
   (when-let ((event_id (a-get data 'event_id))
              (timestamp (matrix-client-event-data-timestamp data))
              (message (propertize (format "Room name changed --> %s" (oref room :room-name))
                                   'timestamp timestamp
                                   'event_id event_id
                                   'face 'matrix-client-metadata)))
     (when (>= timestamp (oref* room :con :connect-ts))
       (matrix-client-insert room message)))))

(defmatrix-client-handler "m.room.aliases"
  ((new-alias-list (a-get* data 'content 'aliases)))
  ((let* ((timestamp (matrix-client-event-data-timestamp data))
          (message (propertize (format "Room alias changed --> %s" new-alias-list)
                               'timestamp timestamp
                               'face 'matrix-client-metadata)))
     (when (>= timestamp (oref* room :con :connect-ts))
       (matrix-client-insert room message)))
   (oset room :aliases new-alias-list)
   (matrix-client-update-name room)
   (matrix-client-update-header-line room)))

(defmatrix-client-handler "m.room.topic"
  ((topic (a-get* data 'content 'topic)))
  ((let* ((timestamp (matrix-client-event-data-timestamp data))
          (message (propertize (format "Room topic changed --> %s" topic)
                               'timestamp timestamp
                               'face 'matrix-client-metadata)))
     (when (>= timestamp (oref* room :con :connect-ts))
       (matrix-client-insert room message)))
   (oset room :topic topic)
   (matrix-client-update-header-line room)))

(defun matrix-client-handler-m.typing (con room data)
  (with-slots (buffer typers) room
    (with-current-buffer buffer
      (setq typers (a-get* data 'content 'user_ids))
      (matrix-client-update-header-line room))))

(defun matrix-client-displayname-from-user-id (room user-id)
  "Return display name for USER-ID in ROOM."
  (let* ((membership (oref room :membership))
         (user (a-get membership user-id))
         (displayname (a-get user 'displayname)))
    (or displayname
        user-id)))

(defun matrix-client-input-filter-join (connection text)
  "Filter /join from input TEXT on CONNECTION.
Return nil if room is joined, otherwise TEXT."
  (if (string-match "^/j\\(oin\\)? +\\(.*\\)" text)
      (progn
        (let ((room (substring text (match-beginning 2) (match-end 2))))
          (matrix-join-room connection room)
          (matrix-client-setup-room connection room))
        nil)
    text))

(defun matrix-client-input-filter-leave (_ text)
  "Filter /leave from input TEXT.
Return nil if room is left, otherwise TEXT."
  (if (string-match (rx bos "/leave" (or space eos)) text)
      (pcase-let (((eieio id con) matrix-client-room-object))
        (when (matrix-leave-room con id)
          (kill-buffer)
          nil))
    text))

(defun matrix-client-input-filter-emote (_ text)
  "Filter emotes from TEXT.
Return nil if emote sent, otherwise TEXT."
  (if (string-match "^/me +\\(.*\\)" text)
      (pcase-let ((emote (substring text (match-beginning 1) (match-end 1)))
                  ((eieio id con) matrix-client-room-object))
        (when (and con id)
          (matrix-send-event con id "m.room.message"
                             (a-list "msgtype" "m.emote"
                                     "body" emote))
          nil))
    text))

(defun matrix-client-input-filter-who (_ text)
  "Show room members in buffer when user types \"/who\".
If user typed /who, return nil, otherwise TEXT for further
filtering."
  (if (string-match (rx bos "/who" (or (1+ space) eos)) text)
      (let ((room matrix-client-room-object))
        (with-slots (membership) room
          (matrix-client-insert room (propertize (format "Room members: %s" (--> membership
                                                                                 (--map (a-get (cdr it) 'displayname) it)
                                                                                 (--sort (string-collate-lessp it other nil 'ignore-case)
                                                                                         it)
                                                                                 (s-join ", " it)))
                                                 'timestamp (time-to-seconds)
                                                 'face 'matrix-client-notice))
          (matrix-client-update-last-seen room)))
    text))

(provide 'matrix-client-handlers)
;;; matrix-client-handlers.el ends here
