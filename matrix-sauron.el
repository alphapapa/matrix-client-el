;;; matrix-sauron.el --- Sauron integration for the Matrix Client

;; Copyright (C) 2017-2018 Jay Kamat
;; Copyright (C) 2015 Ryan Rix
;; Author: Ryan Rix <ryan@whatthefuck.computer>
;; Maintainer: Jay Kamat <jaygkamat@gmail.com>
;; Created: 21 June 2015
;; Keywords: web, comm
;; Homepage: https://github.com/jgkamat/matrix-client-el
;; Package-Version: 0.1.0

;; This file is not part of GNU Emacs.

;; matrix-sauron.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.
;;
;; matrix-sauron.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library pushes Matrix events in to Sauron, allowing you to easily see a
;; list of notifications and jump to them. See https://github.com/djcb/sauron
;; for information about Sauron.

;; To use this, install Sauron via package.el or manually, and then load or eval
;; this library. Add 'sauron-matrix to sauron-modules and run [`sauron-start']

;;; Code:

(require 'sauron nil 'noerror)
(require 'cl-lib)

(defvar sauron-prio-matrix-new-messages 2
  "Priority for incoming Matrix events.")

(defvar sauron-matrix-running nil
  "When non-nil, matrix-sauron is running.")

(defvar matrix-sauron-pushrules nil
  "Matrix homeserver push rules")

(defun matrix-sauron-start ()
  "Start matrix-sauron."
  (if (and (boundp 'matrix-homeserver-base-url)
           matrix-homeserver-base-url)
      (progn
        (when sauron-matrix-running
          (error "matrix-sauron is already running. Call sauron-matrix-stop first."))
        (add-to-list 'matrix-client-after-connect-hooks
                     #'matrix-sauron-after-connect-handler)
        (setq sauron-matrix-running t))
    (message "matrix-client not loadable, so matrix-sauron could not start.")))

(defun matrix-sauron-stop ()
  "Stops and cleans up matrix-sauron."
  (when sauron-matrix-running
    (setq matrix-client-after-connect-hooks
          (remove #'matrix-sauron-after-connect-handler
                  matrix-client-after-connect-hooks))
    (mapc (lambda (con)
            (let* ((real-con (cadr con))
                   (hooks (and (slot-boundp real-con :event-hook)
                               (oref real-con :event-hook))))
              (oset real-con :event-hook (delq #'matrix-add-sauron-event hooks))))
          matrix-client-connections)
    (setq sauron-matrix-running nil)))

(defun matrix-sauron-after-connect-handler (con)
  (let ((hooks (and (slot-boundp con :event-hook)
                    (oref con :event-hook))))
    (matrix-sauron-fetch-rules con)
    (oset con :event-hook
          (append hooks '(matrix-add-sauron-event)))))

(defmethod matrix-sauron-fetch-rules  ((con matrix-client-connection))
  "Fetch and parse the push rules from the server."
  (let* ((resp (matrix-send con "GET" "/pushrules/")))
    (add-to-list 'matrix-sauron-pushrules (list (oref con :username)
                                                (matrix-get 'global resp)))))
(defun matrix-sfind (str array)
  (condition-case nil
      (cl-find str array :test 'string-equal)
    (error nil)))

(defun matrix-sauron-process-event-with-pushrules (me prio room-id membership content username)
  ;; Process each pushrule type-break
  (let* ((rules (car (matrix-get me matrix-sauron-pushrules)))
         (final-prio
          (+ prio
             ;; Underride
             (cl-reduce
              (lambda (rest next)
                (+ rest (or next 0)))
              (map 'list
                   (lambda (rule)
                     (if (matrix-get 'enabled rule)
                         (let* ((actions (matrix-get 'actions rule))
                                (rule-id (matrix-get 'rule_id rule))
                                (conditions (matrix-get 'conditions rule))
                                (score-mod (if (matrix-sfind "dont_notify" actions)
                                               -5 1)))
                           ;; Add match conditiopn here.
                           (cl-reduce (lambda (last cond)
                                        (let ((kind (matrix-get 'kind cond)))
                                          (cond ((equal kind "room_member_count")
                                                 (let ((is (matrix-get 'is cond)))
                                                   (if (eq membership
                                                           (string-to-number is))
                                                       (+ last score-mod))))
                                                ;; put shit here.
                                                (t last))))
                                      conditions :initial-value 0))
                       0))
                   (matrix-get 'underride rules)))
             ;; Rooms
             (cl-reduce
              #'+
              (map 'list
                   (lambda (rule)
                     (when (matrix-get 'enabled rule)
                       (let* ((actions (matrix-get 'actions rule))
                              (rule-id (matrix-get 'rule_id rule))
                              (score-mod (if (matrix-sfind "dont_notify" actions)
                                             -5 1)))
                         (if (string-equal room-id rule-id)
                             (progn
                               score-mod)
                           0))))
                   (matrix-get 'room rules)))
             ;; Content
             (cl-reduce
              #'+
              (map 'list
                   (lambda (rule)
                     (when (matrix-get 'enabled rule)
                       (let* ((actions (matrix-get 'actions rule))
                              (rule-id (matrix-get 'rule_id rule))
                              (body (matrix-get 'body content))
                              (pattern (matrix-get 'pattern rule))
                              (score-mod (if (matrix-sfind "dont_notify" actions)
                                             -5 1)))
                         (if (and body (string-match (format "\\b%s\\b" pattern) body))
                             score-mod
                           0))))
                   (matrix-get 'content rules)))
             ;; Sender
             ;; Override
             )))
    (cond ((< final-prio 0) (progn 0))
          ((< 5 final-prio) (progn 5))
          (t (progn final-prio)))))

;; XXX: Figure out how to plumb from chunk to active connection.
;; Does it make sense to annotate the callback with the connection it came from? Is it actually even
;; feasible to support more than one simultaneous connection?
(defmethod matrix-add-sauron-event ((con matrix-client-connection) room event)
  (let* ((room-id (oref room :id))
         (room-obj room)
         (room-buf (oref room :buffer))
         (membership (if (slot-boundp room :membership)
                         (length (oref room :membership))
                       0))
         (type (matrix-get 'type event))
         (content (matrix-get 'content event))
         (username (matrix-get 'sender event))
         (prio sauron-prio-matrix-new-messages)
         (my-username (oref con :username))
         (prio (matrix-sauron-process-event-with-pushrules my-username prio room-id membership content username))
         (target (if (buffer-live-p room-buf)
                     (save-excursion
                       (with-current-buffer room-buf
                         (goto-char (point-max))
                         (forward-line -1)
                         (point-marker))))))
    (when (not (and username (string-match my-username username)))
      (sauron-add-event 'matrix prio
                        (format "<%s> %s" username (matrix-get 'body content))
                        (lexical-let* ((target-mark target)
                                       (target-buf room-buf))
                          (lambda ()
                            (sauron-switch-to-marker-or-buffer (or target-mark target-buf))))))))

(provide 'matrix-sauron)

;;; matrix-sauron.el ends here
