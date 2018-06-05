;;; matrix-helpers.el --- Helpers for matrix-client

;; Copyright (C) 2017-2018 Jay Kamat
;; Copyright (C) 2015-2016 Ryan Rix
;; Author: Ryan Rix <ryan@whatthefuck.computer>
;; Maintainer: Jay Kamat <jaygkamat@gmail.com>
;; Created: 4 March 2016
;; Keywords: web, comm
;; Homepage: https://github.com/jgkamat/matrix-client-el
;; Package-Version: 0.3.0
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;; matrix-helpers.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.
;;
;; matrix-helpers.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Helpers for [`matrix-api'].

;;; Code:

;;;; Macros

(defmacro oref* (&rest slots)
  "Access SLOTS of nested EIEIO objects.
The first of SLOTS should be an object, while the rest should be
slot symbols.  Accessing each slot should return an object for
which the next slot is valid, except for the last slot, which may
return any value."
  (cl-labels ((rec (slots)
                   `(oref ,(if (and (consp (cdr slots))
                                    (cddr slots))
                               (rec (cdr slots))
                             (cadr slots))
                          ,(car slots))))
    (rec (nreverse slots))))

(defmacro oset-multi (object &rest pairs)
  "Set slot values for OBJECT.
PAIRS should be of the form (SLOT VALUE SLOT VALUE...)."
  (declare (indent defun))
  `(progn
     ,@(cl-loop for (slot value) on pairs by #'cddr
                collect (list 'oset object slot value))))

(defmacro a-get* (&rest keys)
  ;; See https://github.com/plexus/a.el/issues/7
  (cl-labels ((rec (keys)
                   `(a-get ,(if (and (consp (cdr keys))
                                     (cddr keys))
                                (rec (cdr keys))
                              (cadr keys))
                           ,(car keys))))
    (rec (nreverse keys))))

;;;; Functions

(defun matrix-client-buffer-list-update-hook ()
  "Set buffer's modified status and move last-seen overlay when focused."
  ;; NOTE: Since this hook is added to the `buffer-list-update-hook', it
  ;; is ABSOLUTELY NECESSARY that this function be COMPLETELY bug
  ;; free.  If there is any bug, it can make it virtually impossible
  ;; to use Emacs, because nearly any command (especially if using
  ;; something like Helm) calls this hook, and the bug causes an error
  ;; before the command the user is running actually does anything.
  ;; Practically the only solution is to kill and restart Emacs (even
  ;; C-x C-c doesn't work).

  ;; FIXME: Using `when-let' to test for `matrix-client-room-object'
  ;; should be safe, but given the risk of using
  ;; `buffer-list-update-hook', and the fact that it's called in so
  ;; many places in Emacs, it would be better to do this a different
  ;; way altogether.  Maybe we could use `window-configuration-change-hook'.
  (when-let ((room matrix-client-room-object)
             (buffer (oref room buffer))
             (window (get-buffer-window buffer))
             (window-active-p (equal window (selected-window)))
             (not-one-window-p (not (= 1 (length (window-list))))))
    (when (matrix-client-buffer-visible-p)
      ;; FIXME: Need a way to move the seen line when there's one
      ;; window visible and the user has seen it.  Unfortunately,
      ;; there seems to be no way to detect whether the Emacs frame
      ;; ("window" in X) has focus, other than using focus hooks and
      ;; tracking state ourselves, which seems messy.  This is good
      ;; enough for now.
      (set-buffer-modified-p nil)
      (matrix-client-update-last-seen room))))

(defun matrix-client-insert-last-seen-overlay ()
  "Insert last-seen overlay into current buffer."
  (when-let ((prompt-ov (car (ov-in 'matrix-client-prompt)))
             (target-pos (1- (ov-beg prompt-ov))))
    (ov target-pos target-pos
        'before-string (concat "\n" (propertize "\n\n" 'face 'matrix-client-last-seen))
        'matrix-client-last-seen t)))

(defun matrix-client-buffer-visible-p (&optional buffer)
  "Return non-nil if BUFFER is currently visible.
If BUFFER is nil, use the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (or (eq buffer (window-buffer (selected-window)))
        (get-buffer-window buffer))))

(defun matrix-client-delete-backward-char (n &optional kill-flag)
  "Delete backward unless the point is at the prompt or other read-only text."
  (interactive "p\nP")
  (unless (get-text-property (- (point) 2) 'read-only)
    (call-interactively #'delete-backward-char n kill-flag)))

(defun matrix-homeserver-api-url (&optional version)
  "Message `matrix-homeserver-base-url' in to a fully-qualified API endpoint URL."
  (let ((version (or version "api/v1")))
    (format "%s/_matrix/client/%s" matrix-homeserver-base-url version)))

(defun matrix-get (key obj)
  "Easy JSON accessor, get KEY's value from OBJ."
  (cdr (assoc key obj)))

(defun matrix-transform-mxc-uri (uri)
  "Turn an MXC content URI in to an HTTP URL."
  (let ((components (split-string uri "/")))
    (format "%s/_matrix/media/v1/download/%s/%s"
            matrix-homeserver-base-url
            (elt components 2)
            (elt components 3))))

(defun matrix-client-room-for-id (connection room-id)
  "Return room for ROOM-ID on CONNECTION."
  (a-get (oref connection :rooms) room-id))

(defun matrix-parse-curl-exit-code (error-string)
  "Return exit code from ERROR-STRING as a number, or nil if none found."
  (cond
   ;; If we get a list with a number, use that '(401)
   ((and (listp error-string)
         (> (length error-string) 0)
         (numberp (first error-string)))
    (first error-string))
   ((stringp error-string)
    (when (string-match "exited abnormally with code \\([[:digit:]]+\\).*" error-string)
      (ignore-errors
        ;; Ignore errors to avoid any problems that might be caused by
        ;; the error string not matching.  I don't think this is
        ;; strictly necessary, but the old code caught errors, so just
        ;; in case...
        (string-to-number (match-string-no-properties 1 error-string)))))
   (t
    nil)))

(provide 'matrix-helpers)
;;; matrix-helpers.el ends here
