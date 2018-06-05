;;; matrix-utils.el --- General utitilies for `matrix-client'

;; Copyright (C) 2017-2018 Adam Porter
;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Jay Kamat <jaygkamat@gmail.com>
;; Created: 14 November 2017
;; Keywords: web, comm
;; Homepage: https://github.com/jgkamat/matrix-client-el
;; Package-Version: 0.3.0
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;; matrix-utils.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.
;;
;; matrix-utils.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A mixed collection of utilities across the `matrix-client' suite.

;;; Code:

(defmacro matrix--map-merge (map &rest pairs)
  "Merge non-nil PAIRS into MAP.
PAIRS is a plist.  Any pair whose value is `nil' is ignored."
  (declare (indent defun))
  `(cl-loop for (key . value) in ,pairs by #'cddr
            when value
            do (map-put ,map key value)
            finally return ,map))

(defun matrix--alist (&rest pairs)
  "Return an alist of the key-value pairs in PAIRS whose value is non-nil.
PAIRS is a spliced plist."
  ;; e.g. (matrix--alist "direction" "b" "limit" nil) => (("direction" . "b"))
  (cl-loop for (key  value) on pairs by #'cddr
           when value
           collect (cons key value)))

(defun matrix--prev-property-change (pos property)
  "Return the previous position in buffer, starting from POS, where PROPERTY changes and is set.
Positions where PROPERTY is not set are ignored."
  (cl-loop do (setq pos (previous-single-property-change pos property))
           while pos
           until (get-text-property pos property)
           finally return pos))

(defun matrix--next-property-change (pos property &optional limit)
  "Return the next position in buffer, starting from POS, where PROPERTY changes and is set.
Positions where PROPERTY is not set are ignored.  If LIMIT is
non-nil, don't search past that position."
  (cl-loop do (setq pos (next-single-property-change pos property nil limit))
           while (and pos
                      (or (not limit)
                          (< pos limit)))
           until (get-text-property pos property)
           finally return pos))

(provide 'matrix-utils)

;;; matrix-utils.el ends here
