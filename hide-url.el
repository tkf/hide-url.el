;;; hide-url.el --- Hide annoyingly long URLs
;;
;; Filename: hide-url.el
;; Description: Hide annoyingly long URLs
;; Author: ARAKAKI, Takafumi
;; Maintainer: ARAKAKI, Takafumi
;; Created: Fri Dec 16 20:47:22 2011 (+0100)
;; URL:
;;
;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;;

;;; License:
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Code:

(defvar hide-url/offset-beg 10)
(defvar hide-url/offset-end 10)

(defun hide-url/hide-all ()
  "Hide all URLs."
  (interactive)
  (with-silent-modifications
    (save-excursion
      (beginning-of-buffer)
      (while (search-forward-regexp thing-at-point-url-regexp nil t)
        (hide-url/hide-at-point)))))

(defun hide-url/show-all ()
  "Show all hidden URLs."
  (interactive)
  (with-silent-modifications
    (let ((beg) (end (point-min)))
      (while end
        (setq beg (next-single-property-change end 'hide-url/hidden))
        (setq end (when beg
                    (next-single-property-change beg 'hide-url/hidden)))
        (when end
          (hide-url/remove-text-properties beg end))))))

(defun hide-url/hide-at-point ()
  "Replace URL at point with something like 'https://gi....ide-url.el'."
  (let ((bounds (thing-at-point-bounds-of-url-at-point)))
    (when bounds
      (let ((hide-beg (+ (car bounds) hide-url/offset-beg))
            (hide-end (- (cdr bounds) hide-url/offset-end)))
        (when (< hide-beg hide-end)
          (hide-url/put-text-properties hide-beg hide-end))))))

(defun hide-url/put-text-properties (beg end)
  "Hide a region between BEG and END."
  (add-text-properties
   beg end
   '(display
     "..."
     hide-url/hidden
     t)))

(defun hide-url/remove-text-properties (beg end)
  "Show a hidden region between BEG and END."
  (remove-list-of-text-properties
   beg end
   '(display hide-url/hidden)))

;;; hide-url.el ends here
