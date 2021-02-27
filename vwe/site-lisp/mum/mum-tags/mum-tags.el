;;; mum-tags.el ---  Mum tags            -*- lexical-binding: t; -*-

;; Copyright (C) 2021  WuJunyu

;; Author: WuJunyu <vistar_w@hotmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(defgroup mum-tags nil
  "Customization group for beacon."
  :group 'vwiss-mum
  :prefix "mum-tags--")

(defvar mum-tags--keymap
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Move to mark map.")

;;
;; mode
;;
(defun mum-tags-mode-enable ()
  "Enable mode.")

(defun mum-tags-mode-disable ()
  "Disable mode.")

;;;###autoload
(define-minor-mode mum-tags-mode
  "Mum tags minor mode."
  :group 'mum-tags
  :keymap mum-tags--mode-keymap
  :global t
  (if mum-tags-mode
	  (mum-tags-mode-enable)
	(mum-tags-mode-disable)))

(provide 'mum-tags)
;;; mum-tags.el ends here
