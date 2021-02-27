;;; vwe-tags.el ---  Vwe tags            -*- lexical-binding: t; -*-

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
(defgroup vwe-tags nil
  "Customization group for beacon."
  :group 'vwiss-vwe
  :prefix "vwe-tags--")

(defvar vwe-tags--keymap
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Move to mark map.")

;;
;; mode
;;
(defun vwe-tags-mode-enable ()
  "Enable mode.")

(defun vwe-tags-mode-disable ()
  "Disable mode.")

;;;###autoload
(define-minor-mode vwe-tags-mode
  "Vwe tags minor mode."
  :group 'vwe-tags
  :keymap vwe-tags--mode-keymap
  :global t
  (if vwe-tags-mode
	  (vwe-tags-mode-enable)
	(vwe-tags-mode-disable)))

(provide 'vwe-tags)
;;; vwe-tags.el ends here
