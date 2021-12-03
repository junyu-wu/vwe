;;; vwe-auto.el ---                                  -*- lexical-binding: t; -*-

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

(defgroup vwe-auto nil
  "Edit."
  :group 'vwiss-vwe
  :prefix "vwe-auto")

(defcustom vwe-auto--insert-alist
  nil
  "Auto insert alist."
  :type 'list)

(defvar vwe-auto-insert--keymap
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Insert map.")

(defun vwe-auto--insert ()
  "Insert."
  (interactive)
  )

(defun vwe-auto-insert-mode-enable ()
  "Enable mode."
  )

(defun vwe-auto-insert-mode-disable ()
  "Disable mode."
  )

;;;###autoload
(define-minor-mode vwe-auto-insert-mode
  "Auto insert minor mode."
  :group 'vwe-auto
  :keymap nil
  (if vwe-auto-insert-mode
	  (vwe-auto-insert-mode-enable)
	(vwe-auto-insert-mode-disable)))

(provide 'vwe-auto)
;;; vwe-auto.el ends here
