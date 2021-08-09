;;; vwe-command.el ---                        -*- lexical-binding: t; -*-

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

;;
;; M-x
;;
(defgroup vwe-m-x nil
  "Command."
  :group 'vwe-m-x)

(defcustom vwe-m-x--save-length
  10
  "Save length."
  :type 'number
  :group 'vwe-m-x)

(defvar vwe-m-x--keymap
  (let* ((keymap (make-sparse-keymap)))
	keymap)
  "Command keymap.")

(defun vwe-m-x--recent ()
  "Recent command.")

;;
;; mode
;;
(defun vwe-m-x--enable ()
  "Enable mode."
  (vwe-m-x-mode t))

(defun vwe-m-x--disable ()
  "Disable mode."
  (vwe-m-x-mode -1))

;;;###autoload
(define-minor-mode vwe-m-x-mode
  "Vwe command mode."
  :global t
  (if vwe-m-x-mode
	  (vwe-m-x--enable)
    (vwe-m-x--disable)))

(provide 'vwe-command)
;;; vwe-command.el ends here
