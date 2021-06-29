;;; vwe-project.el --- vwe project                   -*- lexical-binding: t; -*-

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
(defgroup vwe-project nil
  "Porject."
  :group 'vwiss-vwe
  :prefix "vwe-project--")

(defvar vwe-project--keymap
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Move to mark map.")

;;
;; mode
;;
(defun vwe-project-mode-enable ()
  "Enable mode.")

(defun vwe-project-mode-disable ()
  "Disable mode.")

;;;###autoload
(define-minor-mode vwe-project-mode
  "Vwe tags minor mode."
  :group 'vwe-project
  :keymap vwe-project--keymap
  :global t
  (if vwe-project-mode
	  (vwe-project-mode-enable)
	(vwe-project-mode-disable)))

(provide 'vwe-project)
;;; vwe-project.el ends here
