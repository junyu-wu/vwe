;;; vwe-modeline.el --- Mode Line UI              -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Wu Junyu

;; Author: Wu Junyu <vistar_w@hotmail.com>
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

;;; Code:
;; ***************************************************************************
;; lib
;; ***************************************************************************
(defun vwe@modeline--hide ()
  "Hide buffer mode line."
  (if vwe@custom--modeline--hide-list
	  (vwe@lib--modeline-hide vwe@custom--modeline--hide-list)
	(vwe@lib--modeline-hide)))

(defun vwe@modeline--init ()
  "Modeline init."
  (add-hook 'emacs-startup-hook 'vwe@modeline--hide)
  (add-hook 'window-configuration-change-hook 'vwe@modeline--hide))

;; ***************************************************************************
;; config
;; ***************************************************************************

(use-package mum-modeline
 :load-path
 (lambda ()
   (vwe@lib--path-vwe-site-lisp "mum/mum-modeline"))
 :hook
 (after-init . mum-modeline-mode))

(vwe@modeline--init)

(provide 'vwe-modeline)
;;; vwe-modeline.el ends here
