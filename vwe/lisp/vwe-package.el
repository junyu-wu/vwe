;;; vwe-package.el --- Package Management  -*- lexical-binding: t; -*-

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
(defconst vwe@custom--source-list
  '(("melpa" .       (("melpa". "https://melpa.org/packages/")
					  ("gnu" . "https://elpa.gnu.org/packages/")
					  ("org"  . "http://orgmode.org/elpa/")))
	("china-melpa" . (("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
					  ("org-cn"   . "http://elpa.emacs-china.org/org/")
					  ("gnu-cn"   . "http://elpa.emacs-china.org/gnu/"))))
  "Source Options.")

(defun vwe@pkg--source-toggle (source-name)
  "Toggle Emacs package source.
SOURCE-NAME is source name."
  (interactive
   (list
    (completing-read "source:"
					 (mapcar (lambda(item)
							   (car item)) vwe@custom--source-list))))
  (let* ((source (cdr (assoc source-name vwe@custom--source-list))))
	(setq package-archives source)))

(defun vwe@pkg--init ()
  "Package init."
  (interactive)
  (vwe@pkg--source-toggle vwe@custom--source)
  (setq package-enable-at-startup nil)
  (package-initialize)
  (vwe@lib--package-load 'cl)
  (vwe@lib--package-load 'cl-lib)
  ;; (vwe@lib--package-load 'seq)
  (vwe@lib--package-load 'use-package)
  (vwe@lib--package-load 'popup))

;; ***************************************************************************
;; config
;; ***************************************************************************
(vwe@pkg--init)

(eval-when-compile
  (setq use-package-always-ensure t
		use-package-always-defer t
		use-package-expand-minimally t
		use-package-enable-imenu-support t))

(use-package auto-package-update
  :init (setq auto-package-update-last-update-day-path
			  (vwe@lib--path-cache
			   "/package/.last-package-update-day" t)))

(provide 'vwe-package)
;;; vwe-package.el ends here
