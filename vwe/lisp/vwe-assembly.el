;;; vwe-assembly.el --- Assembly Programming -*- lexical-binding: t; -*-

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
;; apt install nasm
;; apt install poppler-utils
;; https://software.intel.com/en-us/articles/intel-sdm

;;; Code:
;; ***************************************************************************
;; lib
;; ***************************************************************************
(defun vwe@asm--find-labels()
  "Find lables."
  (interactive)
  (save-excursion
	(save-restriction
	  (let* ((exist-label-list '())
			 (lstr))
		(goto-char (point-min))
		(while (re-search-forward "[0-9 a-z A-Z \. _]+:$" nil t)
		  (c-remove-font-lock-face
		   (car (bounds-of-thing-at-point 'symbol))
		   (cdr (bounds-of-thing-at-point 'symbol)))
		  (setq lstr (substring (thing-at-point 'symbol)
								0
								(- (length (thing-at-point 'symbol)) 1)))
		  (setq exist-label-list (cons lstr exist-label-list)))
		(regexp-opt exist-label-list 'symbols)))))

(defconst vwe@asm--company-completions
  nil
  "Assembly company completions.")

(defun vwe@asm--company-create-completions ()
  "Create Completions."
  (interactive)
  (vwe@lib--package-load 'x86-lookup)
  (let* ((keyword)
		 (keyword-list '())
		 (completions '()))
	(cl-loop for i in (x86-lookup-ensure-index)
			 collect
			 (progn
			   (setq keyword (car i))
			   (setq keyword-list (append keyword-list
										  (list keyword)))
			   (setq completions
					 (append completions
							 (list (propertize keyword
											   ':initials keyword
											   ':summary keyword))))))
	(list completions)))

(defun vwe@asm--annotation (annotation)
  "Format annotation.
ANNOTATION: annotation."
  (format " [%s]" (get-text-property 0 :initials annotation)))

(defun vwe@asm--meta (meta)
  "Format meta.
META: meta."
  (get-text-property 0 :summary meta))

(defun vwe@asm--fuzzy-match (prefix candidate)
  "Fuzzy match key word.
PREFIX: preifx.
CANDIDATE: candidate."
  (cl-subsetp (string-to-list prefix)
			  (string-to-list candidate)))

(defun vwe@asm--company-backend (command &optional arg &rest ignored)
  "Assmbily company backend.
COMMAND: command.
ARG: arg.
IGNORED: ignored."
  (interactive (list 'interactive))
  (case command
	(interactive (company-begin-backend 'vwe@asm--company-backend))
	(prefix (and (or (eq major-mode 'asm-mode) (eq major-mode 'nasm-mode))
				 (company-grab-symbol)))
	(candidates
	 (remove-if-not
	  (lambda (c) (vwe@asm--fuzzy-match arg c))
	  (car vwe@asm--company-completions)))
	(annotation (vwe@asm--annotation arg))
	(meta (vwe@asm--meta arg))
	(no-cache 't)))

;; ***************************************************************************
;; config
;; ***************************************************************************
(use-package nasm-mode
  :load-path
  (lambda ()
	(vwe@lib--path-vwe-site-lisp "nasm-mode"))
  :mode
  ("\\.\\(asm\\|s\\|lst\\)$")
  :hook
  ((before-save . delete-trailing-whitespace)
   (nasm-mode . (lambda()
				  (font-lock-add-keywords
				   nil
				   '(("section\\.[0-9 a-z A-Z \. _ ]+"
					  .
					  'nasm-section-name))))))
  :config
  (use-package company-asm
	:load-path
	(lambda ()
	  (vwe@lib--path-vwe-site-lisp "company"))
	:init
	(setq vwe@asm--company-completions (vwe@asm--company-create-completions))
	:config
  	(add-to-list 'company-backends 'vwe@asm--company-backend)
	(add-to-list 'company-backends 'vwe@asm--company-create-completions)))

(provide 'vwe-assembly)
;;; vwe-assembly.el ends here
