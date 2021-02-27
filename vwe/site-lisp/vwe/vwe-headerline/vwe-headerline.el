;;; vwe-headerline.el --- Header line mode      -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Wu Junyu

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

;;

;;; Code:

(defgroup vwe-headerline nil
  "Vwe mode line."
  :prefix "vwe-headerline--"
  :group 'header-line)

(defcustom vwe-headerline--buffer-filter-list
  nil
  "Buffer filter list."
  :group 'vwe-headerline
  :type 'string)

(defface vwe-headerline--info-face
  '((t (:background "DarkGreen" :foreground "white" :weight bold)))
  "Info face.")

(defface vwe-headerline--warning-face
  '((t (:background "DarkOrange" :foreground "white" :weight bold)))
  "Warning face.")

(defface vwe-headerline--error-face
  '((t (:background "DarkRed" :foreground "white" :weight bold)))
  "Error face.")

(defface vwe-headerline--file-face
  '((t (:foreground "white" :weight bold :inverse-video nil)))
  "File face.")

(defface vwe-headerline--path-face
  '((t (:foreground "#B0BEC5" :weight bold)))
  "Path face.")

(defconst vwe-headerline--buffer-status-list
  '("RW" "MD" "RO")
  "Buffer status list.")

(defun vwe-headerline--sys-separator ()
  "Get system separator."
  (interactive)
  (let* ((split))
	;; (if (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
	;; 	(setq split "\\")
	(setq split "/");;)
	split))

(defun vwe-headerline--path-trim (path)
  "Trim PATH."
  (when (equal (substring path 0 1) (vwe-headerline--sys-separator))
	(setq path (substring path 1)))
  (when (equal (substring path (1- (length path))) (vwe-headerline--sys-separator))
	(setq path (substring path 0 (1- (length path)))))
  path)

(defun vwe-headerline--buffer-status ()
  "Buffer status."
  (let* ((modified  (and buffer-file-name (buffer-modified-p)))
		 (read-only buffer-read-only))
	(cond
	 (modified (propertize (format "[%s]" (cadr vwe-headerline--buffer-status-list))
						   'face 'vwe-headerline--error-face))
	 (read-only (propertize (format "[%s]" (caddr vwe-headerline--buffer-status-list))
							'face 'vwe-headerline--warning-face))
	 (t (propertize (format "[%s]" (car vwe-headerline--buffer-status-list))
					'face 'vwe-headerline--info-face)))))

(defun vwe-headerline--make-buffer-path ()
  "Make buffer path header-line."
  (let* ((file-name (buffer-file-name))
		 (path (if file-name file-name (buffer-name)))
		 (path-length (length path))
		 (dir (if file-name (file-name-directory path) ""))
		 (file (if file-name (file-name-nondirectory path) path))
		 (placeholder 35)
		 (hl-length (+ path-length placeholder 2))
		 (drop-str "[...]")
		 (width (window-body-width)))
	(when (< width hl-length)
	  (let* ((dir-list (split-string (vwe-headerline--path-trim dir) (vwe-headerline--sys-separator)))
			 (dir-list-length (length dir-list)))
		(setq dir (concat (vwe-headerline--sys-separator)
						  (car dir-list)
						  (vwe-headerline--sys-separator)
						  drop-str
						  (vwe-headerline--sys-separator)
						  (car (reverse dir-list))
						  (vwe-headerline--sys-separator)))))
	(setq path (concat (propertize dir
								   'face 'vwe-headerline--path-face)
					   (propertize file
								   'face 'vwe-headerline--file-face)))
	path))

(defun vwe-headerline--make-buffer-branch ()
  "Make buffer branch header-line."
  (propertize (if vc-mode
				  (let* ((backend (vc-backend buffer-file-name)))
					(substring-no-properties vc-mode
											 (+ (if (eq backend 'Hg) 2 3) 2)))
				"*non-vcs*")
			  'face 'vwe-headerline--path-face))

(defun vwe-headerline--make-buffer-major ()
  "Make buffer major mode header-line."
  (propertize (format "%s" (cdr (assoc 'major-mode (buffer-local-variables (current-buffer)))))
			  'face 'vwe-headerline--path-face))

(defun vwe-headerline--make-buffer-info ()
  "Make buffer info header-line."
  (list (vwe-headerline--buffer-status)
		(vwe-headerline--make-buffer-path)
		" "
		(vwe-headerline--make-buffer-branch)
		" "
		(vwe-headerline--make-buffer-major)))

(defun vwe-headerline--active ()
  "Header line active."
  (interactive)
  (add-hook 'focus-in-hook #'vwe-headerline--show-p)
  (add-hook 'window-configuration-change-hook #'vwe-headerline--show-p)
  (unless (vwe@lib--buffer-match-p vwe-headerline--buffer-filter-list)
	(setq-default header-line-format (vwe-headerline--make-buffer-info))))

(defun vwe-headerline--deactive ()
  "Header line deactive."
  (interactive)
  (remove-hook 'focus-in-hook #'vwe-headerline--show-p)
  (remove-hook 'window-configuration-change-hook #'vwe-headerline--show-p)
  (setq-default header-line-format nil))

(defun vwe-headerline--buffer-match-p (regs &optional buffer)
  "Is BUFFER name match in REGS?"
  (unless buffer (setq buffer (buffer-name)))
  (when regs
	(cond
	 ((stringp regs) (string-match regs buffer))
	 ((listp regs) (progn
					 (catch 'break
					   (dotimes (i (length regs))
						 (when (string-match (nth i regs) buffer)
						   (throw 'break t)))))))))

(defun vwe-headerline--show-p ()
  "Header line is show."
  (when vwe-headerline-mode
	(unless (vwe-headerline--buffer-match-p vwe-headerline--buffer-filter-list)
	  (setq header-line-format (vwe-headerline--make-buffer-info)))))

(define-minor-mode vwe-headerline-mode
  "Vwe headreline minor mode."
  :init-value nil
  :keymap nil
  :group 'vwe-headerline
  :global t
  (if vwe-headerline-mode (vwe-headerline--active) (vwe-headerline--deactive)))

(provide 'vwe-headerline)
;;; vwe-headerline.el ends here
