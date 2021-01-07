;;; mum-headerline.el --- Header line mode      -*- lexical-binding: t; -*-

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

(defgroup mum-headerline nil
  "Mum mode line."
  :prefix "mum-headerline--"
  :group 'header-line)

(defcustom mum-headerline--buffer-filter-list
  nil
  "Buffer filter list."
  :group 'mum-headerline
  :type 'string)

(defface mum-headerline--info-face
  '((t (:background "DarkGreen" :foreground "white" :weight bold)))
  "Info face.")

(defface mum-headerline--warning-face
  '((t (:background "DarkOrange" :foreground "white" :weight bold)))
  "Warning face.")

(defface mum-headerline--error-face
  '((t (:background "DarkRed" :foreground "white" :weight bold)))
  "Error face.")

(defface mum-headerline--file-face
  '((t (:background "#434C5E" :foreground "white" :weight bold)))
  "File face.")

(defface mum-headerline--path-face
  '((t (:background "#434C5E" :foreground "#B0BEC5" :weight bold)))
  "Path face.")

(defconst mum-headerline--buffer-status-list
  '("RW" "MD" "RO")
  "Buffer status list.")

(defun mum-headerline--sys-separator ()
  "Get system separator."
  (interactive)
  (let* ((split))
	(if (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
		(setq split "\\")
	  (setq split "/"))
	split))

(defun mum-headerline--path-trim (path)
  "Trim PATH."
  (when (equal (substring path 0 1) (mum-headerline--sys-separator))
	(setq path (substring path 1)))
  (when (equal (substring path (1- (length path))) (mum-headerline--sys-separator))
	(setq path (substring path 0 (1- (length path)))))
  path)

(defun mum-headerline--buffer-status ()
  "Buffer status."
  (let* ((modified  (and buffer-file-name (buffer-modified-p)))
		 (read-only buffer-read-only))
	(cond
	 (modified (propertize (format "[%s]" (cadr mum-headerline--buffer-status-list))
						   'face 'mum-headerline--error-face))
	 (read-only (propertize (format "[%s]" (caddr mum-headerline--buffer-status-list))
							'face 'mum-headerline--warning-face))
	 (t (propertize (format "[%s]" (car mum-headerline--buffer-status-list))
					'face 'mum-headerline--info-face)))))

(defun mum-headerline--make-buffer-path ()
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
	  (let* ((dir-list (split-string (mum-headerline--path-trim dir) (mum-headerline--sys-separator)))
			 (dir-list-length (length dir-list)))
		(setq dir (concat (mum-headerline--sys-separator)
						  (car dir-list)
						  (mum-headerline--sys-separator)
						  drop-str
						  (mum-headerline--sys-separator)
						  (car (reverse dir-list))
						  (mum-headerline--sys-separator)))))
	(setq path (concat (propertize dir
								   'face 'mum-headerline--path-face)
					   (propertize file
								   'face 'mum-headerline--file-face)))
	path))

(defun mum-headerline--make-buffer-branch ()
  "Make buffer branch header-line."
  (propertize (if vc-mode
				  (let* ((backend (vc-backend buffer-file-name)))
					(substring-no-properties vc-mode
											 (+ (if (eq backend 'Hg) 2 3) 2)))
				"*non-vcs*")
			  'face 'mum-headerline--path-face))

(defun mum-headerline--make-buffer-major ()
  "Make buffer major mode header-line."
  (propertize (format "%s" (cdr (assoc 'major-mode (buffer-local-variables (current-buffer)))))
			  'face 'mum-headerline--path-face))

(defun mum-headerline--make-buffer-info ()
  "Make buffer info header-line."
  (list (mum-headerline--buffer-status)
		(mum-headerline--make-buffer-path)
		" "
		(mum-headerline--make-buffer-branch)
		" "
		(mum-headerline--make-buffer-major)))

(defun mum-headerline--active ()
  "Header line active."
  (interactive)
  (add-hook 'focus-in-hook #'mum-headerline--show-p)
  (add-hook 'window-configuration-change-hook #'mum-headerline--show-p)
  (unless (vwe@lib--buffer-match-p mum-headerline--buffer-filter-list)
	(setq-default header-line-format (mum-headerline--make-buffer-info))))

(defun mum-headerline--deactive ()
  "Header line deactive."
  (interactive)
  (remove-hook 'focus-in-hook #'mum-headerline--show-p)
  (remove-hook 'window-configuration-change-hook #'mum-headerline--show-p)
  (setq-default header-line-format nil))

(defun mum-headerline--buffer-match-p (regs &optional buffer)
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

(defun mum-headerline--show-p ()
  "Header line is show."
  (when mum-headerline-mode
	(unless (mum-headerline--buffer-match-p mum-headerline--buffer-filter-list)
	  (setq header-line-format (mum-headerline--make-buffer-info)))))

(define-minor-mode mum-headerline-mode
  "Mum headreline minor mode."
  :init-value nil
  :keymap nil
  :lighter ""
  :group 'mum-headerline
  :global t
  (if mum-headerline-mode (mum-headerline--active) (mum-headerline--deactive)))

(provide 'mum-headerline)
;;; mum-headerline.el ends here
