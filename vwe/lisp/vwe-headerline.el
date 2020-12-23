;;; vwe-headerline.el --- Header line display      -*- lexical-binding: t; -*-

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

;;

;;; Code:
;; ***************************************************************************
;; lib
;; ***************************************************************************
(defun vwe@headerline--buffer-status ()
  "Get current buffer status."
  (let* ((status (vwe@lib--buffer-status))
		 (status-str (concat "[" (nth 1 status) "]")))
	(cond
	 ((> (car status) 0) (vwe@lib--face-of-string status-str
												  :background "DarkRed"
												  :foreground "white"
												  :weight 'bold))
	 ((= (car status) 0) (vwe@lib--face-of-string status-str
												  :background "DarkGreen"
												  :foreground "white"
												  :weight 'bold))
	 ((< (car status) 0) (vwe@lib--face-of-string status-str
												  :background "DarkOrange"
												  :foreground "white"
												  :weight 'bold)))))
(defun vwe@headerline--make-buffer-path ()
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
	  (let* ((dir-list (split-string (vwe@lib--path-trim dir) (vwe@lib--sys-separator)))
			 (dir-list-length (length dir-list)))
		(setq dir (concat (vwe@lib--sys-separator)
						  (car dir-list)
						  (vwe@lib--sys-separator)
						  drop-str
						  (vwe@lib--sys-separator)
						  (car (reverse dir-list))
						  (vwe@lib--sys-separator)))))
	(setq path (concat (vwe@lib--face-of-string dir
												:background "#434C5E"
												:foreground "#B0BEC5"
												:weight 'bold)
					   (vwe@lib--face-of-string file
												:background "#434C5E"
												:foreground "white"
												:weight 'bold)))
	path))

(defun vwe@headerline--make-buffer-branch ()
  "Make buffer branch header-line."
  (vwe@lib--face-of-string (vwe@lib--buffer-vcs-branch)
						   :background "#434C5E"
						   :foreground "#B0BEC5"
						   :weight 'bold))

(defun vwe@headerline--make-buffer-major ()
  "Make buffer major mode header-line."
  (vwe@lib--face-of-string (format "%S" (vwe@lib--buffer-major-mode))
						   :background "#434C5E"
						   :foreground "#B0BEC5"
						   :weight 'bold))

(defun vwe@headerline--make-buffer-info ()
  "Make buffer info header-line."
  (list (vwe@headerline--buffer-status)
		(vwe@headerline--make-buffer-path)
		" "
		(vwe@headerline--make-buffer-branch)
		" "
		(vwe@headerline--make-buffer-major)))

(defun vwe@headerline--active ()
  "Header line active."
  (interactive)
  (unless (vwe@lib--buffer-match-p vwe@custom--buffer-filter-list)
	(setq-default header-line-format (vwe@headerline--make-buffer-info))))

(defun vwe@headerline--deactive ()
  "Header line deactive."
  (interactive)
  (setq-default header-line-format 'nil))

(defun vwe@headerline--show-p ()
  "Header line is show."
  (if vwe@custom--headerline-show?
	  (progn
		(unless (vwe@lib--buffer-match-p vwe@custom--buffer-filter-list)
		  (setq header-line-format (vwe@headerline--make-buffer-info))))
	'nil))

(defun vwe@headerline--init ()
  "Header-line init."
  (interactive)

  (add-hook 'buffer-list-update-hook #'vwe@headerline--show-p)
  (add-hook 'window-configuration-change-hook #'vwe@headerline--show-p)
  (add-hook 'find-file-hook #'vwe@headerline--show-p)
  (add-hook 'after-save-hook #'vwe@headerline--show-p)

  (unless vwe@custom--headerline-show?
	(vwe@headerline--deactive)))

;; ***************************************************************************
;; config
;; ***************************************************************************
(vwe@headerline--init)

(provide 'vwe-headerline)
;;; vwe-headerline.el ends here
