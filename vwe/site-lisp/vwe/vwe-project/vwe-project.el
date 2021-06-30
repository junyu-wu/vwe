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

(defcustom vwe-project--root-file-list
  '(".vwe" ".git")
  "Root file list."
  :type 'list
  :group 'vwe-project)

(defcustom vwe-project--root-directory-list
  '(".snv")
  "Root directory list."
  :type 'list
  :group 'vwe-project)

(defvar vwe-project--keymap
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Move to mark map.")

(defvar vwe-project--root-tag-file-name
  ".vwe"
  "Project root tag file name.")

(defvar-local vwe-project--info
  '(:name nil :label nil)
  "Project info.")

(defun vwe-project--setup-root (&optional dir)
  "Setup project root DIR."
  (interactive (let ((dir (read-directory-name "setup root:")))
				 (list dir)))
  (vwe-project--make-root-tag-file dir))

(defun vwe-project--make-root-tag-file (dir)
  "Make root tag file for DIR."
  (when (and dir (file-directory-p dir))
	(let* ((names (split-string dir "/" t))
		   (name (nth (length names) names))
		   (label (md5 dir))
		   (tag-file (format "%s%s"
							 dir
							 vwe-project--root-tag-file-name))
		   (overried nil))
	  (if (file-exists-p tag-file)
		  (when (y-or-n-p "Tag file existed, overried ? ")
			(setq overried t))
		(setq overried t))

	  (when overried
		(with-temp-buffer (insert (format "%s\n%s"
										  label
										  (format-time-string "%y-%m-%d %H:%M %a")))
						  (write-file tag-file))
		(setq vwe-project--info (list :name name :label label))))))

(defun vwe-project--find-up-directory (dir)
  "Find the previous directory of the DIR."
  (when (file-directory-p dir)
	(let* ((dir-split (split-string dir "/" t))
		   (dir-length (length dir-split))
		   (up-dir (nth 0 (split-string dir (nth (1- dir-length) dir-split)))))
	  (if (or (= dir-length 0) (equal up-dir ""))
		  nil
		up-dir))))

(defun vwe-project--find-root ()
  "Find root."
  (interactive)
  (let* ((dir default-directory)
		 (root dir))
	(while dir
	  (if (file-exists-p (concat dir vwe-project--root-tag-file-name))
		  (setq root dir
				dir nil)
		(setq dir (vwe-project--find-up-directory dir))))
	root))

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
