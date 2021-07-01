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

(defvar vwe-project--root-tag-file-name
  ".vwe"
  "Project root tag file name.")

(defvar vwe-project--cache-name
  ".vwe-project"
  "Project root tag file name.")

(defvar-local vwe-project--info
  nil
  "Project info.")

(defvar vwe-project--info-list
  nil
  "Project info list.")

(defcustom vwe-project--cache-path
  user-emacs-directory
  "Project cache path."
  :type 'string
  :group 'vwe-project)

(defcustom vwe-project--root-file-list
  '(vwe-project--root-tag-file-name ".git")
  "Root file list."
  :type 'list
  :group 'vwe-project)

(defcustom vwe-project--root-directory-list
  '(".snv")
  "Root directory list."
  :type 'list
  :group 'vwe-project)

(defun vwe-project--find-directory-last-name (dir)
  "Find DIR last directory name."
  (let* ((split-dir (split-string dir "/" t)))
	(nth (1- (length split-dir)) split-dir)))

;;;###autoload
(defun vwe-project--add-project (&optional dir name)
  "Add project for DIR and NAME."
  (interactive (let ((dir (read-directory-name "setup root:"
											   (vwe-project--find-root)
											   (vwe-project--find-root))))
				 (list dir)))
  (setq name (or name
				 (read-string "project name:"
							  (vwe-project--find-directory-last-name dir)
							  nil
							  (vwe-project--find-directory-last-name dir))))
  (vwe-project--make-root-tag-file dir name))

(defun vwe-project--make-root-tag-file (dir name)
  "Make root tag file for DIR and NAME."
  (when (and dir (file-directory-p dir))
	(let* ((name (or name (vwe-project--find-directory-last-name dir)))
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
		(let ((pro-info (list label (list label name dir)))
			  (pro-info-str (format "%s:%s:%s:%s\n"
									label name dir
									(time-to-seconds)))
			  (pro-file (format "%s%s"
								vwe-project--cache-path
								vwe-project--cache-name)))
		  (when (memq label vwe-project--info-list)
			(setq vwe-project--info-list (delq pro-info vwe-project--info-list)))
		  (setq vwe-project--info-list (append vwe-project--info-list (list pro-info))
				vwe-project--info pro-info)
		  (save-excursion
			(with-current-buffer (find-file-noselect pro-file)
			  (when (search-forward label nil t)
				(let* ((begin (progn (beginning-of-line) (point)))
					   (end (progn (end-of-line) (point))))
				  (setq buffer-read-only nil)
				  (delete-region begin (1+ end))
				  (save-buffer)))))
		  (with-temp-buffer (insert pro-info-str)
							(write-file tag-file)
							(append-to-file (point-min)
											(point-max)
											pro-file)))))))

(defun vwe-project--find-up-directory (dir)
  "Find the previous directory of the DIR."
  (when (file-directory-p dir)
	(let* ((dir-split (split-string dir "/" t))
		   (dir-length (length dir-split))
		   (up-dir (nth 0 (split-string dir (nth (1- dir-length) dir-split)))))
	  (if (or (= dir-length 0) (equal up-dir "")) nil up-dir))))

(defun vwe-project--find-root ()
  "Find root."
  (interactive)
  (let* ((dir default-directory)
		 (root nil))
	(catch 'break
	  (while dir
		(dotimes (i (length vwe-project--root-file-list))
		  (when (file-exists-p
				 (format "%s%s" dir (nth i vwe-project--root-file-list)))
			(setq root dir)
			(throw 'break nil)))
		(dotimes (i (length vwe-project--root-directory-list))
		  (when (directory-name-p
				 (format "%s%s" dir (nth i vwe-project--root-directory-list)))
			(setq root dir)
			(throw 'break nil)))
		(setq dir (vwe-project--find-up-directory dir))))
	root))

(defun vwe-project--format-project-info (info &optional arg)
  "Format project INFO.
return project info or someone ARG.
ARG privode `name' `label' `path'"
  (let* ((label)
		 (name)
		 (path))
	(if (listp info)
		(setq label (car info)
			  name (car (cdadr info))
			  path (cadr (cdadr info)))
	  (when (stringp info)
		(let ((strs (split-string info ":" t)))
		  (setq label (car strs)
				name (cadr strs)
				path (caddr strs)))))
	(cond
	 ((eq arg 'label) label)
	 ((eq arg 'name) name)
	 ((eq arg 'path) path)
	 (t (format "%s:%s:%s" label name path)))))

(defun vwe-project--switch-project (&optional info)
  "Switch project by INFO."
  (interactive
   (list (completing-read
		  (format "project (%s)"
				  (or (vwe-project--format-project-info vwe-project--info 'name) ""))
		  (mapcar (lambda (p)
					(vwe-project--format-project-info p))
				  vwe-project--info-list)
		  nil nil
		  (vwe-project--format-project-info vwe-project--info))))
  (find-file (vwe-project--format-project-info info 'path)))

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
