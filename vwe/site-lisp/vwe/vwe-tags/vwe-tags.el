;;; vwe-tags.el ---  Vwe tags            -*- lexical-binding: t; -*-

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
(defgroup vwe-tags nil
  "Tags."
  :group 'vwiss-vwe
  :prefix "vwe-tags--")

(defvar vwe-tags--keymap
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Move to mark map.")

(defcustom vwe-tags--command
  (or (executable-find "uctags") (executable-find "etags"))
  "Tags build command function.
reutrn command string to exec."
  :type 'string
  :group 'vwe-tags)

(defcustom vwe-tags--command-func
  'vwe-tags--build-ctags-cmd-func
  "Tags build command function.
reutrn command string to exec."
  :type 'function
  :group 'vwe-tags)

(defcustom vwe-tags--default-file-name
  "TAGS"
  "Default tags file name."
  :type 'string
  :group 'vwe-tags)

(defcustom vwe-tags--tags-file-cache-path
  (concat user-emacs-directory ".tags/")
  "Default tags file name."
  :type 'string
  :group 'vwe-tags)

(defvar-local vwe-tags--root-src
  nil
  "Root src.")

(defun vwe-tags--tags ()
  "Create or refresh tags file.
SRC create tags file path."
  (interactive)
  (setq vwe-tags--root-src (read-directory-name "src:"))
  (vwe-tags--exec-command (funcall vwe-tags--command-func)))

(defun vwe-tags--build-etags-cmd-func ()
  "Build etags command str."
  (let* ((src (or vwe-tags--root-src default-directory))
		 (name (md5 src))
		 ;; (cmd (format "%s -o %s%s -"
		 ;; 			  vwe-tags--command
		 ;; 			  vwe-tags--tags-file-cache-path
		 ;; 			  name))
		 (cmd (format "%s -o %s%s -" vwe-tags--command src vwe-tags--default-file-name)))
	(setq cmd (concat (format "find %s -name \"*\" | " src) cmd))
	cmd))

(defun vwe-tags--build-ctags-cmd-func ()
  "Build ctags command str."
  (let* ((src vwe-tags--root-src)
		 (cmd (format "%s -R -e -o %s%s" vwe-tags--command src vwe-tags--default-file-name)))
	(concat (format "cd %s && " src) cmd)))

(defun vwe-tags--exec-command (cmd)
  "Exec tags CMD command."
  (if cmd
	  (progn
		(with-temp-buffer
		  (shell-command cmd (buffer-name))
		  (message "tags command exec finished.")))
	(message "command is nil")))

(defun vwe-tags--remove-tags ()
  "Remove tags file."
  (interactive)
  (let* ((tags (concat (or vwe-tags--root-src (read-directory-name "remove:" nil default-directory))
					   vwe-tags--default-file-name)))
	(if (file-exists-p tags)
		(progn
		  (delete-file tags)
		  (message "remove tags %s finished." tags))
	  (message "remove tags failed. not found %s" tags))))

;;
;; mode
;;
(defun vwe-tags-mode-enable ()
  "Enable mode.")

(defun vwe-tags-mode-disable ()
  "Disable mode.")

;;;###autoload
(define-minor-mode vwe-tags-mode
  "Vwe tags minor mode."
  :group 'vwe-tags
  :keymap vwe-tags--keymap
  :global t
  (if vwe-tags-mode
	  (vwe-tags-mode-enable)
	(vwe-tags-mode-disable)))

(provide 'vwe-tags)
;;; vwe-tags.el ends here
