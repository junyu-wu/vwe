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
  "Customization group for beacon."
  :group 'vwiss-vwe
  :prefix "vwe-tags--")

(defvar vwe-tags--keymap
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Move to mark map.")

(defcustom vwe-tags--command
  'etags
  "Tags defaultcommand."
  :type 'string
  :group 'vwe-tags)

(defcustom vwe-tags--shell
  nil
  "Tags defaultcommand."
  :type 'string
  :group 'vwe-tags)

(defcustom vwe-tags--default-tags-file-name
  "TAGS"
  "Default tags file name."
  :type 'string
  :group 'vwe-tags)

(defun vwe-tags--tags (&optional src name language extension)
  "Create or refresh tags.
SRC is tags directory.
NAME is tags file name.
LANGUAGE setup tags language.
EXTENSION setup tags file type."
  (interactive)
  (when (or (null src) (directory-name-p src))
	(setq src (read-file-name "directory:" nil (file-name-directory (buffer-file-name)))))
  (unless language
	(setq language (read-string "language(all):")))
  (unless extension
	(setq extension "*"))
  (unless name
	(setq name (read-string (format "tags name(%s):"
									vwe-tags--default-tags-file-name)
							nil nil
							vwe-tags--default-tags-file-name)))
  (string-match (regexp-quote ".") extension)
  (vwe-tags--apply-command src name language extension))

(defun vwe-tags--apply-command (src name language extension)
  "Build tags command.
SRC is tags directory.
NAME is tags file name.
LANGUAGE setup tags language.
EXTENSION setup tags file type."
  (when (or (null src) (directory-name-p src))
	(setq src (file-name-directory (buffer-file-name))))
  (unless language
	(setq language ""))
  (unless extension
	(setq extension "*"))
  (unless name
	(setq name vwe-tags--default-tags-file-name))

  (let* ((cmd))
	(cond
	 ((eq 'etags vwe-tags--command)
	  (progn
		(setq cmd (format "find %s -type f -name \"%s\" | etags -o %s %s -"
						  src
						  (if (string-match (regexp-quote ".") extension)
							  extension (concat "*." extension))
						  name
						  (if (equal language "")
							  "" (concat "-l " language))))))
	 ((eq 'uctags vwe-tags--command) (progn
									   )))
	(when cmd (vwe-tags--exec cmd)
		  (message "tags file %s%s create finished" src name))))

(defun vwe-tags--exec (cmd)
  "Run CMD to create for tags."
  (if cmd
	  (progn
		(with-temp-buffer (shell-command cmd (buffer-name))))
	(message "command is nil")))

(defun vwe-tags--refresh-tags ()
  "Refresh tags."
  (interactive))

(defun vwe-tags--delete-tags (&optional src)
  "Delete tags for SRC."
  (interactive)
  (let* ((tags-names (list "tags" "TAGS" "ETAGS")))
	(unless src ;; (setq src (file-name-directory (buffer-file-name)))
	  (setq src (read-directory-name "directory:")))
	(dotimes (i (length tags-names))
	  (let* ((file (concat src (nth i tags-names))))
		(when (file-exists-p file)
		  (delete-file file)
		  (message "delete %s file" file))))))

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
