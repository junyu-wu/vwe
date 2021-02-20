;;; mum-look-for.el ---  Mum look for     -*- lexical-binding: t; -*-

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

(defgroup mum-look-for nil
  "Mum look for."
  :prefix "mum-look-for--"
  :group 'mum)

(defvar mum-look-for--buffer-name
  "**look for**"
  "Buffer name.")

(defvar mum-look-for--keymap
  (let ((keymap (make-sparse-keymap)))
	(define-key keymap (kbd "q") (lambda () (interactive) (kill-buffer mum-look-for--buffer-name)))
	keymap)
  "Mode keymap.")

(defvar mum-look-for--recover-snapshot
  '(:window nil :point nil)
  "Window buffer and point recover snapshot.")

(defvar mum-look-for--current-search-info
  nil ;; '(:keyword nil :cmd nil :dir nil :args nil :filters nil :hits nil :keymaps nil)
  "Current search info.")

(defcustom mum-look-for--command
  "rg"
  "EXECUTE search command."
  :group 'mum-look-for
  :type 'string)

(defcustom mum-look-for--command-default-args
  nil
  ;; '(;;
  ;; 	:regexp nil ;; regexp or fixed strings
  ;; 	:sensitive nil ;; case sensitive
  ;; 	:hidden nil ;; hidden files
  ;; 	:ignore nil ;; ignore files
  ;; 	:glob nil ;; wildcard
  ;; 	:depth nil ;; search max depth
  ;; 	:pre nil ;; before command
  ;; 	:zip nil ;; search gz,bz2,xz,lzma,lz4 ... files
  ;; 	:type nil ;; search file type
  ;; 	:no-type nil ;; ignore file type
  ;; 	:invert nil ;; condition invert
  ;; 	:ignore-path nil ;; ignore path
  ;; 	)
  "Command default args."
  :group 'mum-look-for
  :type 'list)

(defcustom mum-look-for--command-default-filters
  nil
  "Command default filters."
  :group 'mum-look-for
  :type 'list)

;;
;; buffer
;;
(defun mum-look-for--make-buffer ()
  "Make search result buffer."
  (let* ((buffer (get-buffer mum-look-for--buffer-name)))
	(unless buffer
	  (setq buffer (get-buffer-create mum-look-for--buffer-name)))
    (with-current-buffer buffer
	  (setq-local mode-line-format nil
				  show-trailing-whitespace nil
				  buffer-read-only nil)
	  (erase-buffer)
	  (mum-look-for--view-mode t)
	  (pop-to-buffer mum-look-for--buffer-name)
      (goto-char (point-min)))
	buffer))

(defun mum-look-for--make-current-search-info (keyword dir cmd args hits)
  "Make current search info.
KEYWORD search keyword.
DIR search directory.
CMD dearch command.
ARGS search command args.
HITS serach result count."
  (plist-put mum-look-for--current-search-info :keyword keyword)
  (plist-put mum-look-for--current-search-info :dir (or dir default-directory))
  (plist-put mum-look-for--current-search-info :cmd (or cmd mum-look-for--command))
  (plist-put mum-look-for--current-search-info :args args)
  (plist-put mum-look-for--current-search-info :hits hits))

(defun mum-look-for--get-current-search-info ()
  "Get current search info."
  (if mum-look-for--current-search-info
	  (concat (propertize (format "look for|")) ;; title
			  (propertize (format "%s " (plist-get mum-look-for--current-search-info :cmd))) ;; cmd
			  (propertize (format "keyword|"))
			  (propertize (format "%s " (plist-get mum-look-for--current-search-info :keyword))) ;; keyword
			  (propertize (format "dir|"))
			  (propertize (format "%s " (plist-get mum-look-for--current-search-info :dir))) ;; dir
			  (propertize (format "args|"))
			  (propertize (format "%s " (plist-get mum-look-for--current-search-info :args)))
			  (propertize (format "hits|"))
			  (propertize (format "%s " (plist-get mum-look-for--current-search-info :hits))) ;; hits
			  )
	(format "not found result")))

(defun mum-look-for--is-current-buffer-p ()
  "Current buffer is look for buffer?"
  (string-equal (buffer-name) mum-look-for--buffer-name))

;;
;; search command
;;
(defun mum-look-for--at-point-char ()
  "Get current point char."
  (let* ((p (point))
		 (pm (point-max)))
	(when (= p pm) (setq p (1- pm)))
	(buffer-substring-no-properties p (1+ p))))

(defun mum-look-for--read-keyword (&optional type)
  "Read keyword.
TYPE `word' `symbol' `point' `region' `input'."
  (let* ((key)
		 (word (thing-at-point 'word t))
		 (symbol (thing-at-point 'symbol t))
		 (char (mum-look-for--at-point-char)))
	(message "char is %s" char)
	(cond
	 ((equal 'word type) (setq key word))
	 ((equal 'symbol type) (setq key symbol))
	 ((equal 'point type) (setq key word))
	 ((region-active-p) (setq key (buffer-substring-no-properties (region-beginning) (region-end))))
	 (t (setq key (read-string (format "keyword%s:" (cond (symbol (format "[%s]" symbol))
														  (word  (format "[%s]" word))
														  (t (format "[%s]" (if (equal (string-to-char char) 10) (format " ") char)))))))))
	key))

(defun mum-look-for--make-search-command (keyword directory parameters command)
  "Make search COMMAND based on KEYWORD DIRECTORY and PARAMETERS."
  (let* ((key (or keyword))
		 (dir (or directory default-directory))
		 (cmd (or command mum-look-for--command))
		 (args (or parameters)))
	(when key
	  (concat (format "%s " cmd)
			  (when args
				(format "%s" args))
			  (format "%s " key)
			  (format "%s " dir)))))

(defun mum-look-for--search (&optional keyword directory parameters command)
  "Execute COMMAND to Search KEYWORD in DIRECTORY with PARAMETERS."
  (let* ((key (or keyword (mum-look-for--read-keyword)))
		 (dir (or directory default-directory))
		 (cmd (mum-look-for--make-search-command key dir command parameters))
		 (buffer (mum-look-for--make-buffer)))
	(when (bufferp buffer)
	  (with-current-buffer buffer
		(compilation-start cmd 'mum-look-for-mode)
		(mum-look-for--make-current-search-info key dir cmd parameters 0)
		(setq-local header-line-format (mum-look-for--get-current-search-info))))))

;;
;; rg
;;
(defun mum-look-for--rg (&optional keyword directory parameters)
  "Rg Search input KEYWORD in DIRECTORY.
DIRECTORY default is current directory.
PARAMETERS is rg parameter."
  (interactive)
  (mum-look-for--search keyword directory "rg" parameters))

;;
;; mode
;;
(defun mum-look-for--enable-view ()
  "Enable mode.")

(defun mum-look-for--disable-view ()
  "Disable mode.")

(define-minor-mode mum-look-for--view-mode
  "Mum look for serach result view mode."
  :group 'mum-look-for
  :keymap mum-look-for--keymap
  (if mum-look-for--view-mode
	  (mum-look-for--enable-view)
	(mum-look-for--disable-view)))

(defun mum-look-for--enable ()
  "Enable mode.")

(defun mum-look-for--disable ()
  "Disable mode.")

;;;###autoload
(define-minor-mode mum-look-for-mode
  "Mum look for minor mode."
  :group 'mum-look-for
  (if mum-look-for-mode
	  (mum-look-for--enable)
	(mum-look-for--disable)))

(provide 'mum-look-for)
;;; mum-look-for.el ends here
