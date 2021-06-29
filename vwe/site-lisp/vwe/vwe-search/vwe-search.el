;;; vwe-search.el --- Vwe search        -*- lexical-binding: t; -*-

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

;; ripgrep

;;; Code:
(require 'cl-lib)1
(require 'subr-x)
(require 'grep)

(defgroup vwe-search nil
  "Vwe search."
  :prefix "vwe-look-for--"
  :group 'vwe)

(defcustom vwe-search--command
  "rg"
  "EXECUTE search command."
  :group 'vwe-search
  :type 'string)

(defcustom vwe-search--result-mode-after-hook
  nil
  "Result mode hook."
  :group 'vwe-search
  :type 'list)

(defcustom vwe-search--result-mode-before-hook
  nil
  "Result mode hook."
  :group 'vwe-search
  :type 'list)

(defcustom vwe-search--flash-line-delay
  1
  "Flash line delay."
  :type 'number
  :group 'vwe-search)

(defface vwe-search--default-face
  '((t (:inherit 'default :weight bold)))
  "Default face.")

(defface vwe-search--info-face
  '((t (:foreground "DarkOrange" :weight bold)))
  "Info face.")

(defface vwe-search--success-face
  '((t (:foreground "SpringGreen" :weight bold)))
  "Success face.")

(defface vwe-search--warning-face
  '((t (:foreground "yellow" :weight bold)))
  "Warning face.")

(defface vwe-search--error-face
  '((t (:foreground "DarkRed" :weight bold)))
  "Error face.")

(defface vwe-search--command-face
  '((t (:foreground "SkyBlue" :weight bold)))
  "Command face.")

(defface vwe-search--keyword-face
  '((t (:foreground "red" :weight bold)))
  "Keyword face.")

(defface vwe-search--file-face
  '((t (:foreground "DarkOrange" :weight bold)))
  "Command face.")

(defface vwe-search--position-face
  '((t (:foreground "Orange" :weight bold)))
  "Position face.")

(defface vwe-search--line-number-face
  '((t (:foreground "yellow" :weight bold)))
  "Line number face.")

(defface vwe-search--button-face
  '((t (:foreground "SkyBlue" :weight bold)))
  "Button face.")

(defface vwe-search--flash-line-face
  '((t (:inherit highlight)))
  "Flash the current line.")

(defvar vwe-search--current-search-info
  '()
  "Current search info `:keyword' `:directory' `:result'.")

(defvar vwe-search--result-buffer-name
  "**vwe-search-result**"
  "Buffer name.")

(defvar vwe-search--result-edit-buffer-name
  "**vwe-search-result-edit**"
  "Buffer name.")

(defvar vwe-search--default-parameters
  "--column --heading --hidden --with-filename --color=always --smart-case --max-columns=300"
  "Search command default parameters.")

(defvar vwe-search--result-total
  0
  "Result total.")

(defvar vwe-search--regexp-file
  "^[/\\~].*\\|^[a-z]:.*"
  "Regexp to match filename.")

(defvar vwe-search--regexp-empty-line
  "\n\n"
  "Regexp to match empty line.")

(defvar vwe-search--prefix-key
  "-e"
  "Keyword prefix parameter.")

(defvar vwe-search--apply-replace-alist
  nil
  "Apply replace alist.")

(defvar vwe-search--replace-key-to-value
  nil
  "Apply replace alist.")

(defvar vwe-search--keymap
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Search mode keymap.")

(defvar vwe-search--result-keymap
  (let ((keymap (make-sparse-keymap)))
	(define-key keymap (kbd "q") #'vwe-search--kill-result-buffer)
	(define-key keymap (kbd "n") #'next-line)
	(define-key keymap (kbd "p") #'previous-line)
	(define-key keymap (kbd "e") #'vwe-search--switch-to-edit-mode)
	(define-key keymap (kbd "RET") #'vwe-search--find-file)
	(define-key keymap (kbd "M-RET") #'vwe-search--find-file-after-back)
	keymap)
  "Search result mode keymap.")

(defvar vwe-search--edit-keymap
  (let ((keymap (make-sparse-keymap)))
	(define-key keymap (kbd "v") #'vwe-search--switch-to-result-mode)
	(define-key keymap (kbd "r") #'vwe-search--replace-match)
	(define-key keymap (kbd "x") #'vwe-search--apply-replace)
	(define-key keymap (kbd "q") #'vwe-search--kill-result-edit-buffer)
	keymap)
  "Search edit mode keymap.")

(defun vwe-search--at-point-char ()
  "Get current point char."
  (let* ((pos (point))
		 (pos-max (point-max)))
	(when (= pos pos-max) (setq pos (1- pos-max)))
	(unless nil ;; TODO 不是tab 空格 回车 换行字符
	  (buffer-substring-no-properties pos (1+ pos)))))

(defun vwe-search--read-keyword (&optional type)
  "Read keyword.
TYPE `word' `symbol' `point' `region' `input'."
  (let* ((key)
		 (sexp (thing-at-point 'sexp t))
		 (word (thing-at-point 'word t))
		 (symbol (thing-at-point 'symbol t))
		 (char (vwe-search--at-point-char)))
	(cond
	 ((equal 'word type) (setq key word))
	 ((equal 'symbol type) (setq key symbol))
	 ((equal 'point type) (setq key word))
	 ((region-active-p) (setq key (buffer-substring-no-properties (region-beginning) (region-end))))
	 (t (setq key (read-string (format "keyword%s:" (cond (sexp (format "[%s]" sexp))
														  (symbol (format "[%s]" symbol))
														  (word  (format "[%s]" word))
														  (t (format "[%s]" (if (equal (string-to-char char) 10)
																				(format " ") char))))) nil nil (or sexp symbol word) nil))))
	key))

(defun vwe-search--read-directory (&optional directory)
  "Read DIRECTORY."
  (interactive)
  (or directory (read-file-name "select dir:") default-directory))

(defun vwe-search--make-result-buffer ()
  "Make search result buffer."
  (let* ((buffer (get-buffer vwe-search--result-buffer-name)))
	(unless buffer
	  (setq buffer (get-buffer-create vwe-search--result-buffer-name)))
    (with-current-buffer buffer
	  (setq-local mode-line-format nil
				  show-trailing-whitespace nil
				  buffer-read-only nil)
	  (erase-buffer)
	  (vwe-search-result-mode)
	  (pop-to-buffer vwe-search--result-buffer-name)
      (goto-char (point-min)))
	buffer))

(defun vwe-search--kill-result-buffer ()
  "Kill search result buffer."
  (interactive)
  (when (get-buffer vwe-search--result-buffer-name) (kill-buffer (get-buffer vwe-search--result-buffer-name))))

(defun vwe-search--build-command (keyword directory parameters command)
  "Build search COMMAND based on KEYWORD DIRECTORY PARAMETERS and REGEXP."
  (let* ((prefix-key vwe-search--prefix-key)
		 (split " ")
		 (cmd vwe-search--command))
	(message "dir is:%s" directory)
	(setq cmd (concat command split (or parameters vwe-search--default-parameters) split prefix-key split keyword split directory))
	(when (memq system-type '(cygwin windows-nt ms-dos))
      (setq cmd (encode-coding-string cmd locale-coding-system)))
	cmd))

(defun vwe-search--build-result-buffer-headerline (keyword directory result)
  "KEYWORD DIRECTORY and RESULT build result buffer headerline."
  (concat (propertize (format "vwe serch: ")
					  'face 'vwe-search--success-face)
		  (propertize (format "[keyword | '")
					  'face 'vwe-search--success-face)
		  (propertize (format "%s" keyword)
					  'face 'vwe-search--info-face)
		  (propertize (format "'] ")
					  'face 'vwe-search--success-face)
		  (propertize (format "[dir | '")
					  'face 'vwe-search--success-face)
		  (propertize (format "%s" directory)
					  'face 'vwe-search--info-face)
		  (propertize (format "'] ")
					  'face 'vwe-search--success-face)
		  (propertize (format "[result | '")
					  'face 'vwe-search--success-face)
		  (propertize (format "%s" result)
					  'face 'vwe-search--warning-face)
		  (propertize (format "'] ")
					  'face 'vwe-search--success-face)))

(defun vwe-search--result-filter ()
  "Filter."
  (save-excursion
    (forward-line 0)
    (let ((cursor (point))
		  (finish))
	  (goto-char compilation-filter-start)
	  (forward-line 0)
	  (setq finish (point))
	  (when (< (point) cursor)
        (setq cursor (copy-marker cursor))
        (while (re-search-forward "/.*:\\s-error\\s-parsing\\s-glob\\s-.*" cursor 1) (replace-match "" t t))
        (goto-char finish)
        (while (re-search-forward "^\033\\[[0]*m\033\\[35m\\(.*?\\)\033\\[[0]*m$" cursor 1)
		  (replace-match (concat (propertize (match-string 1) 'face nil 'font-lock-face 'vwe-search--file-face)) t t))
        (goto-char finish)
        (while (re-search-forward "\033\\[[0]*m\033\\[[3]*1m\033\\[[3]*1m\\(.*?\\)\033\\[[0]*m" cursor 1)
		  (replace-match (propertize (match-string 1) 'face nil 'font-lock-face 'vwe-search--keyword-face) t t)
		  (setq vwe-search--result-total (+ vwe-search--result-total 1))
		  (plist-put vwe-search--current-search-info :result vwe-search--result-total)
		  (setq-local header-line-format (vwe-search--build-result-buffer-headerline (plist-get vwe-search--current-search-info :keyword)
																					 (plist-get vwe-search--current-search-info :directory)
																					 (plist-get vwe-search--current-search-info :result))))
        (goto-char finish)
        (while (re-search-forward "\033\\[[0-9;]*[0mK]" cursor 1) (replace-match "" t t))))))

(defun vwe-search--match-result-file ()
  "Match result file."
  (save-excursion
    (search-backward-regexp vwe-search--regexp-file nil t)
	(let* ((file(string-remove-suffix "\n" (thing-at-point 'line))))
	  (when (and file (file-exists-p file))
		file))))

(defun vwe-search--match-result-file-of-line ()
  "Match result file."
  (let* ((path))
	(save-excursion
	  (beginning-of-line)
      (search-forward-regexp vwe-search--regexp-file (line-end-position) t)
	  (let* ((file (string-remove-suffix "\n" (thing-at-point 'line))))
		(when (and file (not (equal file "")) (file-exists-p file))
		  (setq path (substring-no-properties file)))))
	path))

(defun vwe-search--match-result-line ()
  "Match result line."
  (save-excursion
	(beginning-of-line)
	(let* ((line))
	  (condition-case nil
		  (progn
			(setq line (string-to-number (thing-at-point 'symbol))))
		(error line)))))

(defun vwe-search--match-result-column ()
  "Match result column."
  (save-excursion
	(search-forward ":")
	(string-to-number (thing-at-point 'symbol))))

(defun vwe-search--match-result-buffer (path)
  "Match result buffer with PATH."
  (catch 'find-match
    (dolist (buffer (buffer-list))
      (when (string-equal (buffer-file-name buffer) path)
        (throw 'find-match buffer)))
    nil))

(defun vwe-search--goto-column (column)
  "Goto line COLUMN."
  (let ((scan-column 0)
        (first-char-point (point)))
    (while (> column scan-column)
      (forward-char 1)
      (setq scan-column (string-bytes (buffer-substring first-char-point (point)))))
    (backward-char 1)))

(defun vwe-search--goto-point (line column)
  "Goto find buffer LINE and COLUMN."
  (goto-char (point-min))
  (forward-line (1- line))
  (beginning-of-line)
  (vwe-search--goto-column column))

(defun vwe-search--flash-line ()
  "Flash line."
  (let ((pulse-iterations 1)
        (pulse-delay vwe-search--flash-line-delay))
    (pulse-momentary-highlight-one-line (point) 'vwe-search--flash-line-face)))

(defun vwe-search--find-file (&optional refind)
  "Find result file with REFIND."
  (interactive)
  (let* ((file (vwe-search--match-result-file))
		 (line (vwe-search--match-result-line))
		 (column (vwe-search--match-result-column)))
	(save-excursion
	  (let ((inhibit-message t))
		(vwe-search--goto-column column)
		(other-window 1)
		(when file
		  (find-file file)
		  (cond (t (vwe-search--goto-point line column))))
		(vwe-search--flash-line)))
	(when refind
	  (let* ((buffer (get-buffer vwe-search--result-buffer-name))
			 (window (get-buffer-window buffer)))
		(if buffer
			(select-window window)
		  (split-window)
		  (other-window 1)
		  (switch-to-buffer buffer))))))

(defun vwe-search--find-file-after-back ()
  "Find file after back to result buffer."
  (interactive)
  (vwe-search--find-file t))

(defun vwe-search--engine (&optional keyword directory parameters command)
  "Execute COMMAND to Search KEYWORD in DIRECTORY with PARAMETERS."
  (let* ((dir (concat (if (and directory (directory-name-p directory)) directory default-directory) "*"))
		 (key (or keyword
				  (cond ((thing-at-point 'sexp t) (format "%s" (thing-at-point 'sexp t)))
						((thing-at-point 'symbol t) (format "%s" (thing-at-point 'symbol t)))
						((thing-at-point 'word t)  (format "%s" (thing-at-point 'word t)))
						(t (format "%s" (if (equal (string-to-char char) 10)
											(format " ") char))))))
		 (cmd (or command vwe-search--command))
		 (cmd-str (vwe-search--build-command key dir parameters cmd))
		 (buffer (vwe-search--make-result-buffer)))
	(message "cmd str:%s" dir)
	(when (bufferp buffer)
	  (setq vwe-search--result-total 0
			vwe-search--current-search-info nil)
	  (with-current-buffer buffer
		(compilation-start cmd-str 'vwe-search-result-mode)
		(setq vwe-search--current-search-info (list :keyword key :directory dir :command cmd :parameters parameters :result vwe-search--result-total))
		(setq header-line-format (vwe-search--build-result-buffer-headerline key dir vwe-search--result-total))
		(pop-to-buffer buffer)
		(goto-char (point-min))))))

(defun vwe-search--rg ()
  "Rg search."
  (interactive)
  (let* ((dir (vwe-search--read-directory))
		 (key (vwe-search--read-keyword))
		 (parameters nil)
		 (cmd vwe-search--command))
	(vwe-search--engine key dir parameters cmd)))

(defun vwe-search--process-setup-function (&rest _args)
  "Process setup function format input message."
  ;; `compilation-exit-message-function' (lambda (_process-status exit-status msg) (cons msg exit-status))
  (let* ((buffer (get-buffer vwe-search--result-buffer-name)))
	(set (make-local-variable 'compilation-exit-message-function)
		 (lambda (_process-status exit-status msg)
		   (when (eq _process-status 'exit)
			 (cond ((and (zerop exit-status) (buffer-modified-p))
					(setq msg (format "(keyword '%s' matched %s)" (plist-get vwe-search--current-search-info :keyword) vwe-search--result-total)))))
		   (cons msg exit-status)))))

(defun vwe-search--highlight-result ()
  "Hightlight search result keywords."
  (font-lock-add-keywords nil
						  '(("^rg\\s-.*" . 'vwe-search--command-face)
							("^\\([1-9][0-9]*\\)\\(:\\)\\([1-9][0-9]*\\)\\(:\\)" 1 'vwe-search--line-number-face)
							("^\\([1-9][0-9]*\\)\\(:\\)\\([1-9][0-9]*\\)\\(:\\)" 2 'vwe-search--position-face)
							("^\\([1-9][0-9]*\\)\\(:\\)\\([1-9][0-9]*\\)\\(:\\)" 3 'vwe-search--line-number-face)
							("^\\([1-9][0-9]*\\)\\(:\\)\\([1-9][0-9]*\\)\\(:\\)" 4 'vwe-search--position-face)
							("^[/\\~].*\\|^[a-z]:.*" . 'vwe-search--file-face)))
  (set (make-local-variable 'font-lock-keywords-only) t)
  (font-lock-mode 1))

(defun vwe-search--clone-result-to-edit-buffer ()
  "Clone result to edit buffer."
  (vwe-search--kill-result-edit-buffer)
  (with-current-buffer (get-buffer vwe-search--result-buffer-name)
    (add-hook 'kill-buffer-hook 'vwe-search--kill-result-edit-buffer nil t)
    (generate-new-buffer vwe-search--result-edit-buffer-name)
    (append-to-buffer vwe-search--result-edit-buffer-name (point-min) (point-max))))

(defun vwe-search--kill-result-edit-buffer ()
  "Kill result edit buffer."
  (interactive)
  (when (get-buffer vwe-search--result-edit-buffer-name)
    (kill-buffer vwe-search--result-edit-buffer-name)))

(defun vwe-search--replace-match ()
  "Replace match."
  (interactive)
  (when vwe-search--apply-replace-alist (setq vwe-search--apply-replace-alist nil))
  (save-excursion
	(let* ((keyword (plist-get vwe-search--current-search-info :keyword))
		   (result (plist-get vwe-search--current-search-info :result))
		   (max-line (save-excursion (goto-char (point-max)) (line-number-at-pos)))
		   (to-str (read-string (format "replace '%s' to:" keyword) keyword))
		   (result-format) ;; '((file ((line column) (line column))))
		   (index 0)
		   (cur-line))
	  (read-only-mode -1)
	  (goto-char (point-min))
	  (setq cur-line (line-number-at-pos))
	  (catch 'break
		(while (< cur-line max-line)
		  (let* ((file)
				 (sub-result))
			(if (vwe-search--match-result-file-of-line)
				(progn
				  (setq file (vwe-search--match-result-file-of-line))
				  (forward-line 1)
				  (while (and (vwe-search--match-result-line) (> (vwe-search--match-result-line) 0))
					(let* ((line (vwe-search--match-result-line))
						   (column (vwe-search--match-result-column))
						   (position (list (list line column)))
						   (file-result (append (cadr sub-result) position)))
					  (when (and line column)
						(setq sub-result (list file file-result))
						(beginning-of-line)
						(while (search-forward keyword (line-end-position) t)
						  (replace-match to-str))
						(setq index (1+ index))))
					(forward-line 1)))
			  (forward-line 1))
			(when sub-result
			  (setq result-format (cons sub-result result-format))))
		  (setq cur-line (line-number-at-pos))))
	  (read-only-mode 1)
	  (setq vwe-search--apply-replace-alist result-format
			vwe-search--replace-key-to-value (list keyword to-str))
	  (message "match result %d to replace %d." result index))))

(defun vwe-search--apply-replace ()
  "Apply replace."
  (interactive)
  (when vwe-search--apply-replace-alist
	(dotimes (file-index (length vwe-search--apply-replace-alist))
	  (save-excursion
		(let* ((keyword (car vwe-search--replace-key-to-value))
			   (to-str (cadr vwe-search--replace-key-to-value))
			   (file (car (nth file-index vwe-search--apply-replace-alist)))
			   (replace-item (cadr (nth file-index vwe-search--apply-replace-alist)))
			   (replace-len (length replace-item))
			   (buffer (find-file-noselect file)))
		  (when buffer
			(with-current-buffer buffer
			  (when buffer-read-only (setq buffer-read-only nil))
			  (dotimes (rep-index replace-len)
				(let* ((row-info (nth rep-index replace-item))
					   (line (car row-info)))
				  (when (and line (> line 0))
					(goto-line line)
					(beginning-of-line)
					(while (search-forward keyword (line-end-position) t)
					  (replace-match to-str)))))
			  (save-buffer))))))))

(defun vwe-search--switch-to-edit-mode ()
  "Switch to edit mode."
  (interactive)
  (when (equal major-mode 'vwe-search-result-mode)
	(vwe-search--clone-result-to-edit-buffer)
	(let* ((buffer (get-buffer vwe-search--result-edit-buffer-name)))
	  (with-current-buffer buffer
		(goto-char (point-min))
		(vwe-search-edit-mode))
	  (switch-to-buffer buffer))))

(defun vwe-search--switch-to-result-mode ()
  "Switch to result mode."
  (interactive)
  (when (equal major-mode 'vwe-search-edit-mode)
	(let* ((buffer (get-buffer vwe-search--result-buffer-name)))
	  (when buffer
		(switch-to-buffer buffer)))))

(define-derived-mode vwe-search-edit-mode text-mode "vwe-edit-result"
  "Search result minor mode."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'vwe-search-edit-mode
		mode-name "vwe-search-edit-mode"
		vwe-search--apply-replace-alist nil)
  (read-only-mode 1)
  (use-local-map vwe-search--edit-keymap))

(define-derived-mode vwe-search-result-mode text-mode "vwe-search-result"
  "Search result minor mode."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'vwe-search-result-mode
		mode-name "vwe-search-result-mode")
  (read-only-mode 1)
  (vwe-search--highlight-result)
  (use-local-map vwe-search--result-keymap)
  (add-hook 'compilation-filter-hook 'vwe-search--result-filter nil t)
  (set (make-local-variable 'compilation-process-setup-function) 'vwe-search--process-setup-function)
  (run-hooks 'vwe-search--result-mode-after-hook))

(defun vwe-search--enable ()
  "Enable mode.")

(defun vwe-search--disable ()
  "Disable mode.")

;;;###autoload
(define-minor-mode vwe-search-mode
  "Vwe search minor mode."
  :group 'vwe-search
  :keymap vwe-search--keymap
  (if vwe-search-mode
	  (vwe-search--enable)
	(vwe-search--disable)))

(provide 'vwe-search)
;;; vwe-search.el ends here
