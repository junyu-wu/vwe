;;; mum-search.el --- Mum search        -*- lexical-binding: t; -*-

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
(require 'cl-lib)
(require 'subr-x)
(require 'grep)

(defgroup mum-search nil
  "Mum search."
  :prefix "mum-look-for--"
  :group 'mum)

(defcustom mum-search--command
  "rg"
  "EXECUTE search command."
  :group 'mum-search
  :type 'string)

(defcustom mum-search--result-mode-after-hook
  nil
  "Result mode hook."
  :group 'mum-search
  :type 'list)

(defcustom mum-search--result-mode-before-hook
  nil
  "Result mode hook."
  :group 'mum-search
  :type 'list)

(defcustom mum-search--flash-line-delay
  1
  "Flash line delay."
  :type 'number
  :group 'mum-search)

(defface mum-search--default-face
  '((t (:inherit 'default :weight bold)))
  "Default face.")

(defface mum-search--info-face
  '((t (:foreground "DarkOrange" :weight bold)))
  "Info face.")

(defface mum-search--success-face
  '((t (:foreground "SpringGreen" :weight bold)))
  "Success face.")

(defface mum-search--warning-face
  '((t (:foreground "yellow" :weight bold)))
  "Warning face.")

(defface mum-search--error-face
  '((t (:foreground "DarkRed" :weight bold)))
  "Error face.")

(defface mum-search--command-face
  '((t (:foreground "SkyBlue" :weight bold)))
  "Command face.")

(defface mum-search--keyword-face
  '((t (:foreground "red" :weight bold)))
  "Keyword face.")

(defface mum-search--file-face
  '((t (:foreground "DarkOrange" :weight bold)))
  "Command face.")

(defface mum-search--position-face
  '((t (:foreground "Orange" :weight bold)))
  "Position face.")

(defface mum-search--line-number-face
  '((t (:foreground "yellow" :weight bold)))
  "Line number face.")

(defface mum-search--button-face
  '((t (:foreground "SkyBlue" :weight bold)))
  "Button face.")

(defface mum-search--flash-line-face
  '((t (:inherit highlight)))
  "Flash the current line.")

(defvar mum-search--current-search-info
  '()
  "Current search info.")

(defvar mum-search--result-buffer-name
  "**mum-search-result**"
  "Buffer name.")

(defvar mum-search--result-temp-buffer-name
  "**mum-search-result-temp**"
  "Buffer name.")

(defvar mum-search--default-parameters
  "--column --color=always --smart-case --max-columns=300"
  "Search command default parameters.")

(defvar mum-search--result-total
  0
  "Result total.")

(defvar mum-search--regexp-file
  "^[/\\~].*\\|^[a-z]:.*"
  "Regexp to match filename.")

(defvar mum-search--regexp-empty-line
  "\n\n"
  "Regexp to match empty line.")

(defvar mum-search--prefix-key
  "-e"
  "Keyword prefix parameter.")

(defvar mum-search--keymap
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Search mode keymap.")

(defvar mum-search--result-keymap
  (let ((keymap (make-sparse-keymap)))
	(define-key keymap (kbd "q") #'mum-search--kill-result-buffer)
	(define-key keymap (kbd "n") #'next-file)
	(define-key keymap (kbd "p") #'previous-line)
	(define-key keymap (kbd "RET") #'mum-search--find-file)
	(define-key keymap (kbd "M-RET") #'mum-search--find-file-after-back)
	keymap)
  "Search result mode keymap.")

(defun mum-search--at-point-char ()
  "Get current point char."
  (let* ((pos (point))
		 (pos-max (point-max)))
	(when (= pos pos-max) (setq pos (1- pos-max)))
	(unless nil ;; TODO 不是tab 空格 回车 换行字符
	  (buffer-substring-no-properties pos (1+ pos)))))

(defun mum-search--read-keyword (&optional type)
  "Read keyword.
TYPE `word' `symbol' `point' `region' `input'."
  (let* ((key)
		 (word (thing-at-point 'word t))
		 (symbol (thing-at-point 'symbol t))
		 (char (mum-search--at-point-char)))
	(cond
	 ((equal 'word type) (setq key word))
	 ((equal 'symbol type) (setq key symbol))
	 ((equal 'point type) (setq key word))
	 ((region-active-p) (setq key (buffer-substring-no-properties (region-beginning) (region-end))))
	 (t (setq key (read-string (format "keyword%s:" (cond (symbol (format "[%s]" symbol))
														  (word  (format "[%s]" word))
														  (t (format "[%s]" (if (equal (string-to-char char) 10)
																				(format " ") char))))) nil nil word nil))))
	key))

(defun mum-search--read-directory (&optional directory)
  "Read DIRECTORY."
  (interactive)
  (or directory (read-file-name "select dir:") default-directory))

(defun mum-search--make-result-buffer ()
  "Make search result buffer."
  (let* ((buffer (get-buffer mum-search--result-buffer-name)))
	(unless buffer
	  (setq buffer (get-buffer-create mum-search--result-buffer-name)))
    (with-current-buffer buffer
	  (setq-local mode-line-format nil
				  show-trailing-whitespace nil
				  buffer-read-only nil)
	  (erase-buffer)
	  (mum-search-result-mode)
	  (pop-to-buffer mum-search--result-buffer-name)
      (goto-char (point-min)))
	buffer))

(defun mum-search--clone-result-to-temp-buffer ()
  "Clone result to temp buffer."
  (mum-search--kill-result-temp-buffer)
  (with-current-buffer (get-buffer mum-search--result-buffer-name)
    (add-hook 'kill-buffer-hook 'mum-search--kill-result-temp-buffer nil t)
    (generate-new-buffer mum-search--result-temp-buffer-name)
    (append-to-buffer mum-search--result-temp-buffer-name (point-min) (point-max))))

(defun mum-search--kill-result-buffer ()
  "Kill search result buffer."
  (interactive)
  (when (get-buffer mum-search--result-buffer-name) (kill-buffer (get-buffer mum-search--result-buffer-name))))

(defvar mum-search--last-change-lines nil)
(defun mum-search--kill-result-temp-buffer ()
  "Kill result temp buffer."
  (when (get-buffer mum-search--result-temp-buffer-name)
    (kill-buffer mum-search--result-temp-buffer-name)
    (setq mum-search--last-change-lines nil)))

(defun mum-search--build-command (keyword directory parameters command &optional regexp)
  "Build search COMMAND based on KEYWORD DIRECTORY PARAMETERS and REGEXP."
  (let* ((prefix-key mum-search--prefix-key)
		 (split " ")
		 (cmd mum-search--command))
	(setq cmd (concat command split (or parameters mum-search--default-parameters) split prefix-key split keyword split directory))
	(when (memq system-type '(cygwin windows-nt ms-dos))
      (setq cmd (encode-coding-string cmd locale-coding-system)))
	cmd))

(defun mum-search--build-result-buffer-headerline (keyword directory result)
  "KEYWORD DIRECTORY and RESULT build result buffer headerline."
  (concat (propertize (format "mum serch: ")
					  'face 'mum-search--success-face)
		  (propertize (format "[keyword | '")
					  'face 'mum-search--success-face)
		  (propertize (format "%s" keyword)
					  'face 'mum-search--info-face)
		  (propertize (format "'] ")
					  'face 'mum-search--success-face)
		  (propertize (format "[dir | '")
					  'face 'mum-search--success-face)
		  (propertize (format "%s" directory)
					  'face 'mum-search--info-face)
		  (propertize (format "'] ")
					  'face 'mum-search--success-face)
		  (propertize (format "[result | '")
					  'face 'mum-search--success-face)
		  (propertize (format "%s" result)
					  'face 'mum-search--warning-face)
		  (propertize (format "'] ")
					  'face 'mum-search--success-face)))

(defun mum-search--result-filter ()
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
		  (replace-match (concat (propertize (match-string 1) 'face nil 'font-lock-face 'mum-search--file-face)) t t))
        (goto-char finish)
        (while (re-search-forward "\033\\[[0]*m\033\\[[3]*1m\033\\[[3]*1m\\(.*?\\)\033\\[[0]*m" cursor 1)
		  (replace-match (propertize (match-string 1) 'face nil 'font-lock-face 'mum-search--keyword-face) t t)
		  (setq mum-search--result-total (+ mum-search--result-total 1))
		  (plist-put mum-search--current-search-info :result mum-search--result-total)
		  (setq-local header-line-format (mum-search--build-result-buffer-headerline (plist-get mum-search--current-search-info :keyword)
																					 (plist-get mum-search--current-search-info :directory)
																					 (plist-get mum-search--current-search-info :result))))
        (goto-char finish)
        (while (re-search-forward "\033\\[[0-9;]*[0mK]" cursor 1) (replace-match "" t t))))))

(defun mum-search--match-result-file ()
  "Match result file."
  (save-excursion
    (search-backward-regexp mum-search--regexp-file nil t)
    (string-remove-suffix "\n" (thing-at-point 'line))))

(defun mum-search--match-result-line ()
  "Match result line."
  (beginning-of-line)
  (string-to-number (thing-at-point 'symbol)))

(defun mum-search--match-result-column ()
  "Match result column."
  (search-forward ":")
  (string-to-number (thing-at-point 'symbol)))

(defun mum-search--match-result-buffer (path)
  "Match result buffer with PATH."
  (catch 'find-match
    (dolist (buffer (buffer-list))
      (when (string-equal (buffer-file-name buffer) path)
        (throw 'find-match buffer)))
    nil))

(defun mum-search--goto-column (column)
  "Goto line COLUMN."
  (let ((scan-column 0)
        (first-char-point (point)))
    (while (> column scan-column)
      (forward-char 1)
      (setq scan-column (string-bytes (buffer-substring first-char-point (point)))))
    (backward-char 1)))

(defun mum-search--goto-point (line column)
  "Goto find buffer LINE and COLUMN."
  (goto-char (point-min))
  (forward-line (1- line))
  (beginning-of-line)
  (mum-search--goto-column column))

(defun mum-search--flash-line ()
  "Flash line."
  (let ((pulse-iterations 1)
        (pulse-delay mum-search--flash-line-delay))
    (pulse-momentary-highlight-one-line (point) 'mum-search--flash-line-face)))

(defun mum-search--find-file (&optional refind)
  "Find result file with REFIND."
  (interactive)
  (let* ((file (mum-search--match-result-file))
		 (line (mum-search--match-result-line))
		 (column (mum-search--match-result-column)))
	(save-excursion
	  (let ((inhibit-message t))
		(mum-search--goto-column column)
		(other-window 1)
		(when file
		  (find-file file)
		  (cond (t (mum-search--goto-point line column))))
		(mum-search--flash-line)))
	(when refind
	  (let* ((buffer (get-buffer mum-search--result-buffer-name))
			 (window (get-buffer-window buffer)))
		(if buffer
			(select-window window)
		  (split-window)
		  (other-window 1)
		  (switch-to-buffer buffer))))))

(defun mum-search--find-file-after-back ()
  "Find file after back to result buffer."
  (interactive)
  (mum-search--find-file t))

(defun mum-search--engine (&optional keyword directory parameters command)
  "Execute COMMAND to Search KEYWORD in DIRECTORY with PARAMETERS."
  (let* ((dir (or directory (mum-search--read-directory)))
		 (key (or keyword (mum-search--read-keyword)))
		 (cmd (or command mum-search--command))
		 (cmd-str (mum-search--build-command key dir parameters cmd))
		 (buffer (mum-search--make-result-buffer)))
	(when (bufferp buffer)
	  (setq mum-search--result-total 0
			mum-search--current-search-info nil)
	  (with-current-buffer buffer
		(compilation-start cmd-str 'mum-search-result-mode)
		(setq mum-search--current-search-info (list :keyword key :directory dir :command cmd :parameters parameters :result mum-search--result-total))
		(setq header-line-format (mum-search--build-result-buffer-headerline key dir mum-search--result-total))
		(pop-to-buffer buffer)
		(goto-char (point-min))))))

(defun mum-search--rg ()
  "Rg search."
  (interactive)
  (mum-search--engine))

(defun mum-search--process-setup-function (&rest _args)
  "Process setup function format input message."
  ;; `compilation-exit-message-function' (lambda (_process-status exit-status msg) (cons msg exit-status))
  (let* ((buffer (get-buffer mum-search--result-buffer-name)))
	(set (make-local-variable 'compilation-exit-message-function)
		 (lambda (_process-status exit-status msg)
		   (when (eq _process-status 'exit)
			 (cond ((and (zerop exit-status) (buffer-modified-p))
					(setq msg (format "(keyword '%s' matched %s)" (plist-get mum-search--current-search-info :keyword) mum-search--result-total)))))
		   (cons msg exit-status)))))

(defun mum-search--highlight-result ()
  "Hightlight search result keywords."
  (font-lock-add-keywords nil
						  '(("^rg\\s-.*" . 'mum-search--command-face)
							("^\\([1-9][0-9]*\\)\\(:\\)\\([1-9][0-9]*\\)\\(:\\)" 1 'mum-search--line-number-face)
							("^\\([1-9][0-9]*\\)\\(:\\)\\([1-9][0-9]*\\)\\(:\\)" 2 'mum-search--position-face)
							("^\\([1-9][0-9]*\\)\\(:\\)\\([1-9][0-9]*\\)\\(:\\)" 3 'mum-search--line-number-face)
							("^\\([1-9][0-9]*\\)\\(:\\)\\([1-9][0-9]*\\)\\(:\\)" 4 'mum-search--position-face)
							("^[/\\~].*\\|^[a-z]:.*" . 'mum-search--file-face)))
  (set (make-local-variable 'font-lock-keywords-only) t)
  (font-lock-mode 1))

(define-derived-mode mum-search-result-mode text-mode "mum-search-result"
  "Search result minor mode."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'mum-search-result-mode
		mode-name "mum-search-result-mode")
  (read-only-mode 1)
  (mum-search--highlight-result)
  (use-local-map mum-search--result-keymap)
  (add-hook 'compilation-filter-hook 'mum-search--result-filter nil t)
  (set (make-local-variable 'compilation-process-setup-function) 'mum-search--process-setup-function)
  (run-hooks 'mum-search--result-mode-after-hook))

(defun mum-search--enable ()
  "Enable mode.")

(defun mum-search--disable ()
  "Disable mode.")

;;;###autoload
(define-minor-mode mum-search-mode
  "Mum search minor mode."
  :group 'mum-search
  :keymap mum-search--keymap
  (if mum-search-mode
	  (mum-search--enable)
	(mum-search--disable)))

(provide 'mum-search)
;;; mum-search.el ends here
