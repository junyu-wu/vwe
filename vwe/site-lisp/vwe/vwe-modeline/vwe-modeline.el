;;; vwe-modeline--.el --- Mu mode line     -*- lexical-binding: t; -*-

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


;;; Code:

(require 'seq)
(require 'subr-x)

(defgroup vwe-modeline nil
  "Vwe mode line."
  :prefix "vwe-modeline--"
  :group 'mode-line)

(defvar vwe-modeline-mode-p
  nil
  "Mode.")

(defcustom vwe-modeline--buffer-filter-list
  nil
  "Buffer filter list."
  :group 'vwe-modeline
  :type 'string)

(defcustom vwe-modeline--segments
  '((vwe-modeline--segment-active-label
	 vwe-modeline--segment-space
	 vwe-modeline--segment-winnum
	 vwe-modeline--segment-space
	 vwe-modeline--segment-modified
	 vwe-modeline--segment-major-mode
     vwe-modeline--segment-buffer-name
	 vwe-modeline--segment-remote-host
     vwe-modeline--segment-process
	 vwe-modeline--segment-vc
	 vwe-modeline--segment-space
	 vwe-modeline--segment-vc-diff
	 vwe-modeline--segment-space
	 vwe-modeline--segment-location
	 vwe-modeline--segment-space
	 vwe-modeline--segment-symbol-count-info
	 vwe-modeline--segment-space
	 vwe-modeline--segment-buffer-counter)
    (vwe-modeline--segment-space
	 vwe-modeline--segment-conda-env
	 vwe-modeline--segment-pyenv-env
	 vwe-modeline--segment-rvm-env
	 vwe-modeline--segment-rbenv-env
	 vwe-modeline--segment-space
	 vwe-modeline--segment-lsp
	 vwe-modeline--segment-space
	 vwe-modeline--segment-flycheck
	 vwe-modeline--segment-space
     vwe-modeline--segment-input-method
	 vwe-modeline--segment-indent-tab
	 vwe-modeline--segment-separator
	 vwe-modeline--segment-indent-spc
	 vwe-modeline--segment-space
     vwe-modeline--segment-eol
	 vwe-modeline--segment-separator
     vwe-modeline--segment-encoding
     vwe-modeline--segment-misc-info
	 vwe-modeline--segment-space
	 vwe-modeline--segment-time
	 vwe-modeline--segment-space
	 vwe-modeline--segment-end-label))
  "Vwe modeline default segments."
  :type '(list (repeat :tag "left" function)
               (repeat :tag "right" function))
  :group 'vwe-modeline)

(defcustom vwe-modeline--separator
  "^"
  "Separator."
  :type 'string
  :group 'vwe-modeline)

(defcustom vwe-modeline--indent-alist
  '((apache-mode apache-indent-level)
    (awk-mode c-basic-offset)
    (bpftrace-mode c-basic-offset)
    (c++-mode c-basic-offset)
    (c-mode c-basic-offset)
    (cmake-mode cmake-tab-width)
    (coffee-mode coffee-tab-width)
    (cperl-mode cperl-indent-level)
    (crystal-mode crystal-indent-level)
    (csharp-mode c-basic-offset)
    (css-mode css-indent-offset)
    (d-mode c-basic-offset)
    (emacs-lisp-mode lisp-indent-offset)
    (enh-ruby-mode enh-ruby-indent-level)
    (erlang-mode erlang-indent-level)
    (ess-mode ess-indent-offset)
    (f90-mode f90-associate-indent
              f90-continuation-indent
              f90-critical-indent
              f90-do-indent
              f90-if-indent
              f90-program-indent
              f90-type-indent)
    (feature-mode feature-indent-offset
                  feature-indent-level)
    (fsharp-mode fsharp-continuation-offset
                 fsharp-indent-level
                 fsharp-indent-offset)
    (groovy-mode groovy-indent-offset)
    (haskell-mode haskell-indent-spaces
                  haskell-indent-offset
                  haskell-indentation-layout-offset
                  haskell-indentation-left-offset
                  haskell-indentation-starter-offset
                  haskell-indentation-where-post-offset
                  haskell-indentation-where-pre-offset
                  shm-indent-spaces)
    (haxor-mode haxor-tab-width)
    (idl-mode c-basic-offset)
    (jade-mode jade-tab-width)
    (java-mode c-basic-offset)
    (js-mode js-indent-level)
    (js-jsx-mode js-indent-level
                 sgml-basic-offset)
    (js2-mode js2-basic-offset)
    (js2-jsx-mode js2-basic-offset
                  sgml-basic-offset)
    (js3-mode js3-indent-level)
    (json-mode js-indent-level)
    (julia-mode julia-indent-offset)
    (kotlin-mode kotlin-tab-width)
    (latex-mode tex-indent-basic)
    (lisp-mode lisp-indent-offset)
    (livescript-mode livescript-tab-width)
    (lua-mode lua-indent-level)
    (matlab-mode matlab-indent-level)
    (mips-mode mips-tab-width)
    (mustache-mode mustache-basic-offset)
    (nasm-mode nasm-basic-offset)
    (nginx-mode nginx-indent-level)
    (nxml-mode nxml-child-indent)
    (objc-mode c-basic-offset)
    (octave-mode octave-block-offset)
    (perl-mode perl-indent-level)
    (php-mode c-basic-offset)
    (pike-mode c-basic-offset)
    (ps-mode ps-mode-tab)
    (pug-mode pug-tab-width)
    (puppet-mode puppet-indent-level)
    (python-mode python-indent-offset)
    (ruby-mode ruby-indent-level)
    (rust-mode rust-indent-offset)
    (rustic-mode rustic-indent-offset)
    (scala-mode scala-indent:step)
    (scss-mode css-indent-offset)
    (sgml-mode sgml-basic-offset)
    (sh-mode sh-basic-offset
             sh-indentation)
    (slim-mode slim-indent-offset)
    (sml-mode sml-indent-level)
    (tcl-mode tcl-indent-level
              tcl-continued-indent-level)
    (terra-mode terra-indent-level)
    (typescript-mode typescript-indent-level)
    (verilog-mode verilog-indent-level
                  verilog-indent-level-behavioral
                  verilog-indent-level-declaration
                  verilog-indent-level-module
                  verilog-cexp-indent
                  verilog-case-indent)
    (web-mode web-mode-attr-indent-offset
              web-mode-attr-value-indent-offset
              web-mode-code-indent-offset
              web-mode-css-indent-offset
              web-mode-markup-indent-offset
              web-mode-sql-indent-offset
              web-mode-block-padding
              web-mode-script-padding
              web-mode-style-padding)
    (yaml-mode yaml-indent-offset))
  "Indent alist."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'vwe-modeline)

(defface vwe-modeline--space-face
  '((t (:inherit 'default)))
  "Space face.")

(defface vwe-modeline--default-face
  '((t (:foreground "#B0BEC5"
					:weight bold)))
  "Default face.")

(defface vwe-modeline--major-mode-face
  '((t (:foreground "cyan"
					:weight bold)))
  "Default face.")

(defface vwe-modeline--label-face
  '((t (:background "DarkOrange")))
  "Default face.")

(defface vwe-modeline--misc-face
  '((t (:inherit 'shadow)))
  "Misc face.")

(defface vwe-modeline--modified-face
  '((t (:foreground "red" :background "yellow")))
  "Face for the 'modified' indicator symbol in the mode-line.")

(defface vwe-modeline--info-face
  '((t (:foreground "DarkOrange")))
  "Face for generic status indicators in the mode-line.")

(defface vwe-modeline--success-face
  '((t (:inherit 'success)))
  "Face used for success status indicators in the mode-line.")

(defface vwe-modeline--warning-face
  '((t (:inherit 'warning)))
  "Face for warning status indicators in the mode-line.")

(defface vwe-modeline--error-face
  '((t (:foreground "DarkRed")))
  "Face for error status indicators in the mode-line.")

(defface vwe-modeline--mouse-face
  '((t (:foreground "LightSeaGreen")))
  "Face for error status indicators in the mode-line.")

(defface vwe-modeline--read-only-face
  '((t (:background "#434C5E" :foreground "DarkOrange" :weight bold)))
  "Read only face.")

(defface vwe-modeline--read-and-write-face
  '((t (:background "#434C5E" :foreground "SpringGreen" :weight bold)))
  "Read and write face.")

(defvar vwe-modeline--default-format
  mode-line-format
  "Store default `mode-line-format' value.")

(defvar vwe-modeline--init
  '((:eval
     (vwe-modeline--format (car vwe-modeline--segments)
						   (cadr vwe-modeline--segments))))
  "Vwe modeline init.")

(defvar vwe-modeline--segment-encoding-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1]
	  (lambda (e)
		(interactive "e")
		(with-selected-window (posn-window (event-start e))
		  (when (and enable-multibyte-characters
					 buffer-file-coding-system)
			(describe-coding-system buffer-file-coding-system)))))
    (define-key map [mode-line mouse-3]
      (lambda (e)
		(interactive "e")
		(with-selected-window (posn-window (event-start e))
		  (call-interactively #'set-buffer-file-coding-system))))
    (purecopy map))
  "Local keymap for coding-system part.")

(defun vwe-modeline--make-mouse-map (mouse function)
  "Return a keymap with single entry for mouse key MOUSE on the mode line.
MOUSE is defined to run function FUNCTION with no args in the buffer
corresponding to the mode line clicked."
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'mode-line mouse) function)
    map))

(defun vwe-modeline--segment-space ()
  "Space."
  " ")

(defun vwe-modeline--segment-active-label ()
  "Active label."
  (when (vwe-modeline--active?)
	(propertize "✔" 'face 'vwe-modeline--label-face)))

(defun vwe-modeline--segment-end-label ()
  "Active label."
  (when (vwe-modeline--active?)
	(propertize "✘"
				'face 'vwe-modeline--default-face
				'help-echo (format "disable vwe modeline")
				'mouse-face 'vwe-modeline--mouse-face
				'local-map (purecopy (vwe-modeline--make-mouse-map
									  'mouse-1
									  (lambda (event)
										(interactive "e")
										(with-selected-window (posn-window (event-start event))
										  (vwe-modeline-disable))))))))

(defun  vwe-modeline--segment-separator ()
  "Space."
  (propertize vwe-modeline--separator 'face 'vwe-modeline--default-face))

(defun vwe-modeline--segment-modified ()
  "Displays a color-coded buffer modification/read-only indicator in the mode-line."
  (if (not (string-match-p "\\*.*\\*" (buffer-name)))
      (let* ((read-only (and buffer-read-only (buffer-file-name)))
             (modified (buffer-modified-p)))
        (propertize
         (if read-only "RO" (if modified "MD" "RW"))
         'face `(:inherit
                 ,(if modified 'vwe-modeline--modified-face
                    (if read-only 'vwe-modeline--read-only-face
                      'vwe-modeline--read-and-write-face)))
         'help-echo (format
                     "Buffer is %s and %smodified\nmouse-1: Toggle read-only status."
                     (if read-only "read-only" "writable")
                     (if modified "" "not "))
         'local-map (purecopy (vwe-modeline--make-mouse-map
                               'mouse-1
                               (lambda (event)
                                 (interactive "e")
                                 (with-selected-window (posn-window (event-start event))
                                   (read-only-mode 'toggle)))))
         'mouse-face 'vwe-modeline--mouse-face))))

(defun vwe-modeline--segment-buffer-name ()
  "Displays the name of the current buffer in the mode-line."
  (propertize " %b" 'face 'vwe-modeline--default-face))

(defun vwe-modeline--segment-location ()
  "Display the current cursor location."
  `(,(propertize (format "L%s:C%s^P%s"
						 (format-mode-line "%l")
						 (format-mode-line "%c")
						 (format-mode-line "%p"))
				 'face 'vwe-modeline--default-face)
	,(if (region-active-p)
		 (propertize (format "[M+%s]"
							 (apply #'+ (mapcar
										 (lambda (pos)
										   (- (cdr pos)
											  (car pos)))
										 (region-bounds))))
					 'font-lock-face 'vwe-modeline--info-face))))

(defun vwe-modeline--segment-make-flycheck-info ()
  "Make checker info."
  (when (boundp 'flycheck-current-errors)
	(let ((info 0) (warning 0) (error 0))
	  (mapc (lambda (item)
			  (let ((count (cdr item)))
				(pcase (flycheck-error-level-compilation-level (car item))
				  (0 (cl-incf info count))
				  (1 (cl-incf warning count))
				  (2 (cl-incf error count)))))
			(flycheck-count-errors flycheck-current-errors))
	  `((info . ,info) (warning . ,warning) (error . ,error)))))

(defun vwe-modeline--segment-flycheck-text (&optional status)
  "Checker text via STATUS."
  (let* ((text))
	(let-alist (vwe-modeline--segment-make-flycheck-info)
	  (setq text (format "%s%s%s%s%s"
						 (propertize (concat "I" (number-to-string .info))
									 'face 'vwe-modeline--info-face)
						 (vwe-modeline--segment-separator)
						 (propertize (concat "W" (number-to-string .warning))
									 'face 'vwe-modeline--warning-face)
						 (vwe-modeline--segment-separator)
						 (propertize (concat "E" (number-to-string .error))
									 'face 'vwe-modeline--error-face))))
	(propertize text
				'help-echo (concat "mouse-1: Show all errors\nmouse-3: Next error"
								   (if (featurep 'mwheel)
									   "\nwheel-up/wheel-down: Previous/next error"))
				'mouse-face 'vwe-modeline--mouse-face
				'local-map (let ((map (make-sparse-keymap)))
							 (define-key map [mode-line mouse-1] #'flycheck-list-errors)
							 (define-key map [mode-line mouse-3] #'flycheck-next-error)
							 (when (featurep 'mwheel)
							   (define-key map (vector 'mode-line mouse-wheel-down-event)
								 (lambda (event)
								   (interactive "e")
								   (with-selected-window (posn-window (event-start event))
									 (flycheck-previous-error 1))))
							   (define-key map (vector 'mode-line mouse-wheel-up-event)
								 (lambda (event)
								   (interactive "e")
								   (with-selected-window (posn-window (event-start event))
									 (flycheck-next-error 1))))
							   map)))))

(defun vwe-modeline--segment-flycheck ()
  "Checker."
  (when (bound-and-true-p flycheck-mode)
	(vwe-modeline--segment-flycheck-text)))

(defun vwe-modeline--segment-vc ()
  "Display color-coded version control information in the mode-line."
  (propertize (format "%s" (format-mode-line '(vc-mode vc-mode)))
			  'face 'vwe-modeline--default-face))

(defun vwe-modeline--segment-vc-diff ()
  "Display vc diff."
  (let* ((status (vc-state (buffer-name))))
	(when (not (equal status 'up-to-date))
	  (propertize (format "D:%s" (if status status "?"))
				  'face 'vwe-modeline--info-face
				  'help-echo (format "diff: %s" status)
				  'local-map (purecopy
							  (vwe-modeline--make-mouse-map 'mouse-1 (lambda ()
																	   (interactive)
																	   (magit-diff-dwim))))
				  'mouse-face 'vwe-modeline--mouse-face))))

(defun vwe-modeline--segment-remote-host ()
  "Hostname for remote buffers."
  (when default-directory
	(when-let ((host (file-remote-p default-directory 'host)))
	  (propertize (concat "R:" (system-name) "@" host)
				  'face 'vwe-modeline--default-face))))

(defun vwe-modeline--segment-symbol-count-info ()
  "Return Symbol Total And Current Symbol Index."
  (let* ((symbol (thing-at-point 'symbol))
		 (cur-bound (bounds-of-thing-at-point 'symbol)))
	(when (and symbol cur-bound)
	  (let*((cur (point))
			(cur-start (car cur-bound))
			(cur-end (cdr cur-bound))
			(cur-length (- cur-end cur-start))
			(total 0)
			(curindex 0))
		(save-excursion
		  (save-restriction
			(when symbol
			  (goto-char (point-min))
			  (setq symbol (concat "\\_<" (regexp-quote symbol) "\\_>"))
			  (while (re-search-forward symbol nil t)
				(let* ((bound (bounds-of-thing-at-point 'symbol))
					   (end (cdr bound))
					   (start (car bound))
					   (len (- end start)))
				  (if (= cur-length len)
					  (progn
						(setq total (+ total 1))
						(if (and (>= cur start) (<= cur end))
							(setq curindex total))))))
			  (propertize (format "T%d:C%d" total curindex)
						  'face 'vwe-modeline--default-face
						  'help-echo (format "total %d\ncurrent %d" total curindex)
						  'mouse-face 'vwe-modeline--mouse-face))))))))

(defun vwe-modeline--segment-indent-tab ()
  "Displays the indentation information."
  (propertize (format "T%s" tab-width)
			  'face 'vwe-modeline--default-face
			  'help-echo (format "tab width %s" tab-width)
			  'mouse-face 'vwe-modeline--mouse-face))

(defun vwe-modeline--segment-indent-spc ()
  "Display the indentation information."
  (let ((spc)
		(indent
         (seq-find (lambda (var)
                     (and var (boundp var) (symbol-value var)))
				   (cdr (assoc major-mode vwe-modeline--indent-alist)) nil)))
	(if indent (setq spc (symbol-value indent)) (setq spc tab-width))
	(propertize (format "S%s" spc)
				'face 'vwe-modeline--default-face
				'help-echo (format "indent offset %s" spc)
				'mouse-face 'vwe-modeline--mouse-face)))

(defun vwe-modeline--segment-eol ()
  "Display the EOL style of the current buffer in the mode-line."
  (let* ((eol (coding-system-eol-type buffer-file-coding-system))
		 (mnemonic (pcase eol
					 ('0 "LF")
					 ('1 "CRLF")
					 ('2 "CR")
					 (_ "")))
		 (desc (pcase eol
				 ('0 "unix/linux")
				 ('1 "DOS")
				 ('2 "mac")
				 (_ "undecided"))))
	(propertize
	 mnemonic
	 'face 'vwe-modeline--default-face
	 'help-echo (format "eol: %s\nmouse-1: cycle" desc)
	 'local-map (purecopy
				 (vwe-modeline--make-mouse-map
				  'mouse-1
				  (lambda (event)
					(interactive "e")
					(with-selected-window (posn-window (event-start event))
					  (let ((eol (coding-system-eol-type buffer-file-coding-system)))
						(set-buffer-file-coding-system
						 (cond ((eq eol 0) 'dos) ((eq eol 1) 'mac) (t 'unix))))))))
	 'mouse-face 'vwe-modeline--mouse-face)))

(defun vwe-modeline--segment-encoding ()
  "Display encoding of the buffer in mode-line."
  (let* ((sys (coding-system-plist buffer-file-coding-system))
		 (encoding))
	(cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
		   (setq encoding "UTF-8"))
		  (t
		   (setq encoding (upcase (symbol-name (plist-get sys :name))))))
	(propertize
	 encoding
	 'face 'vwe-modeline--default-face
	 'help-echo (format "encoding: %s" encoding)
	 'local-map (purecopy
				 (vwe-modeline--make-mouse-map
				  'mouse-1 'revert-buffer-with-coding-system))
	 'mouse-face 'vwe-modeline--mouse-face)))

(defun vwe-modeline--segment-misc-info ()
  "Displays the current value of `mode-line-misc-info' in the mode-line."
  (let ((misc-info (string-trim (format-mode-line mode-line-misc-info 'vwe-modeline--misc-face))))
	(unless (string= misc-info "")
	  (concat " " misc-info))))

(defun vwe-modeline--segment-input-method ()
  "Displays the input-method of the buffer in the mode-line."
  `(""
	(current-input-method
	 (:propertize (" " current-input-method-title)
				  help-echo (format
							 "Current input method: %s\nmouse-1: Describe current input method"
							 current-input-method)
				  local-map ,(purecopy
							  (vwe-modeline--make-mouse-map
							   'mouse-1
							   (lambda (e)
								 (interactive "e")
								 (with-selected-window (posn-window (event-start e))
								   (describe-current-input-method)))))
				  mouse-face 'vwe-modeline--mouse-face))))

(defun vwe-modeline--segment-minor-modes ()
  "Displays current minor modes in mode-line."
  (replace-regexp-in-string "%" "%%%%"
							(format-mode-line minor-mode-alist)
							t t))

(defun vwe-modeline--segment-process ()
  "Displays current `mode-line-process' in the mode-line."
  (when mode-line-process
	(propertize (string-trim (format-mode-line mode-line-process))
				'face 'vwe-modeline--default-face)))

(defun vwe-modeline--segment-major-mode ()
  "Displays current major mode in mode-line."
  (propertize
   (concat " "
		   (or (and (boundp 'delighted-modes)
					(cadr (assq major-mode delighted-modes)))
			   (format-mode-line mode-name)))
   'face 'vwe-modeline--major-mode-face))

(defun vwe-modeline--buffer-list (regexp &optional not-i)
  "Find buffer list of REGEXP.
NOT-I is include curretn buffer."
  (if regexp
	  (progn
		(seq-filter 'stringp
					(mapcar
					 (lambda (item)
					   (if (string-match regexp (buffer-name item))

						   (if not-i
							   (format "%s" (buffer-name item))
							 (unless (equal item (current-buffer))
							   (format "%s" (buffer-name item))))))
					 (buffer-list))))
	(buffer-list)))

(defun vwe-modeline--segment-buffer-counter ()
  "Displays current open buffer total."
  (let* ((buffer (length (vwe-modeline--buffer-list "^[^\s*]" t)))
		 (tmp-buffer (length (vwe-modeline--buffer-list "^*" t))))
	(propertize (format "B%d^T%d" buffer tmp-buffer)
				'face 'vwe-modeline--default-face
				'help-echo (format "open buffer %d temp buffer %d" buffer tmp-buffer))))

(defun vwe-modeline--segment-time ()
  "Display current data and time."
  (propertize (format-time-string "%y-%m-%d %H:%M %a")
			  'face 'vwe-modeline--default-face
			  'help-echo (format "current time zone %s" (current-time-zone))
			  'mouse-face 'vwe-modeline--mouse-face))

(defun vwe-modeline--lsp-state ()
  "Update lsp state."
  (let* ((workspaces (lsp-workspaces)))
	(if workspaces
		(concat "L^"
				(string-join
				 (mapcar (lambda (w) (format "[%s]" (if (lsp--workspace-print w)
														(lsp--workspace-print w)
													  "none")))
						 workspaces)))
	  (format "%s" "L^[none]"))))

(defun vwe-modeline--segment-lsp ()
  "Display lsp status."
  (let ((lsp-info (when (bound-and-true-p lsp-mode) (vwe-modeline--lsp-state))))
    (when lsp-info
	  (propertize lsp-info
				  'face 'vwe-modeline--default-face
				  'help-echo (format "%s" (if lsp-info
											  (concat "LSP Connected "
													  lsp-info
													  "C-mouse-1: Switch to another workspace folder"
													  "mouse-1: Describe current session"
													  "mouse-2: Quit server"
													  "mouse-3: Reconnect to server")
											"LSP Disconnected\nmouse-1: Reload to start server"))
				  'mouse-face 'vwe-modeline--mouse-face
				  'local-map (let ((map (make-sparse-keymap)))
							   (if lsp-info
								   (progn
                                     (define-key map [mode-line C-mouse-1]
									   #'lsp-workspace-folders-open)
                                     (define-key map [mode-line mouse-1]
									   #'lsp-describe-session)
                                     (define-key map [mode-line mouse-2]
									   #'lsp-workspace-shutdown)
                                     (define-key map [mode-line mouse-3]
									   #'lsp-workspace-restart))
                                 (progn
								   (define-key map [mode-line mouse-1]
                                     (lambda ()
									   (interactive)
									   (ignore-errors (revert-buffer t t))))))
							   map)))))

(defun vwe-modeline--segment-debug ()
  "Display debug."
  ;; TODO
  )

(defun vwe-modeline--segment-repl ()
  "Display repl."
  ;; TODO
  )

(defun vwe-modeline--segment-conda-env ()
  "Display current python anaconda env."
  (when (eq major-mode 'python-mode)
	(let* ((conda-env (if conda-env-current-name
						  conda-env-current-name "non env")))
	  (when conda-env
		(propertize (format "C^[%s]" conda-env)
					'face 'vwe-modeline--default-face
					'help-echo (format "anaconda env %s" conda-env)
					'mouse-face 'vwe-modeline--mouse-face)))))

(defun vwe-modeline--segment-pyenv-env ()
  "Display current python pyenv env."
  (when (eq major-mode 'python-mode)
	(let* ((pyenv-env (getenv "PYENV_VERSION")))
	  (when pyenv-env
		(propertize (format "Py^[%s]" pyenv-env)
					'face 'vwe-modeline--default-face
					'help-echo (format "anaconda env %s" pyenv-env)
					'mouse-face 'vwe-modeline--mouse-face)))))

(defun vwe-modeline--segment-rvm-env ()
  "Display current ruby and gem env."
  (when (and (eq major-mode 'ruby-mode) (fboundp 'rvm--current-ruby) (rvm-working-p))
	(propertize (format "R^[v:%s|g:%s]" rvm-current-ruby rvm--current-gemset)
				'face 'vwe-modeline--default-face
				'help-echo (format "Ruby version %s gem %s" rvm-current-ruby rvm--current-gemset)
		  		'mouse-face 'vwe-modeline--mouse-face)))

(defun vwe-modeline--segment-rbenv-env ()
  "Display current ruby and gem env."
  (when (and (eq major-mode 'ruby-mode) (bound-and-true-p rbenv-version-environment-variable))
	(propertize (format "Rb^[%s]" (rbenv--active-ruby-version))
				'face 'vwe-modeline--default-face
				'help-echo (format "Ruby version %s" (rbenv--active-ruby-version))
		  		'mouse-face 'vwe-modeline--mouse-face)))

(defun vwe-modeline--segment-winnum ()
  "Display current windows number in mode-line."
  (let ((num (cond
			  ((bound-and-true-p ace-window-display-mode)
			   (aw-update)
			   (window-parameter (selected-window) 'ace-window-path))
			  ((bound-and-true-p winum-mode)
			   (setq winum-auto-setup-mode-line nil)
			   (winum-get-number-string))
			  ((bound-and-true-p window-numbering-mode)
			   (window-numbering-get-number-string))
			  (t ""))))
    (if (and (< 0 (length num))
             (< (if (active-minibuffer-window) 2 1) ; exclude minibuffer
                (length (cl-mapcan
                         (lambda (frame)
						   ;; Exclude child frames
						   (unless (and (fboundp 'frame-parent)
                                        (frame-parent frame))
                             (window-list)))
                         (visible-frame-list)))))
        (propertize (format "%s" num)
                    'face 'vwe-modeline--info-face)
	  (propertize (format "-")
				  'face 'vwe-modeline--info-face
				  'help-echo (format "only window")))))

(defun vwe-modeline--format (left-segments right-segments)
  "Make modeline string of `window-width' length.
Containing LEFT-SEGMENTS and RIGHT-SEGMENTS."
  (let* ((left (vwe-modeline--format-segments left-segments))
		 (right (vwe-modeline--format-segments right-segments))
		 (reserve (length right)))
	(concat left (propertize " " 'display `((space :align-to (- right ,reserve)))) right)))

(defun vwe-modeline--format-segments (segments)
  "Make modeline string from SEGMENTS."
  (format-mode-line (mapcar (lambda (segment) `(:eval (,segment)))
							segments)))

(defun vwe-modeline--get-current-window (&optional frame)
  "Get the current window from FRAME."
  (if (and (fboundp 'frame-parent) (frame-parent frame))
	  (frame-selected-window (frame-parent frame))
    (frame-selected-window frame)))

(defvar vwe-modeline--current-window
  (vwe-modeline--get-current-window)
  "Current window.")

(defun vwe-modeline--active? ()
  "Whether is an active window."
  (and vwe-modeline--current-window
	   (eq (vwe-modeline--get-current-window) vwe-modeline--current-window)))

(defun vwe-modeline--set-selected-window (&rest _)
  "Set `vwe-modeline--current-window' appropriately."
  (when-let ((win (vwe-modeline--get-current-window)))
    (unless (or (minibuffer-window-active-p win)
                (and (bound-and-true-p lv-wnd) (eq lv-wnd win)))
	  (setq vwe-modeline--current-window win))))

(defun vwe-modeline--unset-selected-window ()
  "Unset `vwe-modeline--current-window' appropriately."
  (setq vwe-modeline--current-window nil))

(defun vwe-modeline--init-hook (&optional del?)
  "Init add hook.
DEL is add or delete?"
  (if del?
	  (progn
		(remove-hook 'flycheck-status-changed-functions #'vwe-modeline--segment-flycheck-text)
		(remove-hook 'flycheck-mode-hook #'vwe-modeline--segment-flycheck-text)

		(remove-hook 'lsp-before-initialize-hook #'vwe-modeline--lsp-state)
		(remove-hook 'lsp-after-initialize-hook #'vwe-modeline--lsp-state)
		(remove-hook 'lsp-after-uninitialized-functions #'vwe-modeline--lsp-state)
		(remove-hook 'lsp-before-open-hook #'vwe-modeline--lsp-state)
		(remove-hook 'lsp-after-open-hook #'vwe-modeline--lsp-state))

	(add-hook 'window-configuration-change-hook #'vwe-modeline--set-selected-window)
	(add-hook 'buffer-list-update-hook #'vwe-modeline--set-selected-window)
	(add-hook 'after-make-frame-functions #'vwe-modeline--set-selected-window)
	(add-hook 'delete-frame-functions #'vwe-modeline--set-selected-window)
	(add-hook 'exwm-workspace-switch-hook #'vwe-modeline--set-selected-window)
	(advice-add #'handle-switch-frame :after #'vwe-modeline--set-selected-window)
	(with-no-warnings
	  (if (boundp 'after-focus-change-function)
		  (progn
			(defun vwe-modeline--refresh-frame ()
			  (setq vwe-modeline--current-window nil)
			  (cl-loop for frame in (frame-list)
					   if (eq (frame-focus-state frame) t)
					   return (setq vwe-modeline--current-window
									(vwe-modeline--get-current-window frame)))
			  (force-mode-line-update))
			(add-function :after after-focus-change-function #'vwe-modeline--refresh-frame))
		(progn
		  (add-hook 'focus-in-hook #'vwe-modeline--set-selected-window)
		  (add-hook 'focus-out-hook #'vwe-modeline--unset-selected-window))))

	(add-hook 'flycheck-status-changed-functions #'vwe-modeline--segment-flycheck-text)
	(add-hook 'flycheck-mode-hook #'vwe-modeline--segment-flycheck-text)

	(add-hook 'lsp-before-initialize-hook #'vwe-modeline--lsp-state)
	(add-hook 'lsp-after-initialize-hook #'vwe-modeline--lsp-state)
	(add-hook 'lsp-after-uninitialized-functions #'vwe-modeline--lsp-state)
	(add-hook 'lsp-before-open-hook #'vwe-modeline--lsp-state)
	(add-hook 'lsp-after-open-hook #'vwe-modeline--lsp-state)

	(advice-add #'winum--install-mode-line :override #'ignore)
	(advice-add #'winum--clear-mode-line :override #'ignore)))

(defun vwe-modeline-enable ()
  "Mode line enable."
  (interactive)
  (vwe-modeline--init-hook)
  (setq-default mode-line-format '(:eval vwe-modeline--init)))

(defun vwe-modeline-disable (&optional defp)
  "Mode line disenable or if DEFP is non-nil reset default modeline."
  (interactive)
  (vwe-modeline--init-hook)
  (setq-default mode-line-format (if defp vwe-modeline--default-format nil)))

(defun vwe-modeline-re-init ()
  "Reset mode line."
  (interactive)
  (vwe-modeline-enable))

(defun vwe-modeline-buffer-hide (&optional filter buffer)
  "Hide current window BUFFER(buffer name) modeline of FILTER(buffer name list)."
  (interactive)
  (unless filter (setq filter (mapcar (lambda(buf) (buffer-name buf)) (buffer-list))))
  (unless buffer (setq buffer (buffer-name (current-buffer))))
  (dolist (window (window-list))
	(with-selected-window window
	  (dotimes (i (length filter))
		(when (equal (nth i filter) buffer)
		  (setq mode-line-format nil))))))

(defun vwe-modeline--hide ()
  "Hide buffer mode line."
  (if vwe-modeline--buffer-filter-list
	  (vwe-modeline-buffer-hide vwe-modeline--buffer-filter-list)
	(vwe-modeline-buffer-hide)))

;;;###autoload
(defun vwe-modeline-buffer-show-modeline ()
  "Show modeline in current buffer."
  (interactive)
  (vwe-modeline--init-hook)
  (setq mode-line-format '(:eval vwe-modeline--init)))

;;;###autoload
(define-minor-mode vwe-modeline-mode
  "Vwe modeline minor mode."
  :init-value nil
  :keymap nil
  :lighter ""
  :group 'vwe-modeline
  :global t
  (if vwe-modeline-mode
	  (progn
		(vwe-modeline-enable)
		(add-hook 'emacs-startup-hook 'vwe-modeline--hide)
		(add-hook 'window-configuration-change-hook 'vwe-modeline--hide))
	(vwe-modeline-disable)
	(remove-hook 'emacs-startup-hook 'vwe-modeline--hide)
	(remove-hook 'window-configuration-change-hook 'vwe-modeline--hide)))

(provide 'vwe-modeline)
;;; vwe-modeline.el ends here
