;;; mum-modeline/.el --- Mu mode line     -*- lexical-binding: t; -*-

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
(require 'seq)
(require 'subr-x)
;;; Code:
(defgroup mum-modeline nil
  "Mum mode line."
  :prefix "mum-modeline/"
  :group 'mode-line)

(defcustom mum-modeline/segments
  '((mum-modeline/segment-active-label
	 mum-modeline/segment-space
	 mum-modeline/segment-winnum
	 mum-modeline/segment-space
	 mum-modeline/segment-modified
	 mum-modeline/segment-major-mode
     mum-modeline/segment-buffer-name
	 mum-modeline/segment-remote-host
     mum-modeline/segment-process
	 mum-modeline/segment-space
     mum-modeline/segment-vc
	 mum-modeline/segment-space
	 mum-modeline/segment-vc-diff
	 mum-modeline/segment-space
	 mum-modeline/segment-position-percent
	 mum-modeline/segment-separator
     mum-modeline/segment-position
	 mum-modeline/segment-space
	 mum-modeline/segment-symbol-count-info)
    (mum-modeline/segment-minor-modes
	 mum-modeline/segment-flycheck
	 mum-modeline/segment-space
     mum-modeline/segment-input-method
	 mum-modeline/segment-indent-tab
	 mum-modeline/segment-separator
	 mum-modeline/segment-indent-spc
	 mum-modeline/segment-space
     mum-modeline/segment-eol
	 mum-modeline/segment-separator
     mum-modeline/segment-encoding
     mum-modeline/segment-misc-info
	 mum-modeline/segment-time))
  "Mum modeline default segments."
  :type '(list (repeat :tag "left" function)
               (repeat :tag "right" function))
  :group 'mum-modeline)

(defcustom mum-modeline/separator
  "^"
  "Separator."
  :type 'string
  :group 'mum-modeline)

(defcustom mum-modeline/indent-alist
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
  :group 'mum-modeline)

(defface mum-modeline/space-face
  '((t (:inherit 'default)))
  "Space face.")

(defface mum-modeline/default-face
  '((t (:foreground "#B0BEC5"
					:weight bold)))
  "Default face.")

(defface mum-modeline/label-face
  '((t (:background "DarkOrange")))
  "Default face.")

(defface mum-modeline/misc-face
  '((t (:inherit 'shadow)))
  "Misc face.")

(defface mum-modeline/modified-face
  '((t (:foreground "red")))
  "Face for the 'modified' indicator symbol in the mode-line.")

(defface mum-modeline/info-face
  '((t (:foreground "DarkOrange")))
  "Face for generic status indicators in the mode-line.")

(defface mum-modeline/success-face
  '((t (:inherit 'success)))
  "Face used for success status indicators in the mode-line.")

(defface mum-modeline/warning-face
  '((t (:inherit 'warning)))
  "Face for warning status indicators in the mode-line.")

(defface mum-modeline/error-face
  '((t (:foreground "DarkRed")))
  "Face for error status indicators in the mode-line.")

(defvar mum-modeline/default-format
  mode-line-format
  "Store default `mode-line-format' value.")

(defvar mum-modeline/init
  '((:eval
     (mum-modeline/format (car mum-modeline/segments)
						  (cadr mum-modeline/segments))))
  "Mum modeline init.")

(defvar mum-modeline/segment-encoding-map
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

(defun mum-modeline/make-mouse-map (mouse function)
  "Return a keymap with single entry for mouse key MOUSE on the mode line.
MOUSE is defined to run function FUNCTION with no args in the buffer
corresponding to the mode line clicked."
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'mode-line mouse) function)
    map))

(defun mum-modeline/segment-space ()
  "Space."
  " ")

(defun mum-modeline/segment-active-label ()
  "Active label."
  (when (mum-modeline/active?)
	(propertize " " 'face 'mum-modeline/label-face)))

(defun  mum-modeline/segment-separator ()
  "Space."
  (propertize mum-modeline/separator 'face 'mum-modeline/default-face))

(defun mum-modeline/segment-modified ()
  "Displays a color-coded buffer modification/read-only indicator in the mode-line."
  (if (not (string-match-p "\\*.*\\*" (buffer-name)))
      (let* ((read-only (and buffer-read-only (buffer-file-name)))
             (modified (buffer-modified-p)))
        (propertize
         (if read-only "o" (if modified "m" "r"))
         'face `(:inherit
                 ,(if modified 'mum-modeline/modified-face
                    (if read-only 'mum-modeline/info-face
                      'mum-modeline/warning-face)))
         'help-echo (format
                     "Buffer is %s and %smodified\nmouse-1: Toggle read-only status."
                     (if read-only "read-only" "writable")
                     (if modified "" "not "))
         'local-map (purecopy (mum-modeline/make-mouse-map
                               'mouse-1
                               (lambda (event)
                                 (interactive "e")
                                 (with-selected-window (posn-window (event-start event))
                                   (read-only-mode 'toggle)))))
         'mouse-face 'mode-line-highlight))))

(defun mum-modeline/segment-buffer-name ()
  "Displays the name of the current buffer in the mode-line."
  (propertize " %b" 'face 'mum-modeline/default-face))

(defun mum-modeline/segment-position-percent ()
  "Displays the current cursor position in the mode-line."
  (let* ((percent (if (boundp 'mode-line-percent-position) mode-line-percent-position '(-3 "%q"))))
	(propertize (format-mode-line percent)
				'face 'mum-modeline/default-face)))

(defun mum-modeline/segment-position ()
  "Displays the current cursor position in the mode-line."
  `(,(propertize "L%l:C%c" 'face 'mum-modeline/default-face)
	,(if (region-active-p)
		 (propertize (format "[M+%s]"
							 (apply #'+ (mapcar
										 (lambda (pos)
										   (- (cdr pos)
											  (car pos)))
										 (region-bounds))))
					 'font-lock-face 'mum-modeline/info-face))))

(defun mum-modeline/segment-make-flycheck-info ()
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

(defun mum-modeline/segment-flycheck-text (&optional status)
  "Checker text via STATUS."
  (let* ((text))
	(let-alist (mum-modeline/segment-make-flycheck-info)
      (setq text (format "%s%s%s%s%s"
						 (propertize (concat "I:" (number-to-string .info))
									 'face 'mum-modeline/info-face)
						 (mum-modeline/segment-separator)
						 (propertize (concat "W:" (number-to-string .warning))
									 'face 'mum-modeline/warning-face)
						 (mum-modeline/segment-separator)
						 (propertize (concat "E:" (number-to-string .error))
									 'face 'mum-modeline/error-face))))
	(propertize text
				'help-echo (concat "mouse-1: Show all errors\nmouse-3: Next error"
								   (if (featurep 'mwheel)
									   "\nwheel-up/wheel-down: Previous/next error"))
				'mouse-face 'mode-line-highlight
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

(defun mum-modeline/segment-flycheck ()
  "Checker."
  (when (bound-and-true-p flycheck-mode)
	(mum-modeline/segment-flycheck-text)))

(defun mum-modeline/segment-vc ()
  "Display color-coded version control information in the mode-line."
  '(vc-mode vc-mode))

(defun mum-modeline/segment-vc-diff ()
  "Display vc diff."
  (let* ((status (vc-state (buffer-name))))
	(when (not (equal status 'up-to-date))
	  (propertize (format "D:%S" status)
				  'face 'mum-modeline/info-face
				  'help-echo (format "diff: %s" status)
				  'local-map (purecopy
							  (mum-modeline/make-mouse-map 'mouse-1 (lambda ()
																	  (interactive)
																	  (magit-diff-dwim))))
				  'mouse-face 'mode-line-highlight))))

(defun mum-modeline/segment-remote-host ()
  "Hostname for remote buffers."
  (when default-directory
	(when-let ((host (file-remote-p default-directory 'host)))
	  (propertize
	   (concat "R:" (system-name) "@" host)
	   'face 'mum-modeline/default-face))))

(defun mum-modeline/segment-symbol-count-info ()
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
						  'face 'mum-modeline/default-face
						  'help-echo (format "total %d\ncurrent %d" total curindex)))))))))

(defun mum-modeline/segment-indent-tab ()
  "Displays the indentation information."
  (if indent-tabs-mode
	  (propertize (format "T:%d" tab-width)
				  'face 'mum-modeline/default-face
				  'help-echo (format "tab width %d" tab-width))
	nil))

(defun mum-modeline/segment-indent-spc ()
  "Display the indentation information."
  (if indent-tabs-mode
	  (let ((spc)
			(indent
             (seq-find (lambda (var)
                         (and var (boundp var) (symbol-value var)))
                       (cdr (assoc major-mode mum-modeline/indent-alist)) nil)))
		(if indent (setq spc (symbol-value indent)) (setq spc tab-width))
		(propertize (format "S:%d" spc)
					'face 'mum-modeline/default-face
					'help-echo (format "indent offset %d" spc)))
	nil))

(defun mum-modeline/segment-eol ()
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
	 'face 'mum-modeline/default-face
	 'help-echo (format "eol: %s\nmouse-1: cycle" desc)
	 'local-map (purecopy
				 (mum-modeline/make-mouse-map
				  'mouse-1
				  (lambda (event)
					(interactive "e")
					(with-selected-window (posn-window (event-start event))
					  (let ((eol (coding-system-eol-type buffer-file-coding-system)))
						(set-buffer-file-coding-system
						 (cond ((eq eol 0) 'dos) ((eq eol 1) 'mac) (t 'unix))))))))
	 'mouse-face 'mode-line-highlight)))

(defun mum-modeline/segment-encoding ()
  "Display encoding of the buffer in mode-line."
  (let* ((sys (coding-system-plist buffer-file-coding-system))
		 (encoding))
	(cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
		   (setq encoding "UTF-8"))
		  (t
		   (setq encoding (upcase (symbol-name (plist-get sys :name))))))
	(propertize
	 encoding
	 'face 'mum-modeline/default-face
	 'help-echo (format "encoding: %s" encoding)
	 'mouse-face 'mode-line-highlight)))

(defun mum-modeline/segment-misc-info ()
  "Displays the current value of `mode-line-misc-info' in the mode-line."
  (let ((misc-info (string-trim (format-mode-line mode-line-misc-info 'mum-modeline/misc-face))))
	(unless (string= misc-info "")
	  (concat " " misc-info))))

(defun mum-modeline/segment-input-method ()
  "Displays the input-method of the buffer in the mode-line."
  `(""
	(current-input-method
	 (:propertize (" " current-input-method-title)
				  help-echo (format
							 "Current input method: %s\nmouse-1: Describe current input method"
							 current-input-method)
				  local-map ,(purecopy
							  (mum-modeline/make-mouse-map
							   'mouse-1
							   (lambda (e)
								 (interactive "e")
								 (with-selected-window (posn-window (event-start e))
								   (describe-current-input-method)))))
				  mouse-face 'mode-line-highlight))))

(defun mum-modeline/segment-minor-modes ()
  "Displays current minor modes in mode-line."
  (replace-regexp-in-string "%" "%%%%"
							(format-mode-line minor-mode-alist)
							t t))

(defun mum-modeline/segment-process ()
  "Displays current `mode-line-process' in the mode-line."
  (when mode-line-process
	(propertize (concat " " (string-trim (format-mode-line mode-line-process)))
				'face 'mum-modeline/default-face)))

(defun mum-modeline/segment-major-mode ()
  "Displays current major mode in mode-line."
  (propertize
   (concat " "
		   (or (and (boundp 'delighted-modes)
					(cadr (assq major-mode delighted-modes)))
			   (format-mode-line mode-name)))
   'face 'mum-modeline/default-face))

(defun mum-modeline/segment-time ()
  "Display current data and time."
  '("" display-time-string))

(defun mum-modeline/segment-winnum ()
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
                    'face 'mum-modeline/info-face)
	  (propertize (format "O")
				  'face 'mum-modeline/info-face
				  'help-echo (format "only window")))))

(defun mum-modeline/format (left-segments right-segments)
  "Make modeline string of `window-width' length.
Containing LEFT-SEGMENTS and RIGHT-SEGMENTS."
  (let* ((left (mum-modeline/format-segments left-segments))
		 (right (mum-modeline/format-segments right-segments))
		 (reserve (length right)))
	(concat
	 left
	 (propertize " "
				 'display `((space :align-to (- right ,reserve))))
	 right)))

(defun mum-modeline/format-segments (segments)
  "Make modeline string from SEGMENTS."
  (format-mode-line (mapcar (lambda (segment) `(:eval (,segment)))
							segments)))

(defun mum-modeline/get-current-window (&optional frame)
  "Get the current window but should exclude the child windows."
  (if (and (fboundp 'frame-parent) (frame-parent frame))
      (frame-selected-window (frame-parent frame))
    (frame-selected-window frame)))

(defvar mum-modeline/current-window
  (mum-modeline/get-current-window)
  "Current window.")

(defun mum-modeline/active? ()
  "Whether is an active window."
  (and mum-modeline/current-window
       (eq (mum-modeline/get-current-window) mum-modeline/current-window)))

(defun mum-modeline/set-selected-window (&rest _)
  "Set `mum-modeline/current-window' appropriately."
  (when-let ((win (mum-modeline/get-current-window)))
    (unless (or (minibuffer-window-active-p win)
                (and (bound-and-true-p lv-wnd) (eq lv-wnd win)))
      (setq mum-modeline/current-window win))))

(defun mum-modeline/unset-selected-window ()
  "Unset `mum-modeline/current-window' appropriately."
  (setq mum-modeline/current-window nil))

(defun mum-modeline/init-hook (&optional del?)
  "Init add hook.
DEL is add or delete?"
  (if del?
	  (progn
		(remove-hook 'flycheck-status-changed-functions #'mum-modeline/segment-flycheck-text)
		(remove-hook 'flycheck-mode-hook #'mum-modeline/segment-flycheck-text))

	(add-hook 'window-configuration-change-hook #'mum-modeline/set-selected-window)
	(add-hook 'buffer-list-update-hook #'mum-modeline/set-selected-window)
	(add-hook 'after-make-frame-functions #'mum-modeline/set-selected-window)
	(add-hook 'delete-frame-functions #'mum-modeline/set-selected-window)
	(add-hook 'exwm-workspace-switch-hook #'mum-modeline/set-selected-window)
	(advice-add #'handle-switch-frame :after #'mum-modeline/set-selected-window)
	(with-no-warnings
	  (if (boundp 'after-focus-change-function)
		  (progn
			(defun mum-modeline/refresh-frame ()
			  (setq mum-modeline/current-window nil)
			  (cl-loop for frame in (frame-list)
					   if (eq (frame-focus-state frame) t)
					   return (setq mum-modeline/current-window
									(mum-modeline/get-current-window frame)))
			  (force-mode-line-update))
			(add-function :after after-focus-change-function #'mum-modeline/refresh-frame))
		(progn
		  (add-hook 'focus-in-hook #'mum-modeline/set-selected-window)
		  (add-hook 'focus-out-hook #'mum-modeline/unset-selected-window))))

	(add-hook 'flycheck-status-changed-functions #'mum-modeline/segment-flycheck-text)
	(add-hook 'flycheck-mode-hook #'mum-modeline/segment-flycheck-text)

	(advice-add #'window-numbering-install-mode-line :override #'ignore)
	(advice-add #'window-numbering-clear-mode-line :override #'ignore)
	(advice-add #'winum--install-mode-line :override #'ignore)
	(advice-add #'winum--clear-mode-line :override #'ignore)))

;;;###autoload
(define-minor-mode mum-modeline-mode
  "Mum modeline minor mode."
  :init-value nil
  :keymap nil
  :lighter ""
  :group 'mum-modeline
  :global t
  (if mum-modeline-mode
	  (progn
		(mum-modeline/init-hook)
		(setq-default mode-line-format '(:eval mum-modeline/init)))
	(progn
	  (mum-modeline/init-hook)
	  (setq-default mode-line-format mum-modeline/default-format))))

(provide 'mum-modeline)
;;; mum-modeline.el ends here
