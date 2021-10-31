;;; vwe-term.el ---    Vwe term         -*- lexical-binding: t; -*-

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
(require 'term)

(defgroup vwe-term nil
  "Vwe term."
  :group 'vwe)

(defcustom vwe-term--shell-command
  nil
  "Terminal shell command."
  :group 'vwe-term
  :type 'string)

(defvar vwe-term--buffer-name-prefix
  "[vwterm]"
  "Terminal buffer name prefix.")

(defvar vwe-term--terminal-default-path
  ;; (file-name-directory (buffer-file-name))
  (getenv "HOME")
  "Terminal default directory.")

(defvar vwe-term--terminal-list
  nil
  "Terminal list.")

(defvar vwe-term--terminal-window
  nil
  "Terminal window.")

(defvar vwe-term--default-solt
  -3
  "Default solt.")

(defvar vwe-term--default-width
  0.3
  "Default width.")

(defvar vwe-term--default-height
  0.3
  "Default height.")

(defvar vwe-term--located
  'right
  "Term show located.")

(defconst vwe-term--unbinding-key-list
  '("C-z" "C-x" "C-c" "C-h" "C-y" "C-r" "<ESC>")
  "Unbinding key list.")

(defun vwe-term--send-esc ()
  "Send ESC in term mode."
  (interactive)
  (term-send-raw-string "\e"))

(defun vwe-term--send-return ()
  "Send return in term mode."
  (interactive)
  (term-send-raw-string "\C-m"))

(defun vwe-term--send-kill-proc ()
  "Send kill process in term mode."
  (interactive)
  (term-send-raw-string "\C-c"))

(defun vwe-term--send-suspend-proc ()
  "Send suspend in term mode."
  (interactive)
  (term-send-raw-string "\C-z"))

(defun vwe-term--send-clear ()
  "Send clear in term mode."
  (interactive)
  (term-send-raw-string "\C-l"))

(defun vwe-term-send-reverse-search-history ()
  "Search history reverse."
  (interactive)
  (term-send-raw-string "\C-r"))

(defun vwe-term--send-complete ()
  "Send complete in term mode."
  (interactive)
  (term-send-raw-string "\C-i"))

(defun vwe-term--send-quit ()
  "Move forward word in term mode."
  (interactive)
  (term-send-raw-string "exit \C-m"))

(defun vwe-term--send-M-x ()
  "Emacs command in term-mode."
  (interactive)
  (term-send-raw-string "\ex"))

(defun vwe-term--send-paste ()
  "Send paste in term mode."
  (interactive)
  (term-send-raw-string (current-kill 0)))

(defun vwe-term--send-move-begin-of-line ()
  "Move begin of line in term mode."
  (interactive)
  (term-send-raw-string "\C-a"))

(defun vwe-term--send-move-end-of-line ()
  "Move end of line in term mode."
  (interactive)
  (term-send-raw-string "\C-e"))

(defun vwe-term--send-forward-word ()
  "Move forward word in term mode."
  (interactive)
  (term-send-raw-string "\ef"))

(defun vwe-term--send-backward-word ()
  "Move backward word in term mode."
  (interactive)
  (term-send-raw-string "\eb"))

(defun vwe-term--send-delete-word ()
  "Delete word in term mode."
  (interactive)
  (term-send-raw-string "\ed"))

(defun vwe-term--init-keymap ()
  "Init term keymap."
  (dotimes (i (length vwe-term--unbinding-key-list))
	(let* ((key-str (nth i vwe-term--unbinding-key-list))
		   (key))
	  (cond
	   ((stringp key-str) (setq key (read-kbd-macro key-str)))
	   ((mapp key-str) (setq key key-str)))
	  (when key (define-key term-raw-map key nil))))

  (local-set-key (kbd "M-p") nil)
  (local-set-key (kbd "M-n") nil)

  (define-key term-raw-map (kbd "M-p") #'term-send-up)
  (define-key term-raw-map (kbd "M-n") #'term-send-down)
  (define-key term-raw-map (kbd "C-a") #'vwe-term--send-move-begin-of-line)
  (define-key term-raw-map (kbd "C-e") #'vwe-term--send-move-end-of-line)
  (define-key term-raw-map (kbd "C-c l") #'term-line-mode)

  (define-key term-raw-map (kbd "C-m") #'vwe-term--send-return)
  (define-key term-raw-map (kbd "C-c C-e") #'vwe-term--send-esc)
  (define-key term-raw-map (kbd "C-c C-c") #'vwe-term--send-kill-proc)
  (define-key term-raw-map (kbd "C-c C-z") #'vwe-term--send-suspend-proc)
  (define-key term-raw-map (kbd "C-c C-l") #'vwe-term--send-clear)
  (define-key term-raw-map (kbd "C-c C-i") #'vwe-term--send-complete)
  (define-key term-raw-map (kbd "C-y") #'vwe-term--send-paste)
  (define-key term-raw-map (kbd "C-c q") #'vwe-term--send-quit)
  (define-key term-raw-map (kbd "M-f") #'vwe-term--send-forward-word)
  (define-key term-raw-map (kbd "M-b") #'vwe-term--send-backward-word)
  (define-key term-raw-map (kbd "M-d") #'vwe-term--send-delete-word))

(defun vwe-term--make-buffer-name (&optional name suffix)
  "Make terminal buffer name with `vwe-term--buffer-name-prefix' + NAME + [SUFFIX]."
  (let* ((buf-name (format "*%s:%s-%s*"
						   vwe-term--buffer-name-prefix
						   (or name "term")
						   (or suffix 1)))
		 (buffer (get-buffer (format "*%s*" buf-name)))
		 (index (or suffix 1)))
	(when (bufferp buffer)
	  (setq buf-name (vwe-term--make-buffer-name name (1+ index))))
	buf-name))

(defun vwe-term--make-terminal (&optional name)
  "Make NAME terminal buffer with SH-CMD."
  (with-temp-buffer
	(let* ((tname (or name (vwe-term--make-buffer-name major-mode))))
	  (cd (or default-directory vwe-term--terminal-default-path))
	  (make-term tname (getenv "SHELL")))))

(defun vwe-term--find-shell-window ()
  "Find shell window."
  (let (window)
	(dolist (win (window-list))
	  (when (string-match-p "\\**\\(?:\\[vwterm\\]\\)\\**" (buffer-name (window-buffer win)))
		(setq window win)))
	window))

(defun vwe-term--make-buffer-alist (located slt)
  "Make buffer alist by LOCATED and SLT."
  (cond
   ((or (eq located 'left) (eq located 'right))
	(setq vwe-term--located located)
	`((side . ,vwe-term--located)
	  (slot . ,(or slt vwe-term--default-solt))
	  (window-width . ,vwe-term--default-width) ;; (window-width . fit-window-to-buffer)
	  (window-parameters . ((no-other-window . t)
							(no-delete-other-windows . t)))))
   ((or (eq located 'top) (eq located 'bottom))
	(setq vwe-term--located located)
	`((side . ,vwe-term--located)
	  (slot . ,(or slt vwe-term--default-solt))
	  (window-height . ,vwe-term--default-height) ;; (window-height . fit-window-to-buffer)
	  (window-parameters . ((no-other-window . t)
							(no-delete-other-windows . t)))))
   (t  `((side . ,vwe-term--located)
		 (slot . ,(or slt vwe-term--default-solt))
		 (window-width . ,vwe-term--default-width) ;; (window-width . fit-window-to-buffer)
		 (window-parameters . ((no-other-window . t)
							   (no-delete-other-windows . t)))))))

(defun vwe-term--show-buffer (buffer)
  "Show BUFFER."
  (interactive
   (list
    (completing-read
	 (format "shell:")
	 (mapcar (lambda (buf)
			   (when (bufferp buf)
				 (buffer-name buf)))
			 vwe-term--terminal-list))))
  (let* ((win (selected-window))
		 (to (next-window)))
	(unless (eq win to)
	  (delete-window win))
	(set-window-buffer to buffer)))

(defun vwe-term--show-side-buffer (buffer located &optional slt)
  "Show BUFFER by LOCATED and SLT."
  (when (and buffer (bufferp buffer))
	(display-buffer-in-side-window buffer (vwe-term--make-buffer-alist located slt))
	(when (windowp (vwe-term--find-shell-window))
	  (vwe-term--init-keymap)
	  (select-window (vwe-term--find-shell-window)))))

(defun vwe-term--reset-shell-located (&optional located)
  "Reset BUFFER shell show LOCATED."
  (interactive
   (list
    (completing-read (format "show:")
					 (list 'top 'right 'left 'bottom))))
  (let* ((window (vwe-term--find-shell-window))
		 (buf (window-buffer window))
		 (loc (or (intern located) vwe-term--located)))
	(when (and window buf)
	  (delete-windows-on buf)
	  (vwe-term--show-side-buffer buf loc))))

(defun vwe-term--switch-terminal (&optional bufname)
  "Switch term by BUFNAME."
  (interactive
   (list
    (completing-read
	 (format "shell:")
	 (mapcar (lambda (buf)
			   (when (bufferp buf)
				 (buffer-name buf)))
			 vwe-term--terminal-list))))

  (when (bufferp (get-buffer bufname))
	(let* ((window (vwe-term--find-shell-window)))
	  (if (windowp window)
		  (set-window-buffer window bufname)
		(vwe-term--show-side-buffer (get-buffer bufname) vwe-term--located)))))

(defun vwe-term--reshow-terminal-buffer ()
  "Switch term by bufname."
  (let ((bufname (completing-read
				  (format "shell:")
				  (mapcar (lambda (buf)
							(when (bufferp buf)
							  (buffer-name buf)))
						  vwe-term--terminal-list))))
	(when (bufferp (get-buffer bufname))
	  (let* ((window (vwe-term--find-shell-window)))
		(if (windowp window)
			(set-window-buffer window bufname)
		  (vwe-term--show-side-buffer (get-buffer bufname) vwe-term--located))))))

(defun vwe-term--kill-terminal ()
  "Quit term process."
  (interactive)
  (when (eq major-mode 'term-mode)
    (let ((killed-buffer (current-buffer)))
      (when (term-check-proc (current-buffer)) (term-kill-subjob))
	  (setq vwe-term--terminal-list (delq killed-buffer vwe-term--terminal-list))
	  (if (> (length vwe-term--terminal-list) 0)
		  (vwe-term--switch-terminal (car vwe-term--terminal-list))))))

(defun vwe-term--exit ()
  "Exit."
  (interactive)
  (when (eq major-mode 'term-mode)
	(vwe-term--kill-terminal)
	(if vwe-term--terminal-list
		(kill-buffer)
	  (let* ((window (selected-window))
			 (buffer (current-buffer)))
		(delete-window window)
		(kill-buffer buffer)))))

(defun vwe-term--run-term (buffer)
  "Run term in BUFFER."
  (set-buffer buffer)
  (with-current-buffer buffer
	(setq show-trailing-whitespace nil)
	(add-hook 'kill-buffer-hook #'vwe-term--kill-terminal)
	(add-hook 'term-mode-hook #'vwe-term--init-keymap)
	(term-mode)
	(term-char-mode))
  (vwe-term--show-side-buffer buffer vwe-term--located))

;;;###autoload
(defun vwe-terminal (&optional name)
  "Run vwe term with NAME."
  (interactive)
  (let* ((new t))
	(if vwe-term--terminal-list
		(if (y-or-n-p "VWTerm exist, do you create new term? ")
			(setq new t)
		  (setq new nil))
	  (setq vwe-term--located 'right))

	(if new
		(if (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
			(eshell)
		  (let* ((tname (vwe-term--make-buffer-name
						 (or name (read-string
								   (format "term(%s):"
										   (vwe-term--make-buffer-name))
								   nil nil
								   (vwe-term--make-buffer-name)))))
				 (buffer (vwe-term--make-terminal tname))
				 (dir (or default-directory vwe-term--terminal-default-path)))
			(when (bufferp buffer)
			  (setq vwe-term--terminal-list (append vwe-term--terminal-list (list buffer)))
			  (when (vwe-term--find-shell-window)
				(delete-window (vwe-term--find-shell-window)))
			  (vwe-term--run-term buffer)
			  (when (and (featurep 'tramp) (tramp-tramp-file-p dir))
				(with-parsed-tramp-file-name dir path
				  (let ((method (cadr (assoc `tramp-login-program (assoc path-method tramp-methods)))))
					(term-send-raw-string (concat method " " (when path-user (concat path-user "@")) path-host "\C-m"))
					(term-send-raw-string (concat "cd '" path-localname "'\C-m"))))))))
	  (vwe-term--reshow-terminal-buffer))))

(provide 'vwe-term)
;;; vwe-term.el ends here
