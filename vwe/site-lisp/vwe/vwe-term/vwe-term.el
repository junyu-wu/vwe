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

;;
;; require
;;
(require 'term)

;;
;; customize
;;

(defgroup vwe-term nil
  "Vwe term."
  :group 'vwe)

(defcustom vwe-term--shell-command
  nil
  "Terminal shell command."
  :group 'vwe-term
  :type 'string)

;;
;; variable
;;

(defvar vwe-term--buffer-name-prefix
  "m-term"
  "Terminal buffer name prefix.")

(defvar vwe-term--terminal-default-path
  (getenv "HOME")
  "Terminal default directory.")

(defvar vwe-term--terminal-list
  nil
  "Terminal list.")

(defvar vwe-term--terminal-program-arguments
  nil
  "Terminal program arguments.")

(defconst vwe-term--unbinding-key-list
  '("C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>")
  "Unbinding key list.")

;;
;; util func
;;
(defun vwe-term--make-buffer-name (&optional dedicated?)
  "Make terminal buffer name with `vwe-term--buffer-name-prefix' + SUFFIX.
If DEDICATED non-nil the buffer is dedicated."
  (let* ((name (format "*%s:%s*"
					   (if dedicated?
						   (concat (upcase vwe-term--buffer-name-prefix)
								   "-DEDICATED")
						 (upcase vwe-term--buffer-name-prefix))
					   (format-time-string "%y%m%d%H%M%S")))
		 (buffer (get-buffer name)))
	(if (bufferp buffer)
		(vwe-term-make-buffer-name)
	  name)))

(defun vwe-term--get-shell-command ()
  "Get shell command."
  (let ((command))
	(cond ((or (eq system-type 'windows-nt) (eq system-type 'cygwin)) (setq command vwe-term--shell-command))
		  ((getenv "SHELL") (setq command (getenv "SHELL")))
		  ((getenv "ESHELL") (setq command (getenv "ESHELL")))
		  (t (setq command "/usr/bin/sh")))
	command))

(defun vwe-term--make-terminal (&optional special? dedicated?)
  "Make terminal buffer with SH-CMD.
If SPECIAL non-nil then input new shel command.
If DEDICATED non-nil the buffer is dedicated."
  (with-temp-buffer
	(let* ((name (vwe-term--make-buffer-name))
		   (buffer)
		   (shell-cmd)
		   (dir (or default-directory vwe-term--terminal-default-path)))
	  (cd dir)
	  (when dedicated? (setq name (vwe-term--make-buffer-name t)))
	  (if special?
		  (setq shell-cmd (read-from-minibuffer "shell:"))
		(setq shell-cmd (vwe-term--get-shell-command)))
	  (setq buffer (if vwe-term--terminal-program-arguments
					   (make-term name shell-cmd nil vwe-term--terminal-program-arguments)
					 (make-term name shell-cmd)))
	  buffer)))

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

  (define-key term-raw-map (kbd "C-s") #'isearch-forward)
  (define-key term-raw-map (kbd "C-r") #'isearch-backward)
  (define-key term-raw-map (kbd "C-r") #'isearch-backward)

  (define-key term-raw-map (kbd "C-a") #'vwe-term--send-move-begin-of-line)
  (define-key term-raw-map (kbd "C-e") #'vwe-term--send-move-end-of-line)
  (define-key term-raw-map (kbd "M-,") #'term-send-raw)
  (define-key term-raw-map (kbd "M-.") #'completion-at-point)
  (define-key term-raw-map (kbd "M-p") #'term-send-up)
  (define-key term-raw-map (kbd "M-n") #'term-send-down)
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
  (define-key term-raw-map (kbd "M-d") #'vwe-term--send-delete-word)
  (define-key term-raw-map (kbd "M-<") #'vwe-term--previous-terminal)
  (define-key term-raw-map (kbd "M->") #'vwe-term--next-terminal))

;;
;; terminal
;;

(defun vwe-term--switch-terminal (term-buffer)
  "Switch current to TERM-BUFFER buffer.
Switch term to DIRECTION `next' or `previous' buffer."
  (when vwe-term--terminal-list
	(let* ((buffer (cond ((bufferp term-buffer) term-buffer)
						 ((stringp term-buffer) (when (bufferp (get-buffer term-buffer))
												  (get-buffer term-buffer))))))
	  (when (and buffer (memq buffer vwe-term--terminal-list))
		(switch-to-buffer buffer)))))

(defun vwe-term--next-terminal ()
  "To next term buffer."
  (interactive)
  (let* ((cur-buffer (current-buffer))
		 (to-buffer))
	(catch 'break
	  (dotimes (i (length vwe-term--terminal-list))
		(when (equal cur-buffer (nth i vwe-term--terminal-list))
		  (setq to-buffer (nth (if (>= (1+ i) (length vwe-term--terminal-list))
								   0 (1+ i))
							   vwe-term--terminal-list))
		  (throw 'break nil))))
	(vwe-term--switch-terminal to-buffer)))

(defun vwe-term--previous-terminal ()
  "To previous term buffer."
  (interactive)
  (let* ((cur-buffer (current-buffer))
		 (to-buffer))
	(catch 'break
	  (dotimes (i (length vwe-term--terminal-list))
		(when (equal cur-buffer (nth i vwe-term--terminal-list))
		  (setq to-buffer (nth (if (< (1- i) 0)
								   (1- (length vwe-term--terminal-list))
								 (1- i))
							   vwe-term--terminal-list))
		  (throw 'break nil))))
	(vwe-term--switch-terminal to-buffer)))

(defun vwe-term--kill-term-buffer ()
  "Kill term buffer."
  (interactive)
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel
	 (get-buffer-process (current-buffer))
     (lambda (proc change)
	   (when (string-match "\\(finished\\|exited\\)" change)
		 (kill-buffer (process-buffer proc)))))))

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
	(kill-buffer)))

(defun vwe-term--run-term ()
  "Run Emacs term and Vwe term."
  (add-hook 'term-mode-hook #'vwe-term--init-keymap)
  (term-mode)
  (term-char-mode)
  (vwe-term--kill-term-buffer)
  (add-hook 'kill-buffer-hook #'vwe-term--kill-terminal))

;;;###autoload
(defun vwe-terminal ()
  "Run vwe term."
  (interactive)
  (if (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
	  (eshell)
	(let* ((buffer (vwe-term--make-terminal))
		   (dir (or default-directory vwe-term--terminal-default-path)))
	  (when (bufferp buffer)
		(setq vwe-term--terminal-list (append vwe-term--terminal-list (list buffer)))
		(set-buffer buffer)
		(vwe-term--run-term)
		(with-current-buffer buffer (setq show-trailing-whitespace nil))
		(switch-to-buffer buffer)
		(when (and (featurep 'tramp) (tramp-tramp-file-p dir))
		  (with-parsed-tramp-file-name dir path
			(let ((method (cadr (assoc `tramp-login-program (assoc path-method tramp-methods)))))
			  (term-send-raw-string (concat method " " (when path-user (concat path-user "@")) path-host "\C-m"))
			  (term-send-raw-string (concat "cd '" path-localname "'\C-m")))))))))

(provide 'vwe-term)
;;; vwe-term.el ends here
