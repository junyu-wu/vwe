;;; vwe-base.el --- dev dependency environment.	-*- lexical-binding: t -*-

;; Copyright (C) 2015  Wu Junyu

;; Author: Wu Junyu <vistar_w@hotmail.com>
;; Keywords: languages

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
;; ***************************************************************************
;; lib
;; ***************************************************************************
(defun vwe@base--pkg-source-toggle (source-name)
  "Toggle Emacs package source.
SOURCE-NAME is source name."
  (interactive
   (list
    (completing-read "source:"
					 (mapcar (lambda(item)
							   (car item))
							 vwe@custom--source-list))))
  (let* ((source (cdr (assoc source-name vwe@custom--source-list))))
	(setq package-archives source)))

(defun vwe@base--package-init ()
  "Package init."
  (interactive)
  (vwe@base--pkg-source-toggle vwe@custom--source)
  (setq package-enable-at-startup nil)
  (package-initialize)
  (vwe@lib--package-load 'cl-lib)
  (vwe@lib--package-load 'seq)
  (vwe@lib--package-load 'popup))

(defvar vwe@base--socks-proxy-services
  nil
  "Socks proxy services.")

;;; custom file
(defun vwe@base--custom-file-init ()
  "Make and set `custom-file'."
  (let* ((custom (expand-file-name "custom.el" user-emacs-directory)))
    (unless (file-exists-p custom)
      (vwe@lib--path-make-config-path (vwe@lib--path-emacs.d "custom.el") t)
      (when package--initialized (package-refresh-contents)))
	(unless (equal custom custom-file)
      (setq custom-file custom)
      (load custom-file))))

(defun vwe@base--encoding-init (&optional encoding)
  "Set Emacs ENCODING."
  (interactive "zcoding:")
  (unless encoding
	(setq encoding 'utf-8))
  (setq locale-coding-system                 encoding
		default-process-coding-system        '(encoding . encoding))
  (when (fboundp 'set-charset-priority)
	(set-charset-priority 'unicode))
  (set-language-environment encoding)
  (set-keyboard-coding-system encoding)
  (set-clipboard-coding-system encoding)
  (set-terminal-coding-system encoding)
  (set-buffer-file-coding-system encoding)
  (set-default-coding-systems encoding)
  (set-selection-coding-system encoding)
  (set-file-name-coding-system encoding)
  (modify-coding-system-alist 'process "*" encoding)
  (prefer-coding-system encoding))

(defun vwe@base--frame-init ()
  "Frame config."
  (interactive)
  (when (display-graphic-p)
	(if vwe@custom--frame-max? (toggle-frame-maximized)
	  (vwe@lib--frame-reset (car vwe@custom--frame-size)
							(cadr vwe@custom--frame-size)
							(car vwe@custom--frame-position)
							(cadr vwe@custom--frame-position)))))

(defun vwe@base--font-init ()
  "Font config."
  (interactive)
  (if (display-graphic-p)
	  (vwe@lib--font-set-ascii vwe@custom--font-ascii vwe@custom--font-ascii-size)
	(vwe@lib--font-set-non-ascii vwe@custom--font-non-ascii vwe@custom--font-non-ascii-size)))

(defun vwe@base--server-init ()
  "Server init."
  (interactive)
  (when (and vwe@custom--server-startup?
			 (not (vwe@lib--server-running-p)))
	(server-start)))

(defun vwe@base--deamon-init ()
  "Deamon init."
  (interactive)
  ;; (add-hook 'vwe@custom--deamon-create-frame-after-hook #'vwe@base--package-init)
  (add-hook 'vwe@custom--deamon-create-frame-after-hook #'vwe@base--custom-file-init)
  (add-hook 'vwe@custom--deamon-create-frame-after-hook #'vwe@base--gc-init)
  (add-hook 'vwe@custom--deamon-create-frame-after-hook #'vwe@base--encoding-init)
  (add-hook 'vwe@custom--deamon-create-frame-after-hook #'vwe@base--frame-init)
  (add-hook 'vwe@custom--deamon-create-frame-after-hook #'vwe@base--font-init)
  (add-hook 'vwe@custom--deamon-create-frame-after-hook #'vwe@base--server-init)
  (add-hook 'vwe@custom--deamon-create-frame-after-hook #'vwe@base--debug-init)
  (run-hooks 'vwe@custom--deamon-create-frame-after-hook))

(defun vwe@base--gc-init ()
  "GC init."
  (interactive)
  (let* ((threshold (if (display-graphic-p) 8000000 800000))
		 (upper-limit (if (display-graphic-p) 400000000 100000000))
		 (handler-alist file-name-handler-alist))
	(setq file-name-handler-alist nil
		  gc-cons-threshold upper-limit)
	(add-hook 'emacs-startup-hook
			  (lambda ()
				"Restore defalut values after startup."
				(setq file-name-handler-alist handler-alist
					  gc-cons-threshold threshold)
				(add-hook 'minibuffer-setup-hook
						  #'(lambda () (setq gc-cons-threshold upper-limit)))
				(add-hook 'minibuffer-exit-hook
						  #'(lambda () (setq gc-cons-threshold threshold)))))))

(defun vwe@base--debug-init ()
  "Debug init."
  (when vwe@custom--debug?
	(setq debug-on-error t
		  max-lisp-eval-depth vwe@custom--debug-max-lisp-eval-depth)))

(defun vwe@base--make-welcome-msg ()
  "Make welcome message."
  (let* ((name '((:foreground "cyan" :weight bold)))
		 (vwe '((:background "DarkViolet" :foreground "white" :weight bold))))
	(concat
	 ";; Hello "
	 (propertize (format "%s" vwe@custom--user-name)
				 'face name)
	 ", welcome "
	 (propertize "vwiss emacs (vwe)"
				 'face vwe)
	 ", let's enjoy hacking ^_^ !!!\n"
	 ";; "
	 (vwe@lib--sys-startup-info)
	 "\n")))

(defun vwe@base--ui-init ()
  "UI init."
  (interactive)
  (setq-default cursor-type    'bar
				fill-column    80
				tab-width      4
				show-trailing-whitespace t)
  (setq frame-title-format (list (format "%s %%S: %%j "
										 (system-name))
								 '(buffer-file-name "%f"
                                                    (dired-directory dired-directory "%b")))
		icon-title-format frame-title-format)

  (unless vwe@custom--frame-menu-bar?
	(menu-bar-mode -1))
  (unless vwe@custom--frame-tool-bar?
	(tool-bar-mode -1))
  (unless vwe@custom--frame-scroll-bar?
	(scroll-bar-mode -1))
  (blink-cursor-mode -1))

(defun vwe@base--init ()
  "Base init."
  (interactive)
  (vwe@base--package-init)
  (vwe@base--custom-file-init)
  (vwe@base--gc-init)
  (vwe@base--encoding-init)
  (vwe@base--frame-init)
  (vwe@base--font-init)
  (vwe@base--server-init)
  (vwe@base--debug-init)
  (vwe@base--deamon-init)
  (vwe@base--ui-init)

  ;; builtin mode setup
  (add-hook 'emacs-startup-hook #'vwe@lib--sys-startup-info)
  (add-hook 'server-after-make-frame-hook #'vwe@base--deamon-init)

  (setq initial-scratch-message              (vwe@base--make-welcome-msg)
		initial-major-mode                   'fundamental-mode
		inhibit-startup-screen               t

        user-full-name                       vwe@custom--user-name
		user-mail-address                    vwe@custom--user-mail

		ring-bell-function                   'ignore
		read-process-output-max              (* 1024 1024)
		indent-tabs-mode                     nil
		completion-ignore-case               t

		yes-or-no-p                          'y-or-n-p
        kill-whole-line                      t
        delete-selection-mode                t
        make-backup-files                    nil
        create-lockfiles                     nil
        select-enable-clipboard              t
        confirm-kill-emacs                   (lambda (prompt)
											   (if vwe@custom--quit-ask?
												   (y-or-n-p-with-timeout "quit emacs:"
                                                                          10
                                                                          "y")
												 '(nil)))

		display-time-day-and-date            t
		display-time-24hr-format             t))

;; ***************************************************************************
;; config
;; ***************************************************************************

(when vwe@lib--sys-win-p
  (start-process "emacs-hide-cmd" nil
				 (vwe@lib--path-vwe-etc' "win/emacs_hide_cmd.exe" t)))

(vwe@base--init)

(vwe@lib--log "Initialization of Base configuration is complete.")

(provide 'vwe-base)
;;; vwe-base.el ends here
