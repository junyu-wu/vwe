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
;;; custom file
(defun vwe@base--custom-file-init ()
  "Make and set `custom-file'."
  (let* ((custom (expand-file-name "custom.el" user-emacs-directory)))
    (unless (file-exists-p custom)
      (vwe@lib--path-make-config-path (vwe@lib--path-emacs.d "custom.el") t))
    (setq custom-file custom)
    (load custom-file)))

(defun vwe@base--encoding-init (&optional encoding)
  "Set Emacs ENCODING."
  (interactive "zcoding:")
  (unless encoding
	(setq encoding 'utf-8))
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
  (vwe@lib--frame-reset vwe@custom--frame-width
						vwe@custom--frame-height
						50 50))

(defun vwe@base--font-init ()
  "Font config."
  (interactive)
  (when (display-graphic-p)
	(vwe@lib--font-set-ascii vwe@custom--font-ascii
							 vwe@custom--font-ascii-size)
	(vwe@lib--font-set-non-ascii vwe@custom--font-non-ascii
								 vwe@custom--font-non-ascii-size)))

(defun vwe@base--server-init ()
  "Server init."
  (interactive)
  (when (and vwe@custom--server-startup?
			 (not (vwe@lib--server-running-p)))
	(server-start)))

(defun vwe@base--deamon-init ()
  "Deamon init."
  (interactive)
  ;; TODO deamon init.
  )

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
  (format ";; hello %s, welcome vwiss emacs (vwe), let's enjoy hacking!\n;; %s\n"
		  vwe@custom--user-name
		  (vwe@lib--sys-startup-info)))

(defun vwe@base--init ()
  "Base init."
  (interactive)

  (vwe@base--custom-file-init)
  (vwe@base--gc-init)
  (vwe@base--encoding-init)
  (vwe@base--frame-init)
  (vwe@base--font-init)
  (vwe@base--server-init)
  (vwe@base--deamon-init)
  (vwe@base--debug-init)

  (add-hook 'emacs-startup-hook #'vwe@lib--sys-startup-info)
  (add-hook 'before-save-hook  #'delete-trailing-whitespace)
  (add-hook 'after-init-hook #'delete-selection-mode)
  (add-hook 'after-init-hook #'save-place-mode)
  (add-hook 'after-init-hook #'recentf-mode)
  (add-hook 'after-init-hook #'show-paren-mode)
  (add-hook 'server-after-make-frame-hook 'vwe@base--daemon-init)


  (when vwe@custom--frame-max?
	(add-hook 'window-setup-hook 'toggle-frame-maximized))

  (setq inhibit-startup-screen               t
		ring-bell-function                   'ignore
		yes-or-no-p                          'y-or-n-p
		locale-coding-system                 'utf-8
		default-process-coding-system        '(utf-8 . utf-8)
		user-full-name                       (format vwe@custom--user-name)
		user-mail-address                    (format vwe@custom--user-mail)
		initial-scratch-message              (vwe@base--make-welcome-msg)

		column-number-mode                   t
        line-number-mode                     t
        kill-whole-line                      t
		line-move-visual                     nil
        track-eol                            t
        set-mark-command-repeat-pop          t

		display-time-24hr-format             t
		display-time-day-and-date            t

		calendar-mark-holidays-flag          t

		server-auth-dir                      (vwe@lib--path-cache "server")

		indent-tabs-mode                     nil
		auto-save-default                    nil
        auto-save-list-file-prefix           (concat (vwe@lib--path-cache
                                                      "auto-save")
                                                     "/.saves-")
		make-backup-files                    nil
		confirm-kill-emacs                   #'(lambda (prompt)
												 (if vwe@custom--quit-ask?
													 (y-or-n-p-with-timeout
                                                      "quit emacs:" 10 "y")
												   '(nil)))

        select-enable-clipboard              t

        save-place-file                      (vwe@lib--path-cache
                                              "saveplace/places" t)

        recentf-auto-cleanup                 900
		recentf-max-menu-item                30
		recentf-max-saved-items              200
		recentf-save-file                    (vwe@lib--path-cache "recentf/.recentf" t)
		recentf-exclude                      '("\\.?cache" ".cask" "url"
                                               "COMMIT_EDITMSG\\'" "bookmarks"
                                               "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$"
											   "^/tmp/" "^/ssh:" "\\.?ido\\.last$"
                                               "\\.revive$" "/TAGS$" "^/var/folders/.+$"
											   (lambda (file)
                                                 (file-in-directory-p file package-user-dir)))

        show-paren-style                     'parenthesis)

  (define-advice show-paren-function
      (:around (fn) fix-show-paren-function)
    "Highlight enclosing parens."
    (cond ((looking-at-p "\\s(") (funcall fn))
		  (t (save-excursion
			   (ignore-errors (backward-up-list))
			   (funcall fn))))))

(defun vwe@base--recentf-clear ()
  "Recentf clear."
  (interactive)
  (write-region "" nil recentf-save-file)
  (setq recentf-list 'nil)
  (recentf-save-list))

(defun vwe@base--paren-toggle-style (&optional style)
  "Paren style."
  (interactive
   (list
    (completing-read "paren style: "
					 '("parenthesis" "expression" "mixed"))))
  (let* ((styles '(("parenthesis" . parenthesis)
				   ("expression" . expression)
				   ("mixed" . mixed))))
    (setq show-paren-style (cdr (assoc style styles)))))

(defun vwe@base--proxy-status ()
  "Show HTTP/HTTPS proxy."
  (interactive)
  (let* ((http-msg "http/https no proxy.")
		 (socks-msg "socks no proxy."))
	(when url-proxy-services
	  (setq http-msg (format "http proxy: %s." vwe@custom--proxy-http)))
	(when vwe@base--socks-proxy-services
	  (setq socks-msg (format "socks/%d proxy: %s."
							  vwe@custom--proxy-socks-version
							  vwe@custom--proxy-socks)))
	(message "%s %s" http-msg socks-msg)))

(defun vwe@base--proxy-http (&optional on/off?)
  "Enable or disable http/https proxy.
ON/OFF switch."
  (interactive)
  (if on/off?
	  (progn
		(setq url-proxy-services `(("http" . ,vwe@custom--proxy-http)
								   ("https" . ,vwe@custom--proxy-http)
								   ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)"))))
	(setq url-proxy-services nil)))

(defun vwe@base--proxy-http-toggle ()
  "Toggle http/https proxy."
  (interactive)
  (if url-proxy-services
	  (vwe@base--proxy-http nil)
	(vwe@base--proxy-http t)))

(defun vwe@base--proxy-socks (&optional on/off?)
  "Enable or disable socks proxy.
ON/OFF?"
  (interactive)
  (if (and on/off? (vwe@lib--package-load 'socks))
	  (progn
		(let* ((server:port (split-string vwe@custom--proxy-socks ":"))
			   (server (car server:port))
			   (port (nth 1 server:port)))
		  (setq url-gateway-method 'socks
				socks-noproxy '("localhost")
				socks-server '("Default server" server port)
				vwe@base--socks-proxy-services socks-server)))
	(setq url-gateway-method 'native
		  socks-noproxy nil)))

(defun vwe@base--proxy-socks-toggle ()
  "Toggle http/https proxy."
  (interactive)
  (if vwe@base--socks-proxy-services
	  (vwe@base--proxy-socks)
	(vwe@base--proxy-socks t)))

;; ***************************************************************************
;; config
;; ***************************************************************************

(when vwe@lib--sys-win-p
  (start-process "emacs-hide-cmd" nil
				 (vwe@lib--path-vwe-etc' "win/emacs_hide_cmd.exe" t)))

(vwe@base--init)

(require 'vwe-ui)
(require 'vwe-theme)
(require 'vwe-layout)
(require 'vwe-headerline)
(require 'vwe-modeline)
(require 'vwe-buffer)

(provide 'vwe-base)
;;; vwe-base.el ends here
