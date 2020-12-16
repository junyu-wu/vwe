;;; vwe-buffer.el --- Buffer Setup               -*- lexical-binding: t; -*-

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
;; ***************************************************************************
;; lib
;; ***************************************************************************
(defun vwe@buffer--switch-to-minibuffer ()
  "Minibuffer show switch buffer list."
  (interactive)
  (let* ((buffer (buffer-name))
		 (split (list (vwe@lib--face-of-string "------------------------------"
										:background "DarkRed"
										  :foreground "white"
										  :weight 'bold)))
		 (non-alength (length(vwe@lib--buffer-non-asterisk-list)))
		 (buffers))
	(if (> non-alength 0)
		(setq buffers (append (vwe@lib--buffer-non-asterisk-list)
							 split
							 (vwe@lib--buffer-asterisk-list)))
	  (setq buffers (vwe@lib--buffer-asterisk-list)))

	(setq buffer (completing-read "switch to:" buffers))

	(unless (equal buffer split)
	  (switch-to-buffer buffer))))

(defun vwe@buffer--switch-to (&optional mini?)
  "Switch buffer to ....
MINI pop frame or minibuffer."
  (interactive)
  (if (and (not (vwe@lib--buffer-match-p vwe@custom--buffer-filter-list))
		   (display-graphic-p)
		   (not mini?))
	  (progn
		(define-key popup-menu-keymap (kbd "n") 'popup-next)
		(define-key popup-menu-keymap (kbd "p") 'popup-previous)
		(define-key popup-menu-keymap (kbd ".") 'popup-page-next)
		(define-key popup-menu-keymap (kbd ",") 'popup-page-previous)
		(define-key popup-menu-keymap (kbd "s") 'popup-isearch)
		(define-key popup-menu-keymap (kbd "q") 'keyboard-quit)
		(define-key popup-menu-keymap (kbd "f") 'popup-open)
		(define-key popup-menu-keymap (kbd "b") 'popup-close)
		(define-key popup-menu-keymap (kbd "<tab>") 'popup-close)
		(define-key popup-menu-keymap (kbd "<return>") 'popup-select)
		(define-key popup-menu-keymap (kbd "m") (lambda () (interactive)
												  (vwe@buffer--switch-to-minibuffer)
												  (keyboard-quit)))
		(switch-to-buffer (popup-cascade-menu
						   (cons (cons "* buffer list *" (vwe@lib--buffer-asterisk-list))
								 (vwe@lib--buffer-non-asterisk-list))
						   :initial-index 1)))
	(vwe@buffer--switch-to-minibuffer)))

;; ***************************************************************************
;; lib
;; ***************************************************************************
;; 保存buffer历史记录
(use-package savehist
  :ensure nil
  :hook
  (after-init . savehist-mode)
  :init
  (setq enable-recursive-minibuffers t
		savehist-file (vwe@lib--path-cache "savehist/history" t)
		history-length 1000
		savehist-additional-variables '(mark-ring
										global-mark-ring
										search-ring
										regexp-search-ring
										extended-command-history)
		savehist-autosave-interval 300))

;; 特定条件还原buffer
(use-package autorevert
  :ensure nil
  :diminish
  :hook
  (after-init . global-auto-revert-mode)
  :config
  (global-auto-revert-mode 1))

(use-package ibuffer
  :ensure nil
  :bind
  (:map global-map
  		("C-x C-b" . ibuffer))
  :init
  (setq ibuffer-expert t
  		ibuffer-show-empty-filter-groups nil
  		ibuffer-display-summary nil
		ibuffer-saved-filter-groups
  		(quote (("default"
				 ("lisp" (or
						  (mode . emacs-lisp-mode)
  						  (mode . lisp-mode)))
				 ("cpp" (or
						 (mode . cperl-mode)
						 (mode . c-mode)
						 (mode . c++-mode)
						 (mode . objc-mode)
						 ))
				 ("build" (or
						   (mode . cmake-mode) ))
				 ("java" (or
						  (mode . java-mode)
						  (mode . scala-mode)))
				 ("python" (or
							(mode . python-mode)))
				 ("ruby" (or
						  (mode . ruby-mode) ))
				 ("db" (or
						(mode . sql-mode)	))
				 ("golang" (or
							(mode . go-mode)	))
				 ("rust" (or
						  (mode . rust-mode) ))
				 ("web" (or
						 (mode . web-mode)
						 (mode . js2-mode)
						 (mode . css-mode)
						 (mode . scss-mode)
						 (mode . javascript-mode)
						 (mode . rjsx-mode)
						 (mode . lua-mode)
						 (mode . json-mode)))
				 ("assembly" (or
							  (mode . asm-mode)
							  (mode . nasm-mode)))
				 ("shell" (or
						   (mode . sh-mode) ))
  				 ("dired" (or
  						   (mode . dired-mode)
  						   (mode . sr-mode)))
  				 ("erc" (mode . erc-mode))
  				 ("edit" (or
  						  (name . "^\\*Calendar\\*$")
  						  (name . "^diary$")
  						  (mode . muse-mode)
  						  (mode . org-mode)
  						  (mode . org-agenda-mode)
  						  (mode . text-mode)
						  (mode . yaml-mode)))
  				 ("buffer" (or
  							(name . "^\\*scratch\\*$")
  							(name . "^\\*Messages\\*$")))
				 ("email" (or
						   (name . "^\\*mu4e-headers\\*$")))
  				 ("mesasge" (or
  							 (mode . message-mode)
  							 (mode . bbdb-mode)
  							 (mode . mail-mode)
  							 (mode . gnus-group-mode)
  							 (mode . gnus-summary-mode)
  							 (mode . gnus-article-mode)
  							 (name . "^\\.bbdb$")
  							 (name . "^\\.newsrc-dribble")))))))
  (add-hook 'ibuffer-mode-hook
  			(lambda ()
  			  (unless (eq ibuffer-sorting-mode 'filename/process)
  				(ibuffer-do-sort-by-filename/process))
  			  (ibuffer-switch-to-saved-filter-groups "default"))))

;; buffer文本转换为html
(use-package htmlize)

(provide 'vwe-buffer)
;;; vwe-buffer.el ends here
