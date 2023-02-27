;;; vwe-general.el --- Package Management   -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Wu Junyu

;; Author: Wu Junyu <vistar_w@hotmail.com.com>
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
;; ***************************************************************************
;; lib
;; ***************************************************************************
(defun vwe@base--recentf-clear ()
  "Recentf clear."
  (interactive)
  (write-region "" nil recentf-save-file)
  (setq recentf-list 'nil)
  (recentf-save-list))

(defun vwe@base--paren-toggle-style (&optional style)
  "Paren STYLE."
  (interactive
   (list
    (completing-read (format "style (%s):" show-paren-style)
					 '("parenthesis" "expression" "mixed"))))
  (let* ((styles '(("parenthesis" . parenthesis)
				   ("expression" . expression)
				   ("mixed" . mixed))))
    (setq show-paren-style (cdr (assoc style styles)))))
;; ***************************************************************************
;; config
;; ***************************************************************************

;;; builtin pkgs
(vwe@lib--pkg simple
  :init ((add-hook 'before-save-hook  #'delete-trailing-whitespace))
  :variable ((setq column-number-mode t
				   line-number-mode t
				   line-move-visual t
				   auto-save-mode t

				   track-eol t
				   set-mark-command-repeat-pop t))
  :buildin t)

(vwe@lib--pkg files
  :init ((add-hook 'auto-save-hook #'vwe@lib--buffer-save-all)
		 (add-hook 'find-file-hook (lambda()
									 (when (equal major-mode 'org-mode)
									   (goto-char (point-min))
									   (org-shifttab nil)))))
  :variable ((setq auto-save-default vwe@custom--buffer-auto-save?
				   auto-save-visited-interval 5
				   auto-save-timeout 10
				   auto-save-interval 300
				   auto-save-list-file-prefix (concat (vwe@lib--path-cache "auto-save"))))
  :buildin t)

(vwe@lib--pkg saveplace
  :init ((add-hook 'after-init-hook #'save-place-mode))
  :variable ((setq save-place-file (vwe@lib--path-cache "saveplace/places" t)))
  :buildin t)

(vwe@lib--pkg recentf
  :init ((add-hook 'after-init-hook #'recentf-mode))
  :variable ((setq recentf-max-menu-item 30
				   recentf-max-saved-items 200
				   recentf-save-file (vwe@lib--path-cache "recentf/.recentf" t)
				   recentf-exclude '("\\.?cache"
									 ".cask"
									 "url"
									 "COMMIT_EDITMSG\\'"
									 "bookmarks"
									 "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$"
									 "^/tmp/"
									 "^/ssh:"
									 "\\.?ido\\.last$"
									 "\\.revive$"
									 "/TAGS$"
									 "^/var/folders/.+$"
									 (lambda (file)
									   (file-in-directory-p file
															package-user-dir)))))
  :buildin t)

(vwe@lib--pkg paren
  :init ((add-hook 'after-init-hook #'show-paren-mode)
		 (define-advice show-paren-function
			 (:around (fn) fix-show-paren-function)
		   "Highlight enclosing parens."
		   (cond ((looking-at-p "\\s(") (funcall fn))
				 (t (save-excursion
					  (ignore-errors (backward-up-list))
					  (funcall fn))))))
  :variable ((setq show-paren-style 'expression) nil)
  :bullet t)

(vwe@lib--pkg autorevert
  :init ((add-hook 'after-init-hook #'global-auto-revert-mode))
  :buildin t)

(vwe@lib--pkg so-long
  :init ((add-hook 'after-init-hook #'global-so-long-mode))
  :buildin t)

(vwe@lib--pkg delsel
  :init ((add-hook 'after-init-hook #'delete-selection-mode))
  :buildin t)

;;; depand pkgs
;;
;; `vwe-modeline'
;;
(vwe@lib--pkg vwe-modeline
  :init ((autoload 'vwe-modeline-mode (vwe@lib--path-vwe-site-lisp "vwe/vwe-modeline/vwe-modeline.el" t) "Vwe modeline mode" t t)
		 (setq-default mode-line-format nil))
  :config ((add-hook 'after-init-hook (lambda () (when vwe@custom--modeline-show? (vwe-modeline-mode)))))
  :variable ((setq vwe-modeline--buffer-filter-list vwe@custom--modeline--hide-list
				   vwe-modeline--default-format mode-line-format))
  :path (vwe@lib--path-vwe-site-lisp "vwe/vwe-modeline"))

;;
;; `vwe-tray'
;;
(vwe@lib--pkg vwe-tray
  :init ((autoload 'vwe-tray-mode (vwe@lib--path-vwe-site-lisp "vwe/vwe-tray/vwe-tray.el" t) "Vwe tray mode" t t)
		 (add-hook 'after-init-hook (lambda () (when (or vwe@custom--tray-show? (not (display-graphic-p))) (vwe-tray-mode)))))
  :path (vwe@lib--path-vwe-site-lisp "vwe/vwe-tray"))

;;
;; `vwe-headerline'
;;
(vwe@lib--pkg vwe-headerline
  :init ((autoload 'vwe-headerline-mode (vwe@lib--path-vwe-site-lisp "vwe/vwe-headerline/vwe-headerline.el" t) "Vwe headerline mode" t t)
		 (add-hook 'after-init-hook (lambda () (when (or vwe@custom--headerline-show? (not (display-graphic-p))) (vwe-headerline-mode)))))
  :variable ((setq vwe-headerline--buffer-filter-list vwe@custom--buffer-filter-list))
  :path (vwe@lib--path-vwe-site-lisp "vwe/vwe-headerline"))

;;
;; `savehist' 保存buffer历史记录
;;
(vwe@lib--pkg savehist
  :init ((add-hook 'after-init-hook #'savehist-mode))
  :variable ((setq enable-recursive-minibuffers t
				   savehist-file (vwe@lib--path-cache "savehist/history" t)
				   history-length 1000
				   savehist-autosave-interval 5
				   savehist-additional-variables '(mark-ring
												   global-mark-ring
												   search-ring
												   regexp-search-ring
												   extended-command-history))))

;;
;; `ibuffer'
;;
(vwe@lib--pkg ibuffer
  :init ((add-hook 'ibuffer-mode-hook
  				   (lambda ()
  					 (unless (eq ibuffer-sorting-mode 'filename/process)
  					   (ibuffer-do-sort-by-filename/process))
  					 (ibuffer-switch-to-saved-filter-groups "default"))))
  :variable ((setq ibuffer-expert t
  				   ibuffer-show-empty-filter-groups nil
  				   ibuffer-display-summary nil
				   ibuffer-saved-filter-groups
  				   (quote (("default"
							("Lisp" (or (mode . emacs-lisp-mode) (mode . lisp-mode)))
							("CPP" (or (mode . cperl-mode) (mode . c-mode) (mode . c++-mode) (mode . objc-mode)))
							("Build" (or (mode . cmake-mode)))
							("Java" (or (mode . java-mode) (mode . scala-mode)))
							("Python" (or (mode . python-mode)))
							("Ruby" (or (mode . ruby-mode)))
							("DB" (or (mode . sql-mode)))
							("Golang" (or (mode . go-mode)))
							("Rust" (or (mode . rust-mode) ))
							("Web" (or (mode . web-mode) (mode . js2-mode) (mode . css-mode) (mode . scss-mode) (mode . javascript-mode) (mode . rjsx-mode) (mode . lua-mode) (mode . json-mode)))
							("Assembly" (or (mode . asm-mode) (mode . nasm-mode)))
							("Shell" (or (mode . sh-mode)))
  							("Dired" (or (mode . dired-mode) (mode . sr-mode)))
  							("Erc" (mode . erc-mode))
  							("Edit" (or (name . "^\\*Calendar\\*$") (name . "^diary$") (mode . muse-mode) (mode . org-mode) (mode . org-agenda-mode) (mode . text-mode) (mode . yaml-mode)))
  							("Buffer" (or (name . "^\\*scratch\\*$") (name . "^\\*Messages\\*$")))
							("Email" (or (name . "^\\*mu4e-headers\\*$")))
  							("Mesasge" (or (mode . message-mode) (mode . bbdb-mode) (mode . mail-mode) (mode . gnus-group-mode) (mode . gnus-summary-mode) (mode . gnus-article-mode) (name . "^\\.bbdb$") (name . "^\\.newsrc-dribble")))))))))

;;
;; `transient'
;;
(vwe@lib--pkg transient
  :variable ((setq transient-history-file (vwe@lib--path-cache "transient/history.el" t)
				   transient-levels-file (vwe@lib--path-cache "transient/levels.el" t)
				   transient-values-file (vwe@lib--path-cache "transient/values.el" t))))

;;
;; `htmlize' buffer文本转换为html
;;
(vwe@lib--pkg htmlize)

;;
;; `which-key'
;;
(vwe@lib--pkg which-key
  :init ((add-hook 'after-init-hook #'which-key-mode))
  :variable((setq which-key-use-C-h-commands nil)))

;;
;; `ivy'
;;
;; (vwe@lib--pkg ivy
;;   :init ((add-hook 'after-init-hook #'ivy-mode))
;;   :config (;;
;; 		   ;; `counsel'
;; 		   ;;
;; 		   (vwe@lib--pkg counsel
;; 			 :config ((if (executable-find "rg")
;; 						  (setq counsel-grep-base-command
;; 								"rg -i -M 120 --no-heading --line-number --color never %s %s"
;; 								counsel-rg-base-command
;; 								"rg -i -M 120 --no-heading --line-number --color never %s ."))))

;; 		   ;;
;; 		   ;; `swiper'
;; 		   ;;
;; 		   (vwe@lib--pkg swiper))
;;   :variable ((setq ivy-use-virtual-buffers t
;; 				   ivy-height 10
;; 				   ivy-initial-inputs-alist nil
;; 				   ivy-count-format "%d/%d"
;; 				   ivy-re-builders-alist `((t . ivy--regex-ignore-order)
;; 										   (t . orderless-ivy-re-builder))
;; 				   enable-recursive-minibuffers t)))

;;
;; `yasnippet' include `yasnippet-snippets' `auto-yasnippet'
;;
(vwe@lib--pkg yasnippet
  :init ((add-hook 'prog-mode-hook #'yas-global-mode)
		 (add-hook 'org-mode-hook #'yas-global-mode))
  :config ((defun vwe@pkg--yas-expand-snippet ()
			 "Replace text in yasnippet template."
			 (yas-expand-snippet (buffer-string) (point-min) (point-max)))
		   (yas-reload-all)
		   ;;
		   ;; `yasnippet-snippets'
		   ;;
		   (vwe@lib--pkg yasnippet-snippets)
		   ;;
		   ;; `auto-yasnippet'
		   ;;
		   (vwe@lib--pkg auto-yasnippet))
  :variable ((setq yas-snippet-dirs (list (vwe@lib--path-vwe-etc "snippets")))))

;;
;; `autoinsert'
;;
(vwe@lib--pkg autoinsert
  :init ((add-hook 'after-init-hook #'auto-insert-mode))
  :config ((define-auto-insert
			 "\\.org$"
			 ["default-reveal-org.org"
			  (lambda ()
				(when (fboundp 'yas-expand-snippet) (vwe@pkg--yas-expand-snippet)))])
		   (define-auto-insert
			 "\\.txt$"
			 ["default-reveal-org.org"
			  (lambda ()
				(when (fboundp 'yas-expand-snippet) (vwe@pkg--yas-expand-snippet)))]))
  :variable ((setq auto-insert nil
				   auto-insert-query nil
				   auto-insert-directory (vwe@lib--path-vwe-etc "templates"))))

;;
;; `company'
;;
(vwe@lib--pkg company
  :init ((defvar vwe@pkg--company-general-backends
		   '(company-files ;; file path
			 company-keywords ;; 在语言中具有特定含义的单词
			 company-dabbrev ;; 在打开的缓冲区的内容中搜索完成候选
			 company-dabbrev-code ;; 在打开的缓冲区的内容中搜索完成候选
			 company-semantic ;;
			 company-capf)
		   "General company backends.")

		 (defvar vwe@pkg--company-with-yas-p
		   nil
		   "Company backends with yas.")

		 (defun vwe@pkg--company-make-mode-local-backends (backends &optional ignores)
		   "Make mode local BACKENDS."
		   (let* ((bks (progn
						 (unless (listp backends) (setq backends (list backends)))
						 (append backends vwe@pkg--company-general-backends))))
			 (when (and ignores (listp ignores))
			   (dolist (ibk ignores)
				 (setq bks (delq ibk bks))))

			 (set (make-local-variable 'company-backends)
				  (if vwe@pkg--company-with-yas-p
					  (mapcar #'vwe@pkg--company-backends-with-yas bks)
					bks))))

		 (defun vwe@pkg--company-backends-with-yas (backend)
		   "Company backends with yasnippet."
		   (if (or (and (listp backend) (member 'company-yasnippet backend)))
			   backend
			 (append (if (consp backend) backend (list backend))
					 '(:with company-yasnippet))))

		 (add-hook 'prog-mode-hook #'global-company-mode))
  :config ((vwe@lib--keymap-set company-search-map
								'(("C-n" company-select-next)
								  ("C-p" company-select-previous)))
		   (vwe@lib--keymap-set company-active-map
								'(("C-n" company-select-next)
								  ("C-p" company-select-previous)
								  ("<tab>" company-complete-common-or-cycle)))

		   ;; (add-to-list 'company-transformers #'delete-dups)

		   ;;
		   ;; `company-prescient'
		   ;;
		   ;; (vwe@lib--pkg company-prescient
		   ;;   :init ((add-hook 'company-mode-hook #'company-prescient-mode)))
		   ;;
		   ;; `company-quickhelp'
		   ;;
		   (vwe@lib--pkg company-quickhelp
			 :init ((add-hook 'company-mode-hook #'company-quickhelp-mode))
			 :variable ((setq company-quickhelp-delay 1.5)))
		   ;;
		   ;; `company-quickhelp-terminal'
		   ;;
		   (vwe@lib--pkg company-quickhelp-terminal
			 :init ((add-hook 'company-mode-hook #'company-quickhelp-terminal-mode)))
		   ;;
		   ;; `company-box'
		   ;;
		   (vwe@lib--pkg company-box
			 :init ((add-hook 'company-mode-hook #'company-box-mode))
			 :variable ((setq company-box-enable-icon t
							  company-box-backends-colors nil
							  company-box-show-single-candidate t
							  company-box-max-candidates 50
							  company-box-doc-delay 1.5))))
  :variable ((setq company-tooltip-align-annotations t
				   company-tooltip-limit 12
				   company-idle-delay 0
				   company-require-match nil
				   company-minimum-prefix-length 1
				   company-show-numbers t
				   company-echo-delay (if (display-graphic-p) nil 0)

				   company-transformers '(delete-dups
										  delete-consecutive-dups
										  company-sort-by-occurrence)

				   company-dabbrev-downcase t
				   company-dabbrev-ignore-case t

				   vwe@pkg--company-with-yas-p nil
				   company-backends (mapcar #'vwe@pkg--company-backends-with-yas
											vwe@pkg--company-general-backends))))

;;
;; `vertico'
;;
(vwe@lib--pkg vertico
  :init ((add-hook 'after-init-hook #'vertico-mode)))

;;
;; `orderless'
;;
(vwe@lib--pkg orderless
  :variable ((setq completion-styles '(orderless)))
  :undefer t)

;;
;; `marginalia'
;;
(vwe@lib--pkg marginalia
  :init ((add-hook 'after-init-hook #'marginalia-mode)))

;;
;; `consult'
;;
(vwe@lib--pkg consult
  :variable ((setq consult-narrow-key "<"
				   consult-async-min-input 2)))

;;
;; `embark'
;;
(vwe@lib--pkg embark
  :init	  ((setq prefix-help-command 'embark-prefix-help-command))
  :variable ((setq embark-verbose-indicator-display-action
				   '((display-buffer-at-bottom)
					 (window-parameters (mode-line-format . none))
					 (window-height . fit-window-to-buffer)))
			 (with-eval-after-load 'consult
			   (progn
				 (define-key embark-identifier-map "R" #'consult-ripgrep)
				 (define-key embark-identifier-map (kbd "C-s") #'consult-line)
				 (define-key embark-file-map (kbd "E") #'consult-file-externally)
				 (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))))
  nil t)

;;
;; `mmm-mode'
;;
(vwe@lib--pkg mmm-mode
  :config ((mmm-add-mode-ext-class nil "\\.html\\'" 'js2-mode))
  :variable ((setq mmm-global-mode 'maybe)))

;;
;; `hungry-delete' 删除多余的空格
;;
(vwe@lib--pkg hungry-delete
  :init ((add-hook 'after-init-hook #'global-hungry-delete-mode))
  :variable ((setq-default hungry-delete-chars-to-skip " \t\f\v")))

;;
;; `expand-region' 快捷选中
;;
(vwe@lib--pkg expand-region)

;;
;; `mwim' 根据代码移动光标
;;
(vwe@lib--pkg mwim)

;;
;; `smartparens'
;;
(vwe@lib--pkg smartparens
  :init ((add-hook 'prog-mode-hook #'smartparens-mode))
  :config ((sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
		   (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)
		   (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
		   (sp-local-pair 'lisp-interaction-mode "`" nil :actions nil)
		   (sp-local-pair 'web-mode "<" ">")))

;;
;; `iedit' 批量编辑
;;
(vwe@lib--pkg iedit
  :init ((vwe@lib--keymap-set isearch-mode-map
							  '(("C-;" iedit-mode-from-isearch)))))

;;
;; `comment-dwim-2' 注释
;;
(vwe@lib--pkg comment-dwim-2)

;;
;; `esup' test startup time
;;
(vwe@lib--pkg esup
  :variable ((setq esup-child-max-depth 1)))

;;
;; `url-cache'
;;
(vwe@lib--pkg url-cache
  :variable ((setq url-cache-directory (vwe@lib--path-cache "url/cache")))
  :buildin t)

;;
;; `format-all'
;;
(vwe@lib--pkg format-all
  :init ((add-hook 'prog-mode-hook #'format-all-mode)))

;;
;; `command-log-mode'
;;
(vwe@lib--pkg command-log-mode)

;;
;; `browse-kill-ring'
;;
(vwe@lib--pkg browse-kill-ring
  :config ((vwe@lib--keymap-set browse-kill-ring-mode-map
								'(("i" browse-kill-ring-insert-move-and-quit))))
  :variable ((setq browse-kill-ring-highlight-current-entry t
				   browse-kill-ring-highlight-inserted-item 'pulse)))

;;
;; `undo-tree'
;;
(vwe@lib--pkg undo-tree
  :init ((add-hook 'after-init-hook #'global-undo-tree-mode))
  :variable ((setq undo-tree-visualizer-timestamps t
				   undo-tree-visualizer-diff t
				   undo-tree-enable-undo-in-region nil
				   undo-tree-auto-save-history t
				   undo-tree-history-directory-alist `(("." . ,(vwe@lib--path-cache "undotree/hist"))))))


;;
;; `treemacs'
;;
(vwe@lib--pkg treemacs
  :config ((vwe@lib--keymap-global-set '(("M-0" treemacs-select-window)))
		   (treemacs-follow-mode t)
		   (treemacs-filewatch-mode t)
		   (treemacs-fringe-indicator-mode 'always)
		   (pcase (cons (not (null (executable-find "git")))
						(not (null treemacs-python-executable)))
			 (`(t . t)
			  (treemacs-git-mode 'deferred))
			 (`(t . _)
			  (treemacs-git-mode 'simple))))
  :variable ((setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
				   treemacs-deferred-git-apply-delay        0.5
				   treemacs-directory-name-transformer      #'identity
				   treemacs-display-in-side-window          t
				   treemacs-eldoc-display                   'simple
				   treemacs-file-event-delay                5000
				   treemacs-file-extension-regex            treemacs-last-period-regex-value
				   treemacs-file-follow-delay               0.2
				   treemacs-file-name-transformer           #'identity
				   treemacs-follow-after-init               t
				   treemacs-expand-after-init               t
				   treemacs-find-workspace-method           'find-for-file-or-pick-first
				   treemacs-git-command-pipe                ""
				   treemacs-goto-tag-strategy               'refetch-index
				   treemacs-indentation                     2
				   treemacs-indentation-string              " "
				   treemacs-is-never-other-window           nil
				   treemacs-max-git-entries                 5000
				   treemacs-missing-project-action          'ask
				   treemacs-move-forward-on-expand          nil
				   treemacs-no-png-images                   nil
				   treemacs-no-delete-other-windows         t
				   treemacs-project-follow-cleanup          nil
				   treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
				   treemacs-position                        'left
				   treemacs-read-string-input               'from-child-frame
				   treemacs-recenter-distance               0.1
				   treemacs-recenter-after-file-follow      nil
				   treemacs-recenter-after-tag-follow       nil
				   treemacs-recenter-after-project-jump     'always
				   treemacs-recenter-after-project-expand   'on-distance
				   treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
				   treemacs-show-cursor                     nil
				   treemacs-show-hidden-files               t
				   treemacs-silent-filewatch                nil
				   treemacs-silent-refresh                  nil
				   treemacs-sorting                         'alphabetic-asc
				   treemacs-select-when-already-in-treemacs 'move-back
				   treemacs-space-between-root-nodes        t
				   treemacs-tag-follow-cleanup              t
				   treemacs-tag-follow-delay                1.5
				   treemacs-text-scale                      nil
				   treemacs-user-mode-line-format           nil
				   treemacs-user-header-line-format         nil
				   treemacs-wide-toggle-width               70
				   treemacs-width                           35
				   treemacs-width-increment                 1
				   treemacs-width-is-initially-locked       t
				   treemacs-workspace-switch-cleanup        nil)))

;;
;; `sudo-edit'
;;
(vwe@lib--pkg sudo-edit)

;;
;; `epa-file'
;;
(vwe@lib--pkg epa-file
  :config ((setenv "GPG_AGENT_INFO" nil)
		   (epa-file-enable))
  :variable ((setq epa-file-inhibit-auto-save nil))
  :buildin t)

;;
;; `exec-path-from-shell'
;;
(vwe@lib--pkg exec-path-from-shell
  :config ((when (daemonp)
			 (exec-path-from-shell-initialize))
		   (when (memq window-system '(mac ns x))
			 (exec-path-from-shell-initialize)))
  :variable ((setq exec-path-from-shell-check-startup-files nil
				   exec-path-from-shell-variables '("PATH")
				   exec-path-from-shell-arguments nil)))

;;
;; `tramp'
;;
(vwe@lib--pkg tramp
  :variable ((setq tramp-auto-save-directory (vwe@lib--path-cache "tramp")
				   tramp-persistency-file-name (vwe@lib--path-cache "tramp/tramp" t)
				   tramp-backup-directory-alist (vwe@lib--path-cache "tramp"))))

;;
;; `clipetty'
;;
(vwe@lib--pkg clipetty
  :init ((add-hook 'after-init-hook (lambda()
									  (unless (display-graphic-p)
										(global-clipetty-mode))))))


;;
;; `bookmark'
;;
(vwe@lib--pkg bookmark
  :init ((setq bookmark-annotation-name vwe@custom--user-name
			   bookmark-save-flag 1
			   bookmark-default-file (vwe@lib--path-cache "bookmark/bookmarks" t)))
  :config ((defadvice bookmark-jump (after bookmark-jump activate)
			 (let ((latest (bookmark-get-bookmark bookmark)))
			   (setq bookmark-alist (delq latest bookmark-alist))
			   (add-to-list 'bookmark-alist latest)))))

;;
;; `dired'
;;
(vwe@lib--pkg dired
  :init ((setq dired-recursive-deletes 'always
			   dired-recursive-copies 'always
			   dired-dwin-target 1))
  :config ((put 'dired-find-alternate-file 'disabled nil)
		   ;;
		   ;; `diredfl' 丰富dired颜色
		   ;;
		   (vwe@lib--pkg diredfl
			 :init ((diredfl-global-mode 1)))

		   ;;
		   ;; `find-by-pinyin-dirred' 拼音搜索
		   ;;
		   (vwe@lib--pkg find-by-pinyin-dired)

		   ;;
		   ;; `dired-filter'
		   ;;
		   (vwe@lib--pkg dired-filter
			 :init ((add-hook 'dired-mode-hook #'dired-filter-group-mode))
			 :variable ((setq dired-filter-group-saved-groups
							  (quote
							   (("default"
								 ("Lisp" (extension . "el"))
								 ("C" (extension "c" "cpp" "h" "hpp"))
								 ("Java" (extension "java" "class"))
								 ("Python" (extension "py" "pyc"))
								 ("Ruby" (extension . "rb"))
								 ("Web" (extension "js" "html" "css"))
								 ("Json" (extension . "json"))
								 ("Rust" (extension . "rs"))
								 ("Shell" (extension . "sh"))
								 ("Golang" (extension . "go"))
								 ("Assembly" (extension "asm" "lst" "s"))
								 ("PDF" (extension . "pdf"))
								 ("LaTeX" (extension "tex" "bib"))
								 ("Org" (extension . "org"))
								 ("Log" (extension . "log"))
								 ("Profile" (extension "xml" "xsd" "yaml" "yml" "config" "conf"))
								 ("Markdown" (extension "md" "markdown" "mkd"))
								 ("Archives" (extension "zip" "rar" "gz" "bz2" "tar"))
								 ("Media" (extension "mp4" "avi" "wmv" "flv" "mov" "3gp" "rmvb" "mkv" "flvc" "mp3" "aac" "ape"))
								 ("Picture" (extension "jpg" "jepg" "png" "gif")))))))
			 :undefer t))
  :buildin t)

;;
;; `ediff'
;;
(vwe@lib--pkg ediff
  :variable ((setq ediff-window-setup-function 'ediff-setup-windows-plain
				   ediff-split-window-function 'split-window-horizontally
				   ediff-merge-split-window-function 'split-window-horizontally)))

;;
;; `diff-hl'
;;
(vwe@lib--pkg diff-hl
  :init ((add-hook 'prog-mode-hook #'global-diff-hl-mode)
		 (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

;;
;; `imenu-list'
;;
(vwe@lib--pkg imenu-list)

;;
;; `magit'
;;
(vwe@lib--pkg magit
  :config ((add-hook 'after-save-hook #'magit-after-save-refresh-status)
		   (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
		   (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

;;
;; `vwe-key'
;;
(vwe@lib--pkg vwe-key
  :init ((add-hook 'after-init-hook #'vwe-key-mode))
  :path (vwe@lib--path-vwe-site-lisp "vwe/vwe-key")
  :undefer t)

;;
;; `vwe-package'
;;
(vwe@lib--pkg vwe-package
  :init ((autoload
		   'vwe-package--update-packages
		   (vwe@lib--path-vwe-site-lisp "vwe/vwe-package/vwe-package.el" t)
		   "Vwe update packages."
		   t t))
  :path (vwe@lib--path-vwe-site-lisp "vwe/vwe-package"))

;;
;; `vwe-editor'
;;
(vwe@lib--pkg vwe-editor
  :init ((autoload
		   'vwe-editor-mode
		   (vwe@lib--path-vwe-site-lisp "vwe/vwe-editor/vwe-editor.el" t)
		   "Vwe editor mode."
		   t t)
		 (add-hook 'after-init-hook  #'vwe-editor-mode))
  :variable ((setq vwe-editor--ignore-func '(browse-kill-ring
											 ediff-files
											 org-capture)))
  :path (vwe@lib--path-vwe-site-lisp "vwe/vwe-editor"))

;;
;; `vwe-term'
;;
(vwe@lib--pkg vwe-term
  :init ((autoload 'vwe-terminal (vwe@lib--path-vwe-site-lisp "vwe/vwe-term/vwe-term.el" t) "Vwe term mode." t t))
  :path (vwe@lib--path-vwe-site-lisp "vwe/vwe-term"))

;;
;; `vwe-proxy'
;;
(vwe@lib--pkg vwe-proxy
  :init ((autoload 'vwe-proxy--enable (vwe@lib--path-vwe-site-lisp "vwe/vwe-proxy/vwe-proxy.el" t) "Vwe proxy mode." t t)
		 (autoload 'vwe-proxy--enable-global (vwe@lib--path-vwe-site-lisp "vwe/vwe-proxy/vwe-proxy.el" t) "Vwe global proxy mode." t t))
  :path (vwe@lib--path-vwe-site-lisp "vwe/vwe-proxy"))

;;
;; `vwe-layout'
;;
(vwe@lib--pkg vwe-layout
  :init ((autoload 'vwe-layout--enable (vwe@lib--path-vwe-site-lisp "vwe/vwe-layout/vwe-layout.el" t) "Vwe global layout mode." t t)
		 (autoload 'vwe-layout--text-scale-increase (vwe@lib--path-vwe-site-lisp "vwe/vwe-layout/vwe-layout.el" t) "Vwe global layout mode." t t)
		 (autoload 'vwe-layout--text-scale-decrease (vwe@lib--path-vwe-site-lisp "vwe/vwe-layout/vwe-layout.el" t) "Vwe global layout mode." t t)
		 (autoload 'vwe-layout--text-scale-adjust (vwe@lib--path-vwe-site-lisp "vwe/vwe-layout/vwe-layout.el" t) "Vwe global layout mode." t t)
		 (autoload 'vwe-layout--zoom-type-toggle (vwe@lib--path-vwe-site-lisp "vwe/vwe-layout/vwe-layout.el" t) "Vwe global layout mode." t t)
		 (autoload 'vwe-layout--window-height-enlarge (vwe@lib--path-vwe-site-lisp "vwe/vwe-layout/vwe-layout.el" t) "Vwe global layout mode." t t)
		 (autoload 'vwe-layout--window-height-shrink (vwe@lib--path-vwe-site-lisp "vwe/vwe-layout/vwe-layout.el" t) "Vwe global layout mode." t t)
		 (autoload 'vwe-layout--window-width-enlarge (vwe@lib--path-vwe-site-lisp "vwe/vwe-layout/vwe-layout.el" t) "Vwe global layout mode." t t)
		 (autoload 'vwe-layout--window-width-shrink (vwe@lib--path-vwe-site-lisp "vwe/vwe-layout/vwe-layout.el" t) "Vwe global layout mode." t t)
		 (autoload 'vwe-layout-config-mode (vwe@lib--path-vwe-site-lisp "vwe/vwe-layout/vwe-layout.el" t) "Vwe global layout mode." t t)
		 (add-hook 'after-init-hook #'vwe-layout-config-mode))
  :path (vwe@lib--path-vwe-site-lisp "vwe/vwe-layout"))

;;
;; `vwe-search'
;;
(vwe@lib--pkg vwe-search
  :init ((autoload 'vwe-search--rg (vwe@lib--path-vwe-site-lisp "vwe/vwe-search/vwe-search.el" t) "Vwe global search mode." t t))
  :path (vwe@lib--path-vwe-site-lisp "vwe/vwe-search"))

;;
;; `vwe-move'
;;
(vwe@lib--pkg vwe-move
  :init ((autoload 'vwe-move-mode (vwe@lib--path-vwe-site-lisp "vwe/vwe-move/vwe-move.el" t) "Vwe move mode." t t)
		 (autoload 'vwe-move-zoom-point--toggle (vwe@lib--path-vwe-site-lisp "vwe/vwe-move/vwe-move.el" t) "Vwe move zoom point." t t)
		 (add-hook 'after-init-hook #'vwe-move-mode))
  :path (vwe@lib--path-vwe-site-lisp "vwe/vwe-move"))

;;
;; `vwe-edit'
;;
(vwe@lib--pkg vwe-edit
  :init ((autoload 'vwe-edit-region--mark-edit (vwe@lib--path-vwe-site-lisp "vwe/vwe-edit/vwe-edit.el" t) "Vwe edit mode." t t)
		 (autoload 'vwe-edit-toggle-case--upper-case (vwe@lib--path-vwe-site-lisp "vwe/vwe-edit/vwe-edit.el" t) "Vwe edit mode." t t)
		 (autoload 'vwe-edit-toggle-case--lower-case (vwe@lib--path-vwe-site-lisp "vwe/vwe-edit/vwe-edit.el" t) "Vwe edit mode." t t)
		 (autoload 'vwe-edit-toggle-case--capitalize (vwe@lib--path-vwe-site-lisp "vwe/vwe-edit/vwe-edit.el" t) "Vwe edit mode." t t)
		 (autoload 'vwe-edit-toggle-case--select-convert (vwe@lib--path-vwe-site-lisp "vwe/vwe-edit/vwe-edit.el" t) "Vwe edit mode." t t)
		 (autoload 'vwe-edit-bound-mode (vwe@lib--path-vwe-site-lisp "vwe/vwe-edit/vwe-edit.el" t) "Vwe edit bound mode." t t)
		 (autoload 'wve-edit-bound--temp-show (vwe@lib--path-vwe-site-lisp "vwe/vwe-edit/vwe-edit.el" t) "Vwe edit bound mode." t t)
		 (autoload 'vwe-edit-bound--draw (vwe@lib--path-vwe-site-lisp "vwe/vwe-edit/vwe-edit.el" t) "Vwe edit bound mode." t t)
		 (autoload 'vwe-edit--open-newline-above (vwe@lib--path-vwe-site-lisp "vwe/vwe-edit/vwe-edit.el" t) "Vwe edit previous newline." t t)
		 (autoload 'vwe-edit--open-newline-below (vwe@lib--path-vwe-site-lisp "vwe/vwe-edit/vwe-edit.el" t) "Vwe edit next line." t t))
  :path (vwe@lib--path-vwe-site-lisp "vwe/vwe-edit"))

;;
;; `vwe-tags'
;;
(vwe@lib--pkg vwe-tags
  :init ((autoload 'vwe-tags-mode (vwe@lib--path-vwe-site-lisp "vwe/vwe-tags/vwe-tags.el" t) "Vwe tags mode." t t)
		 (autoload 'vwe-tags--create-tags (vwe@lib--path-vwe-site-lisp "vwe/vwe-tags/vwe-tags.el" t) "Create tags ." t t)
		 (autoload 'vwe-tags--refresh-tags (vwe@lib--path-vwe-site-lisp "vwe/vwe-tags/vwe-tags.el" t) "Refresh tags." t t)
		 (add-hook 'prog-mode-hook #'vwe-tags-mode))
  :path(vwe@lib--path-vwe-site-lisp "vwe/vwe-tags"))

;;
;; `vwe-project'
;;
(vwe@lib--pkg vwe-project
  :init ((autoload 'vwe-project-mode (vwe@lib--path-vwe-site-lisp "vwe/vwe-project/vwe-project.el" t) "Vwe project mode." t t)
		 (autoload 'vwe-project--add-project (vwe@lib--path-vwe-site-lisp "vwe/vwe-project/vwe-project.el" t) "add project." t t)
		 (add-hook 'prog-mode-hook #'vwe-project-mode))
  :config ((vwe-project--add-project user-emacs-directory "emacs"))
  :path (vwe@lib--path-vwe-site-lisp "vwe/vwe-project"))

;;
;; `vwe-panren'
;;
(vwe@lib--pkg vwe-panren
  :init ((autoload 'vwe-paren-mode (vwe@lib--path-vwe-site-lisp "vwe/vwe-paren/vwe-paren.el" t) "Vwe paren mode." t t)
		 (autoload 'vwe-paren--toggle (vwe@lib--path-vwe-site-lisp "vwe/vwe-paren/vwe-paren.el" t) "Vwe paren mode." t t)
		 (add-hook 'after-init-hook #'vwe-paren-mode)
		 (add-hook 'prog-mode-hook #'vwe-paren--toggle))
  :path(vwe@lib--path-vwe-site-lisp "vwe/vwe-paren"))

;;
;; `vwe-mark'
;;
(vwe@lib--pkg vwe-mark
  :init ((autoload 'vwe-mark-mode (vwe@lib--path-vwe-site-lisp "vwe/vwe-mark/vwe-mark.el" t) "Vwe mark mode." t t)
		 (autoload 'vwe-mark-multi-edit--chars (vwe@lib--path-vwe-site-lisp "vwe/vwe-mark/vwe-mark.el" t) "Vwe mark multi edit chars." t t)
		 (autoload 'vwe-mark-multi-edit--words (vwe@lib--path-vwe-site-lisp "vwe/vwe-mark/vwe-mark.el" t) "Vwe mark multi edit words." t t)
		 (autoload 'vwe-mark-multi-edit--symbols (vwe@lib--path-vwe-site-lisp "vwe/vwe-mark/vwe-mark.el" t) "Vwe mark multi edit symbols." t t)
		 (autoload 'vwe-mark-multi-edit--lines (vwe@lib--path-vwe-site-lisp "vwe/vwe-mark/vwe-mark.el" t) "Vwe mark multi edit lines." t t)
		 (autoload 'vwe-mark-rect--mark (vwe@lib--path-vwe-site-lisp "vwe/vwe-mark/vwe-mark.el" t) "Vwe mark multi edit rect mark." t t)
		 (autoload 'vwe-mark-multi-edit--replace (vwe@lib--path-vwe-site-lisp "vwe/vwe-mark/vwe-mark.el" t) "Vwe mark multi edit replace." t t))
  :path(vwe@lib--path-vwe-site-lisp "vwe/vwe-paren"))

(vwe@lib--log "Initialization of General configuration is complete.")

(provide 'vwe-general)
;;; vwe-general.el ends here
