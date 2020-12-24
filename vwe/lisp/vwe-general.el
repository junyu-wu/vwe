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

(with-eval-after-load 'yasnippet
  (defun vwe@pkg--yas-expand ()
	"Replace text in yasnippet template."
	(yas-expand-snippet (buffer-string) (point-min) (point-max))))

;; ***************************************************************************
;; config
;; ***************************************************************************

(use-package mum-modeline
 :load-path
 (lambda ()
   (vwe@lib--path-vwe-site-lisp "mum/mum-modeline"))
 :hook
 (after-init . mum-modeline-mode)
 :init
 (setq mum-modeline--buffer-filter-list vwe@custom--modeline--hide-list))

(use-package mum-headerline
  :load-path
  (lambda ()
	(vwe@lib--path-vwe-site-lisp "mum/mum-headerline"))
  :hook
  (after-init . mum-headerline-mode)
  :init
  (setq mum-headerline--buffer-filter-list vwe@custom--buffer-filter-list))

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
  				 ("Mesasge" (or (mode . message-mode) (mode . bbdb-mode) (mode . mail-mode) (mode . gnus-group-mode) (mode . gnus-summary-mode) (mode . gnus-article-mode) (name . "^\\.bbdb$") (name . "^\\.newsrc-dribble")))))))
  (add-hook 'ibuffer-mode-hook
  			(lambda ()
  			  (unless (eq ibuffer-sorting-mode 'filename/process)
  				(ibuffer-do-sort-by-filename/process))
  			  (ibuffer-switch-to-saved-filter-groups "default"))))

;; buffer文本转换为html
(use-package htmlize)

(use-package which-key
  :diminish
  (which-key-mode . nil)
  :hook
  (after-init . which-key-mode))

(use-package ivy
  :diminish (ivy-mode . nil)
  :hook
  (after-init . ivy-mode)
  :init
  (setq ivy-use-virtual-buffers t
		ivy-height 10
		ivy-initial-inputs-alist nil
		ivy-count-format "%d/%d"
		ivy-re-builders-alist `((t . ivy--regex-ignore-order))
		enable-recursive-minibuffers t))

(use-package counsel
  :diminish
  (counsel-mode . "")
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file))
  :config
  (if (executable-find "rg")
      (setq counsel-grep-base-command
            "rg -i -M 120 --no-heading --line-number --color never %s %s"
            counsel-rg-base-command
            "rg -i -M 120 --no-heading --line-number --color never %s .")))

(use-package swiper
  :bind
  (:map global-map
		("C-s" . swiper)))

(use-package yasnippet
  :diminish (yas-minor-mode . nil)
  :hook
  (prog-mode . yas-global-mode)
  :init
  (setq yas-snippet-dirs (list (vwe@lib--path-vwe-etc "snippets")))
  :config
  (yas-reload-all)

  (use-package yasnippet-snippets
    :after (yasnippet))

  (use-package auto-yasnippet
    :after (yasnippet)))

(use-package company
  :diminish
  (company-mode . nil)
  :commands
  company-abort
  :hook
  (prog-mode . global-company-mode)
  :defines
  (company-dabbrev-ignore-case company-dabbrev-downcase)
  :bind
  (("M-/" . company-complete)
   ("C-M-/" . company-yasnippet)
   :map company-search-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   :map company-active-map
   ("C-p" . company-select-previous)
   ("C-n" . company-select-next)
   ("<tab>" . company-complete-common-or-cycle))
  :init
  (setq company-tooltip-align-annotations t
        company-tooltip-limit 12
        company-idle-delay 0
        company-echo-delay (if (display-graphic-p) nil 0)
        company-minimum-prefix-length 1
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
		company-show-numbers t
		;; company-global-modes '(not erc-mode message-mode help-mode
		;; gud-mode eshell-mode shell-mode)
        company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend)
		company-backends '((company-files
							company-yasnippet
							company-keywords
							company-capf)
						   (company-abbrev company-dabbrev)))
  :config
  ;; 排序与过滤
  (use-package company-prescient
	:init
	(company-prescient-mode 1))

  ;; 显示文档说明
  (use-package company-quickhelp
	:defines company-quickhelp-delay
	:hook
	(global-company-mode . company-quickhelp-mode)
	:bind
	(:map company-active-map
          ([remap company-show-doc-buffer] . company-quickhelp-manual-begin))
	:init
	(setq company-quickhelp-delay 1.5))

  (use-package company-quickhelp-terminal
	:after
	company-quickhelp
	:config
	(company-quickhelp-terminal-mode 1)))

;; 补全显示图标
(use-package company-box
  :diminish
  (company-box-mode)
  :hook
  (company-mode . company-box-mode)
  :init
  (setq company-box-enable-icon t
		company-box-backends-colors nil
		company-box-show-single-candidate t
		company-box-max-candidates 50
		company-box-doc-delay 1.5))

;; 补全弹出框
(use-package company-posframe
  :diminish
  (company-posframe-mode . nil)
  :hook
  (prog-mode . company-posframe-mode)
  :config
  (company-posframe-mode 1))

;; 自动插入内容
(use-package autoinsert
  :ensure nil
  :hook
  (after-init . auto-insert-mode)
  (find-file . auto-insert)
  (emacs-startup . (lambda() (setq auto-insert t)))
  :init
  (setq auto-insert nil
		auto-insert-query nil
		auto-insert-directory (locate-user-emacs-file
							   "assets/templates/"))
  :config
  (auto-insert-mode t)
  (define-auto-insert "\\.org$" ["default-org.org" vwe@pkg--yas-expand])
  (define-auto-insert "\\.py$" ["default_python.py" vwe@pkg--yas-expand])
  (define-auto-insert "\\.rb$" ["default_ruby.rb" vwe@pkg--yas-expand])
  (define-auto-insert "\\.html$" ["default-html.html" vwe@pkg--yas-expand])
  (define-auto-insert "\\.js$" ["default-js.js" vwe@pkg--yas-expand])
  (define-auto-insert "\\.css$" ["default-css.css" vwe@pkg--yas-expand])
  (define-auto-insert "\\.scm$" ["default-scheme.scm" vwe@pkg--yas-expand]))

;; 删除多余的空格
(use-package hungry-delete
  :diminish
  :hook
  (after-init . global-hungry-delete-mode)
  :config
  (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; 快捷选中
(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

;; 跳转到最后一次修改
(use-package goto-chg
  :bind
  ("C-," . goto-last-change))

;; 根据代码移动光标
(use-package mwim
  :bind
  (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
   ([remap move-end-of-line] . mwim-end-of-code-or-line)))

;; 拖拽行、区域、字
(use-package drag-stuff
  :diminish
  :commands drag-stuff-define-keys
  :hook
  (after-init . drag-stuff-global-mode)
  :config
  (drag-stuff-define-keys))

;; 预览指定行
(use-package goto-line-preview
  :bind
  ([remap goto-line] . goto-line-preview))

;; 括号
(use-package smartparens
  :diminish (smartparens-mode . "")
  :hook
  (prog-mode . smartparens-global-mode)
  :bind
  (:map global-map
		("M-'" . sp-up-sexp)
		("M-)" . sp-up-sexp)
		("M-(" . sp-down-sexp)
		("M-C-'" . sp-backward-unwrap-sexp))
  :config
  (smartparens-mode t)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "`" nil :actions nil))

(use-package paren-face)

;; 彩虹括号
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  :config
  (rainbow-delimiters-mode t))

;; 批量编辑
(use-package iedit
  :bind
  (("C-;" . iedit-mode)
   ("C-M-;" . iedit-rectangle-mode)
   :map isearch-mode-map
   ("C-;" . iedit-mode-from-isearch)))

;; 多个光标
(use-package multiple-cursors
  :bind
  (("C-M-[" . mc/edit-beginnings-of-lines)
   ("C-M-]" . mc/edit-ends-of-lines)
   ("C-M-|" . set-rectangular-region-anchor))
  :init
  (setq mc/list-file (vwe@lib--path-cache "mc/.mc-lists.el" t)))

;; 注释
(use-package comment-dwim-2
  :bind
  ([remap comment-dwim] . comment-dwim-2))

;; 清除未使用的空格
(use-package clean-aindent-mode
  :hook
  (prog-mode . clean-aindent-mode)
  :init
  (setq clean-aindent-is-simple-indent t))

;; 字符串大小写切换
(use-package string-inflection
  :bind
  (:map global-map
        ("M-l" . string-inflection-lower-camelcase)
        ("M-u" . string-inflection-upcase)))

(use-package diminish)

;; show battery
(use-package battery
  :ensure nil)

;; test startup time
(use-package esup
  :init
  (setq esup-child-max-depth 0))

;; asynchronous processing
(use-package async)

;; URL cache
(use-package url-cache
  :ensure nil
  :init
  (setq url-cache-directory (vwe@lib--path-cache "url/cache")))

(use-package format-all
  :diminish
  (format-all-mode . nil)
  :hook
  (prog-mode . format-all-mode))

(use-package treemacs
  :bind
  ("M-*" . treemacs)
  :config
  (setq treemacs-collapse-dirs                 (if treemacs-python-executable
												   3 0)
		treemacs-deferred-git-apply-delay      0.5
		treemacs-display-in-side-window        t
		treemacs-eldoc-display                 t
		treemacs-file-event-delay              5000
		treemacs-file-follow-delay             0.2
		treemacs-follow-after-init             t
		treemacs-git-command-pipe              ""
		treemacs-goto-tag-strategy             'refetch-index
		treemacs-indentation                   2
		treemacs-indentation-string            " "
		treemacs-is-never-other-window         nil
		treemacs-max-git-entries               5000
		treemacs-missing-project-action        'ask
		treemacs-no-png-images                 nil
		treemacs-no-delete-other-windows       t
		treemacs-project-follow-cleanup        nil
		treemacs-persist-file                  (vwe@lib--path-cache
												"treemacs/treemacs-persisst" t)
		treemacs-position                      'left
		treemacs-recenter-distance             0.1
		treemacs-recenter-after-file-follow    t
		treemacs-recenter-after-tag-follow     nil
		treemacs-recenter-after-project-jump   'always
		treemacs-recenter-after-project-expand 'on-distance
		treemacs-show-cursor                   nil
		treemacs-show-hidden-files             t
		treemacs-silent-filewatch              nil
		treemacs-silent-refresh                nil
		treemacs-sorting                       'alphabetic-desc
		treemacs-space-between-root-nodes      t
		treemacs-tag-follow-cleanup            t
		treemacs-tag-follow-delay              1.5
		treemacs-width                         25)

  (treemacs-resize-icons 45)
  (treemacs-tag-follow-mode t)
  (treemacs-git-mode t)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (pcase (cons (not (null (executable-find "git")))
			   (not (null treemacs-python-executable)))
	(`(t . t) (treemacs-git-mode 'deferred))
	(`(t . _) (treemacs-git-mode 'simple))))

(use-package command-log-mode)

(use-package browse-kill-ring
  :diminish
  (browse-kill-ring-mode . nil)
  :bind
  (:map browse-kill-ring-mode-map
		("m" . browse-kill-ring-insert-move-and-quit))
  :init
  (setq browse-kill-ring-highlight-current-entry t
		browse-kill-ring-highlight-inserted-item 'pulse))

(use-package undo-tree
  :diminish
  (undo-tree-mode . nil)
  :hook
  (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
		undo-tree-visualizer-diff t
		undo-tree-enable-undo-in-region nil
		undo-tree-auto-save-history nil
		undo-tree-history-directory-alist `(("." . ,(vwe@lib--path-cache
													 "undotree/hist"))))
  :config
  (dolist (dir undo-tree-history-directory-alist)
	(push (expand-file-name (cdr dir)) recentf-exclude)))

(use-package sudo-edit)

(use-package term
  :ensure nil
  :bind
  (:map term-mode-map
		("C-s" . swiper))
  :config
  (use-package multi-term
	:init
	(setq term-bind-key-alist
		  '(("C-c C-c" . term-interrupt-subjob)
			("C-c C-e" . term-send-esc)
			("M-p" . previous-line)
			("M-n" . next-line)
			("C-s" . isearch-forward)
			("C-r" . isearch-backward)
			("C-m" . term-send-return)
			("C-y" . term-paste)
			("M-f" . term-send-forward-word)
			("M-b" . term-send-backward-word)
			("M-o" . term-send-backspace)
			("C-p" . term-send-up)
			("C-n" . term-send-down)
			("M-r" . term-send-reverse-search-history)
			("M-d" . term-send-delete-word)
			("M-," . term-send-raw)
			("M-." . comint-dynamic-complete)
			("C-s" . swiper)))))

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil
		exec-path-from-shell-variables '("PATH")
		exec-path-from-shell-arguments nil))

(use-package tramp
  :ensure nil
  :init
  (setq tramp-auto-save-directory (vwe@lib--path-cache "tramp")
		tramp-persistency-file-name (vwe@lib--path-cache "tramp/tramp" t)
		tramp-backup-directory-alist (vwe@lib--path-cache "tramp")))

(use-package bookmark
  :ensure nil
  :init
  (setq bookmark-annotation-name vwe@custom--user-name
		bookmark-save-flag 1
		bookmark-default-file (vwe@lib--path-cache "bookmark/bookmarks" t))
  :config
  (defadvice bookmark-jump (after bookmark-jump activate)
    (let ((latest (bookmark-get-bookmark bookmark)))
      (setq bookmark-alist (delq latest bookmark-alist))
      (add-to-list 'bookmark-alist latest))))

(use-package dired
  :ensure nil
  :init
  (setq dired-recursive-deletes 'always
		dired-recursive-copies 'always
		dired-dwin-target 1)
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  ;; 丰富dired颜色
  (use-package diredfl
    :init
    (diredfl-global-mode 1))

  ;; 拼音搜索
  (use-package find-by-pinyin-dired)

  (use-package dired-filter
	:hook
	(dired-mode . dired-filter-group-mode)
	:init
	(setq dired-filter-group-saved-groups
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

  ;; 如果目录只有一级或一个文件直接选中
  (use-package dired-collapse
	:hook
	(dired-mode . dired-collapse-mode)))

(use-package ediff
  :ensure nil
  :hook
  (ediff-quit . winner-undo)
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
		ediff-split-window-function 'split-window-horizontally
		ediff-merge-split-window-function 'split-window-horizontally))

(use-package diff-hl
  :diminish
  (diff-hl-mode . nil)
  :hook
  ((prog-mode . global-diff-hl-mode)
   (magit-post-refresh-hook . diff-hl-magit-post-refresh)))

(use-package mum-key
  :load-path
  (lambda ()
	(vwe@lib--path-vwe-site-lisp "mum/mum-key"))
  :hook
  (after-init . mum-key-mode))

(use-package imenu-list)

(provide 'vwe-general)
;;; vwe-general.el ends here
