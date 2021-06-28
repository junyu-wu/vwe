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

;; ***************************************************************************
;; config
;; ***************************************************************************

;;
;; `vwe-modeline'
;;
(vwe@lib--package 'vwe-modeline
				  (autoload 'vwe-modeline-mode (vwe@lib--path-vwe-site-lisp "vwe/vwe-modeline/vwe-modeline.el" t) "Vwe modeline mode" t t)
				  nil
				  (progn
					(setq-default mode-line-format nil)
					(add-hook 'after-init-hook (lambda () (when vwe@custom--modeline-show? (vwe-modeline-mode))))
					(setq vwe-modeline--buffer-filter-list vwe@custom--modeline--hide-list
						  vwe-modeline--default-format mode-line-format))
				  nil
				  (vwe@lib--path-vwe-site-lisp "vwe/vwe-modeline"))

;;
;; `vwe-tray'
;;
(vwe@lib--package 'vwe-tray-mode
				  (autoload 'vwe-tray-mode (vwe@lib--path-vwe-site-lisp "vwe/vwe-tray/vwe-tray.el" t) "Vwe tray mode" t t)
				  nil
				  (add-hook 'after-init-hook (lambda () (when vwe@custom--tray-show? (vwe-tray-mode))))
				  nil
				  (vwe@lib--path-vwe-site-lisp "vwe/vwe-tray"))

;;
;; `vwe-headerline'
;;
(vwe@lib--package 'vwe-headerline
				  (autoload 'vwe-headerline-mode (vwe@lib--path-vwe-site-lisp "vwe/vwe-headerline/vwe-headerline.el" t) "Vwe headerline mode" t t)
				  nil
				  (progn
					(add-hook 'after-init-hook (lambda () (when vwe@custom--headerline-show? (vwe-headerline-mode))))
					(setq vwe-headerline--buffer-filter-list vwe@custom--buffer-filter-list))
				  nil
				  (vwe@lib--path-vwe-site-lisp "vwe/vwe-headerline"))

;;
;; `savehist' 保存buffer历史记录
;;
(vwe@lib--package 'savehist
				  (add-hook 'after-init-hook #'savehist-mode)
				  nil
				  (setq enable-recursive-minibuffers t
						savehist-file (vwe@lib--path-cache "savehist/history" t)
						history-length 1000
						savehist-additional-variables '(mark-ring
														global-mark-ring
														search-ring
														regexp-search-ring
														extended-command-history)
						savehist-autosave-interval 300))

;;
;; `ibuffer'
;;
(vwe@lib--package 'ibuffer
				  (add-hook 'ibuffer-mode-hook
  							(lambda ()
  							  (unless (eq ibuffer-sorting-mode 'filename/process)
  								(ibuffer-do-sort-by-filename/process))
  							  (ibuffer-switch-to-saved-filter-groups "default")))
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
  								 ("Mesasge" (or (mode . message-mode) (mode . bbdb-mode) (mode . mail-mode) (mode . gnus-group-mode) (mode . gnus-summary-mode) (mode . gnus-article-mode) (name . "^\\.bbdb$") (name . "^\\.newsrc-dribble"))))))))

;;
;; `transient'
;;
(vwe@lib--package 'transient
				  nil nil
				  (setq transient-history-file (vwe@lib--path-cache "transient/history.el" t)
						transient-levels-file (vwe@lib--path-cache "transient/levels.el" t)
						transient-values-file (vwe@lib--path-cache "transient/values.el" t)))

;;
;; `htmlize' buffer文本转换为html
;;
(vwe@lib--package 'htmlize)

;;
;; `which-key'
;;
(vwe@lib--package 'which-key
				  (add-hook 'after-init-hook #'which-key-mode))

;;
;; `ivy'
;;
(vwe@lib--package 'ivy
				  (add-hook 'after-init-hook #'ivy-mode)
				  (progn
					;;
					;; `ivy-rich'
					;;
					(vwe@lib--package 'ivy-rich
									  (add-hook 'ivy-mode-hook (lambda () (ivy-rich-mode 1)))
									  (progn
										(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)))
					;;
					;; `counsel'
					;;
					(vwe@lib--package 'counsel
									  nil
									  (if (executable-find "rg")
										  (setq counsel-grep-base-command
												"rg -i -M 120 --no-heading --line-number --color never %s %s"
												counsel-rg-base-command
												"rg -i -M 120 --no-heading --line-number --color never %s .")))

					;;
					;; `swiper'
					;;
					(vwe@lib--package 'swiper))
				  (setq ivy-use-virtual-buffers t
						ivy-height 10
						ivy-initial-inputs-alist nil
						ivy-count-format "%d/%d"
						ivy-re-builders-alist `((t . ivy--regex-ignore-order))
						enable-recursive-minibuffers t))

;;
;; `yasnippet' include `yasnippet-snippets' `auto-yasnippet'
;;
(vwe@lib--package 'yasnippet
				  (add-hook 'after-init-hook #'yas-global-mode)
				  (progn
					(yas-reload-all)
					;;
					;; `yasnippet-snippets'
					;;
					(vwe@lib--package 'yasnippet-snippets)
					;;
					;; `auto-yasnippet'
					;;
					(vwe@lib--package 'auto-yasnippet))
				  (setq yas-snippet-dirs (list (vwe@lib--path-vwe-etc "snippets"))))

;;
;; `autoinsert'
;;
(vwe@lib--package 'autoinsert
				  (add-hook 'after-init-hook #'auto-insert-mode)
				  (progn
					(define-auto-insert "\\.org$" ["default-reveal-org.org" (lambda () (when (fboundp 'yas-expand-snippet) (yas-expand-snippet (buffer-string) (point-min) (point-max))))])
					(define-auto-insert "\\.txt$" ["default-reveal-org.org" (lambda () (when (fboundp 'yas-expand-snippet) (yas-expand-snippet (buffer-string) (point-min) (point-max))))]))
				  (setq auto-insert t
						auto-insert-query nil
						auto-insert-directory (vwe@lib--path-vwe-etc "templates")))

;;
;; `company'
;;
(vwe@lib--package 'company
				  (add-hook 'prog-mode-hook #'global-company-mode)
				  (progn
					(vwe@lib--keymap-set company-search-map '(("C-n" company-select-next)
															  ("C-p" company-select-previous)))
					(vwe@lib--keymap-set company-active-map '(("C-n" company-select-next)
															  ("C-p" company-select-previous)
															  ("<tab>" company-complete-common-or-cycle)))

					;;
					;; `company-prescient'
					;;
					(vwe@lib--package 'company-prescient (add-hook 'company-mode-hook #'company-prescient-mode))
					;;
					;; `company-quickhelp'
					;;
					(vwe@lib--package 'company-quickhelp (add-hook 'company-mode-hook #'company-quickhelp-mode)
									  nil
									  (setq company-quickhelp-delay 1.5))
					;;
					;; `company-quickhelp-terminal'
					;;
					(vwe@lib--package 'company-quickhelp-terminal (add-hook 'company-mode-hook #'company-quickhelp-terminal-mode))
					;;
					;; `company-box'
					;;
					(vwe@lib--package 'company-box (add-hook 'company-mode-hook #'company-box-mode)
									  nil
									  (setq company-box-enable-icon t
											company-box-backends-colors nil
											company-box-show-single-candidate t
											company-box-max-candidates 50
											company-box-doc-delay 1.5))
					;;
					;; `company-posframe'
					;;
					(vwe@lib--package 'company-posframe (add-hook 'prog-mode-hook #'company-posframe-mode)))
				  (setq company-tooltip-align-annotations t
						company-tooltip-limit 12
						company-idle-delay 0
						company-echo-delay (if (display-graphic-p) nil 0)
						company-minimum-prefix-length 0
						company-require-match nil
						company-dabbrev-ignore-case nil
						company-dabbrev-downcase nil
						company-show-numbers t
						company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
						company-backends '((company-files company-yasnippet company-keywords company-capf)
										   (company-abbrev company-dabbrev))))

;;
;; `mmm-mode'
;;
(vwe@lib--package 'mmm-mode nil
				  (progn
					(mmm-add-mode-ext-class nil "\\.html\\'" 'js2-mode))
				  (setq mmm-global-mode 'maybe))

;;
;; `wgrep'
;;
(vwe@lib--package 'wgrep)

;;
;; `hungry-delete' 删除多余的空格
;;
(vwe@lib--package 'hungry-delete
				  (add-hook 'after-init-hook #'global-hungry-delete-mode)
				  (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;;
;; `expand-region' 快捷选中
;;
(vwe@lib--package 'expand-region)

;;
;; `mwim' 根据代码移动光标
;;
(vwe@lib--package 'mwim)

;;
;; `smartparens'
;;
(vwe@lib--package 'smartparens
				  (add-hook 'prog-mode-hook #'smartparens-mode)
				  (progn
					(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
					(sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)
					(sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
					(sp-local-pair 'lisp-interaction-mode "`" nil :actions nil)
					(sp-local-pair 'web-mode "<" ">")))

;;
;; `iedit' 批量编辑
;;
(vwe@lib--package 'iedit
				  (define-key isearch-mode-map	(kbd "C-;") #'iedit-mode-from-isearch))

;;
;; `comment-dwim-2' 注释
;;
(vwe@lib--package 'comment-dwim-2)

;;
;; `esup' test startup time
;;
(vwe@lib--package 'esup
				  (setq esup-child-max-depth 0))

;;
;; `url-cache'
;;
(vwe@lib--package 'url-cache
				  nil nil
				  (setq url-cache-directory (vwe@lib--path-cache "url/cache"))
				  nil nil t)

;;
;; `format-all'
;;
(vwe@lib--package 'format-all
				  (add-hook 'prog-mode-hook #'format-all-mode))

;;
;; `command-log-mode'
;;
(vwe@lib--package 'command-log-mode)

;;
;; `browse-kill-ring'
;;
(vwe@lib--package 'browse-kill-ring
				  nil
				  (define-key browse-kill-ring-mode-map (kbd "i") #'browse-kill-ring-insert-move-and-quit)
				  (setq browse-kill-ring-highlight-current-entry t
						browse-kill-ring-highlight-inserted-item 'pulse))

;;
;; `undo-tree'
;;
(vwe@lib--package 'undo-tree
				  (add-hook 'after-init-hook #'global-undo-tree-mode)
				  (dolist (dir undo-tree-history-directory-alist)
					(push (expand-file-name (cdr dir)) recentf-exclude))
				  (setq undo-tree-visualizer-timestamps t
						undo-tree-visualizer-diff t
						undo-tree-enable-undo-in-region nil
						undo-tree-auto-save-history nil
						undo-tree-history-directory-alist `(("." . ,(vwe@lib--path-cache "undotree/hist")))))

;;
;; `sudo-edit'
;;
(vwe@lib--package 'sudo-edit)

;;
;; `exec-path-from-shell'
;;
(vwe@lib--package 'exec-path-from-shell
				  nil nil
				  (setq exec-path-from-shell-check-startup-files nil
						exec-path-from-shell-variables '("PATH")
						exec-path-from-shell-arguments nil))

;;
;; `tramp'
;;
(vwe@lib--package 'tramp
				  nil nil
				  (setq tramp-auto-save-directory (vwe@lib--path-cache "tramp")
						tramp-persistency-file-name (vwe@lib--path-cache "tramp/tramp" t)
						tramp-backup-directory-alist (vwe@lib--path-cache "tramp")))

;;
;; `bookmark'
;;
(vwe@lib--package 'bookmark
				  (setq bookmark-annotation-name vwe@custom--user-name
						bookmark-save-flag 1
						bookmark-default-file (vwe@lib--path-cache "bookmark/bookmarks" t))
				  (defadvice bookmark-jump (after bookmark-jump activate)
					(let ((latest (bookmark-get-bookmark bookmark)))
					  (setq bookmark-alist (delq latest bookmark-alist))
					  (add-to-list 'bookmark-alist latest))))

;;
;; `dired'
;;
(vwe@lib--package 'dired
				  (setq dired-recursive-deletes 'always
						dired-recursive-copies 'always
						dired-dwin-target 1)
				  (progn
					(put 'dired-find-alternate-file 'disabled nil)
					;;
					;; `diredfl' 丰富dired颜色
					;;
					(vwe@lib--package 'diredfl
									  (diredfl-global-mode 1))

					;;
					;; `find-by-pinyin-dirred' 拼音搜索
					;;
					(vwe@lib--package 'find-by-pinyin-dired)

					;;
					;; `dired-filter'
					;;
					(vwe@lib--package 'dired-filter
									  (add-hook 'dired-mode-hook #'dired-filter-group-mode)
									  nil
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
											   ("Picture" (extension "jpg" "jepg" "png" "gif"))))))
									  t nil nil)

					;;
					;; `dired-collapse' 如果目录只有一级或一个文件直接选中
					;;
					(vwe@lib--package 'dired-collapse
									  (add-hook 'dired-mode-hook #'dired-collapse-mode)))
				  nil nil nil t)

;;
;; `ediff'
;;
(vwe@lib--package 'ediff
				  (setq ediff-window-setup-function 'ediff-setup-windows-plain
						ediff-split-window-function 'split-window-horizontally
						ediff-merge-split-window-function 'split-window-horizontally))

;;
;; `diff-hl'
;;
(vwe@lib--package 'diff-hl
				  (progn (add-hook 'prog-mode-hook #'global-diff-hl-mode)
						 (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

;;
;; `vwe-key'
;;
(vwe@lib--package 'vwe-key
				  (add-hook 'after-init-hook #'vwe-key-mode)
				  nil nil t
				  (vwe@lib--path-vwe-site-lisp "vwe/vwe-key"))

;;
;; `vwe-editor'
;;
(vwe@lib--package 'vwe-editor
				  (progn
					(autoload 'vwe-editor-mode (vwe@lib--path-vwe-site-lisp "vwe/vwe-editor/vwe-editor.el" t) "Vwe editor mode." t t)
					(add-hook 'after-init-hook  #'vwe-editor-mode))
				  nil nil nil
				  (vwe@lib--path-vwe-site-lisp "vwe/vwe-editor"))

;;
;; `vwe-term'
;;
(vwe@lib--package 'vwe-term
				  (autoload 'vwe-terminal (vwe@lib--path-vwe-site-lisp "vwe/vwe-term/vwe-term.el" t) "Vwe term mode." t t)
				  nil nil nil
				  (vwe@lib--path-vwe-site-lisp "vwe/vwe-term"))

;;
;; `vwe-proxy'
;;
(vwe@lib--package 'vwe-proxy
				  (progn
					(autoload 'vwe-proxy--enable (vwe@lib--path-vwe-site-lisp "vwe/vwe-proxy/vwe-proxy.el" t) "Vwe proxy mode." t t)
					(autoload 'vwe-proxy--enable-global (vwe@lib--path-vwe-site-lisp "vwe/vwe-proxy/vwe-proxy.el" t) "Vwe global proxy mode." t t))
				  nil nil nil
				  (vwe@lib--path-vwe-site-lisp "vwe/vwe-proxy"))

;;
;; `vwe-layout'
;;
(vwe@lib--package 'vwe-layout
				  (progn
					(autoload 'vwe-layout--enable (vwe@lib--path-vwe-site-lisp "vwe/vwe-layout/vwe-layout.el" t) "Vwe global layout mode." t t)
					(autoload 'vwe-layout--text-scale-increase (vwe@lib--path-vwe-site-lisp "vwe/vwe-layout/vwe-layout.el" t) "Vwe global layout mode." t t)
					(autoload 'vwe-layout--text-scale-decrease (vwe@lib--path-vwe-site-lisp "vwe/vwe-layout/vwe-layout.el" t) "Vwe global layout mode." t t)
					(autoload 'vwe-layout--text-scale-adjust (vwe@lib--path-vwe-site-lisp "vwe/vwe-layout/vwe-layout.el" t) "Vwe global layout mode." t t)
					(autoload 'vwe-layout--zoom-type-toggle (vwe@lib--path-vwe-site-lisp "vwe/vwe-layout/vwe-layout.el" t) "Vwe global layout mode." t t))
				  nil nil nil
				  (vwe@lib--path-vwe-site-lisp "vwe/vwe-layout"))

;;
;; `vwe-search'
;;
(vwe@lib--package 'vwe-search
				  (progn
					(autoload 'vwe-search--rg (vwe@lib--path-vwe-site-lisp "vwe/vwe-search/vwe-search.el" t) "Vwe global search mode." t t))
				  nil nil nil
				  (vwe@lib--path-vwe-site-lisp "vwe/vwe-search"))

;;
;; `vwe-move'
;;
(vwe@lib--package 'vwe-move
				  (progn
					(autoload 'vwe-move-mode (vwe@lib--path-vwe-site-lisp "vwe/vwe-move/vwe-move.el" t) "Vwe move mode." t t)
					(add-hook 'after-init-hook #'vwe-move-mode))
				  nil nil nil
				  (vwe@lib--path-vwe-site-lisp "vwe/vwe-move"))

;;
;; `vwe-edit'
;;
(vwe@lib--package 'vwe-edit
				  (progn
					(autoload 'vwe-edit-region--mark-edit (vwe@lib--path-vwe-site-lisp "vwe/vwe-edit/vwe-edit.el" t) "Vwe edit mode." t t)
					(autoload 'vwe-edit-toggle-case--upper-case (vwe@lib--path-vwe-site-lisp "vwe/vwe-edit/vwe-edit.el" t) "Vwe edit mode." t t)
					(autoload 'vwe-edit-toggle-case--lower-case (vwe@lib--path-vwe-site-lisp "vwe/vwe-edit/vwe-edit.el" t) "Vwe edit mode." t t)
					(autoload 'vwe-edit-toggle-case--select-convert (vwe@lib--path-vwe-site-lisp "vwe/vwe-edit/vwe-edit.el" t) "Vwe edit mode." t t))
				  nil nil nil
				  (vwe@lib--path-vwe-site-lisp "vwe/vwe-edit"))

;;
;; `vwe-tags'
;;
(vwe@lib--package 'vwe-tags
				  (progn
					(autoload 'vwe-tags-mode (vwe@lib--path-vwe-site-lisp "vwe/vwe-tags/vwe-tags.el" t) "Vwe tags mode." t t)
					(autoload 'vwe-tags--create-tags (vwe@lib--path-vwe-site-lisp "vwe/vwe-tags/vwe-tags.el" t) "Create tags ." t t)
					(autoload 'vwe-tags--refresh-tags (vwe@lib--path-vwe-site-lisp "vwe/vwe-tags/vwe-tags.el" t) "Refresh tags." t t)
					(add-hook 'prog-mode-hook #'vwe-tags-mode))
				  nil nil nil
				  (vwe@lib--path-vwe-site-lisp "vwe/vwe-tags"))

;;
;; `vwe-paren'
;;
(vwe@lib--package 'vwe-paren
				  (progn
					(autoload 'vwe-paren-mode (vwe@lib--path-vwe-site-lisp "vwe/vwe-paren/vwe-paren.el" t) "Vwe paren mode." t t)
					(add-hook 'prog-mode-hook #'vwe-paren-mode))
				  nil nil nil
				  (vwe@lib--path-vwe-site-lisp "vwe/vwe-paren"))

;;
;; `imenu-list'
;;
(vwe@lib--package 'imenu-list)

;;
;; `text-mode'
;;
(vwe@lib--package 'text-mode
				  (push '("\\.txt\\'" . org-mode) auto-mode-alist)
				  nil nil nil nil t)

;;
;; `diff-hl'
;;
(vwe@lib--package 'diff-hl)

;;
;; `magit'
;;
(vwe@lib--package 'magit nil (progn
							   (add-hook 'after-save-hook #'magit-after-save-refresh-status)
							   (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
							   (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

(provide 'vwe-general)
;;; vwe-general.el ends here
