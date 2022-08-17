;;; vwe-keybinds.el --- Global Key Binding   -*- lexical-binding: t; -*-

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

;;

;;; Code:
;; ***************************************************************************
;; lib
;; ***************************************************************************
(defvar vwe@keybind--default-list
  '(("<f1>"                               vwe-key:global)
	("<f2>"                               vwe-layout-zoom-mode)
	("<f3>"                               split-window-horizontally)
	("<f4>"                               split-window-vertically)
	("<f8>"                               vwe@theme--init)
	("<f11>"                              toggle-frame-maximized)
	("C-y"                                vwe@lib--paste-from-clipboard)
	("M-w"                                vwe@lib--copy-to-clipboard)
	("C-r"                                recentf-open-files)
	("M-r"                                consult-recent-file)
	("C-'"                                imenu-list-smart-toggle)
	;; ("C-s"                                swiper)
	("C-s"                                consult-line)
	("M-s"                                consult-locate)
	("C-x C-b"                            ibuffer)
	("C-x C-f"                            find-file)
	("C-;"                                iedit-mode)
	("C-M-;"                              iedit-rectangle-mode)
	("C-="                                er/expand-region)
	;; ("M-x"                                counsel-M-x)
	("M-/"                                company-complete)
	("C-M-/"                              company-yasnippet)
	;; ("M-y"                                undo-tree-redo)
	("M-y"                                consult-yank-pop)
	("M-@"                                vwe@lib--minibuffer-switch)
	("M-C-k"                              vwe@lib--buffer-kill-current)
	("M-C-y"                              vwe@lib--window-kill-current)
	("M-C-o"                              vwe@lib--window-maximize)
	("M-C-r"                              browse-kill-ring)
	("M-C-z"                              eval-defun)
	("M-\""                               vwe-move--switch-buffer)
	("C-c C-f"                            format-all-buffer)
	("C-|"                                wve-edit-bound--temp-show)
	("C-M-|"                              vwe-edit-bound--remove-overlays)
	("C-M-="                              cua-rectangle-mark-mode)
	("M-'"                                sp-up-sexp)
	("M-("                                sp-down-sexp)
	("M-="                                sp-backward-unwrap-sexp)
	("M-l"                                vwe-edit-toggle-case--lower-case)
	("M-u"                                vwe-edit-toggle-case--upper-case)
	("M-c"                                vwe-edit-toggle-case--capitalize)
	("M-h"                                vwe-edit-toggle-case--hyphen)
	("M-C-,"                              eacl-complete-multiline)
	("M-C-?"                              youdao-dictionary-search-at-point+)
	("M-?"                                embark-act)
	("C-h b"                              embark-bindings)
	("S-s"                                toggle-frame-maximized)
	("M-+"                                hs-show-block)
	("M--"                                hs-hide-block)
	("M-&"                                (lambda () (interactive) (if (bound-and-true-p hs-minor-mode) (hs-toggle-hiding) (hs-minor-mode t) (hs-toggle-hiding))))
	("C-1"                                vwe-move--windmove-up)
	("C-2"                                vwe-move--windmove-down)
	("C-3"                                vwe-move--windmove-left)
	("C-4"                                vwe-move--windmove-right)
	("C-7"                                vwe-move--swap-up-buffer)
	("C-8"                                vwe-move--swap-down-buffer)
	("C-9"                                vwe-move--swap-left-buffer)
	("C-0"                                vwe-move--swap-right-buffer)
	("M-j"                                vwe-key:global)
	("M-RET"                              vwe-key:global)

	([remap comment-dwim]                 comment-dwim-2)
	([remap move-beginning-of-line]       mwim-beginning-of-code-or-line)
	([remap move-end-of-line]             mwim-end-of-code-or-line)
	([remap goto-line]                    vwe-move-goto-line--goto))
  "Default keybind list.")

(defun vwe@keybind--init ()
  "Key bind init."
  (interactive)
  (vwe@lib--keymap-global-set vwe@keybind--default-list))

(with-eval-after-load 'vwe-key
  (vwe-key-define common
				  (("common" :face (:background "DarkOrange" :foreground "white" :weight bold))
				   (("f" find-file "find file")
					("o" read-only-mode "read only")
					("s" save-buffer "save buffer")
					("w" write-file "save as")
					("k" vwe@lib--buffer-kill-current "kill buffer")
					("n" (lambda () (interactive) (vwe@lib--buffer-kill-other (buffer-list) "^*")) "kill ohter")
					("d" delete-window "del window")
					("x" save-buffers-kill-terminal "kill emacs")
					("," toggle-frame-maximized "maximized")
					("." toggle-frame-fullscreen "fullscreen")
					("l" toggle-truncate-lines "truncate liens")
					("M" menu-bar-mode "menu bar mode")
					("T" tool-bar-mode "tool bar mode")
					("S" scroll-bar-mode "scroll bar mode")
					("b" tmm-menubar "menubar")
					("F" helpful-function "help func")
					("V" helpful-variable "help var")
					("K" helpful-key "help key")
					("m" (lambda () (interactive) (set-mark (point))) "mark")
					("u" vwe-package--update-packages "update pkgs")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold)))))

  (vwe-key-define global
				  (("global" :face (:background "DarkOrange" :foreground "white" :weight bold))
				   (("B" ibuffer "ibuffer")
					("@" vwe@lib--minibuffer-switch "switch minibuffer")
					("i" recentf-open-files "recent files")
					("D" dired "dired")
					("C-d" treemacs "treemacs")
					;; ("s" swiper-thing-at-point "thing at point")
					("R" vwe@lib--replace "replace")
					("r" vwe-search--rg "search")
					("x" vwe@lib--frame-reset "reset frame")
					("T" vwe@theme--toggle "toggle theme")
					("f" format-all-buffer "format code")
					("p" vwe@prog--switch-mode "switch mode")
					("n" switch-to-next-buffer "next buffer ")
					("N" display-line-numbers-mode "line number")
					("c" vwe-edit-toggle-case--select-convert "covert string")
					("m" vwe-key:move "move" :face (:foreground "yellow" :underline t :weight bold))
					("E" vwe-key:edit "edit" :face (:foreground "yellow" :underline t :weight bold))
					("H" vwe-headerline-mode "header line")
					("M" vwe-modeline-mode "modeline")
					("Y" vwe-tray-mode "tray")
					("V" vwe-editor-mode "editor")
					("v" vwe-terminal "term")
					("I" vwe-edit-bound-mode "edit bound")
					("L" vwe@lsp--run "lsp")
					("t" vwe-key:tags "tags" :face (:foreground "yellow" :underline t :weight bold))
					("G" vwe-key:magit "magit" :face (:foreground "yellow" :underline t :weight bold))
					("l" vwe-key:layout "layout" :face (:foreground "yellow" :underline t :weight bold))
					("d" vwe-key:diff "diff" :face (:foreground "yellow" :underline t :weight bold))
					("o" vwe-key:org-extend "org extend" :face (:foreground "yellow" :underline t :weight bold))
					("C" vwe-key:check "check" :face (:foreground "yellow" :underline t :weight bold))
					("P" vwe-key:project "project" :face (:foreground "yellow" :underline t :weight bold))
					("u" vwe-key:undo "undo" :face (:foreground "yellow" :underline t :weight bold))
					("X" vwe-key:proxy "proxy" :face (:foreground "yellow" :underline t :weight bold))
					("s" vwe-key:consult "consult" :face (:foreground "yellow" :underline t :weight bold))
					("g" vwe-key:gud "gud" :face (:foreground "yellow" :underline t :weight bold))
					("e" vwe-edit-region--mark-edit "edit region")
					("C-x" vwe-editor-toggle-submode "editor toggle")
					("F" follow-mode "follow mode")
					("U" esup "esup")
					("O" command-log-mode "command log")
					("z" customize-group "customize group")
					("=" vwe-layout--zoom-type-toggle "toggle zoom")
					("S" sudo-edit "sudo")
					("b" revert-buffer-with-coding-system "revert coding system")
					("h" vwe@lib--eol-hidden-dos "hidden dos eol")
					("^" vwe@lib--eol-remove-dos "remove dos eol")
					("!" winum-mode "win number")
					("(" vwe@base--paren-toggle-style "paren style")
					(")" vwe-paren-mode "show paren color mode")
					("-" vwe@lib--font-reset "font reset")
					("*" calendar "calendar")
					("$" youdao-dictionary-search-at-point+ "translate")
					("?" (lambda () (interactive) (find-file (vwe@lib--path-vwe-lisp "vwe-main.el" t))) "main file")
					("C-f" (lambda () (interactive)
							 (vwe@base--init)
							 (vwe@theme--init)) "reinit" :footer t)
					("RET" vwe-key:common "common" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold)))))

  (vwe-key-define proxy
				  (("proxy" :face (:background "DarkOrange" :foreground "white" :weight bold))
				   (("e" vwe-proxy--enable "proxy enable")
					("E" vwe-proxy--enable-global "proxy global disable")
					("d" vwe-proxy--disable "proxy enable")
					("D" vwe-proxy--disable-global "proxy global disable")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold)))))

  (vwe-key-define layout
				  (("window" :face (:background "DarkOrange" :foreground "white" :weight bold))
				   (("z" vwe-layout-zoom-mode "zoom mode")
					("2" split-window-vertically "split vertically")
					("3" split-window-horizontally "split horizontally")
					("o" switch-to-buffer-other-frame "view buffer other frame")
					("f" view-file-other-frame "view file other frame")
					("P" previous-window-any-frame "prev frame")
					("N" next-window-any-frame "next frame")
					("k" (lambda()(interactive) (delete-frame (selected-frame))) "delete frame")
					("i" image-show-frame "image show frame")
					("e" vwe-layout--window-height-enlarge "win height+" :circle t)
					("s" vwe-layout--window-height-shrink "win height-" :circle t)
					("E" vwe-layout--window-width-enlarge "win width+" :circle t)
					("S" vwe-layout--window-width-shrink "win width-" :circle t)
					("b" balance-windows "win balance")
					("u" vwe-move--windmove-up "win move up" :circle t)
					("d" vwe-move--windmove-down "win move down" :circle t)
					("l" vwe-move--windmove-left "win move left" :circle t)
					("r" vwe-move--windmove-right "win move right" :circle t)
					("U" vwe-move--swap-up-buffer "swap up buffer" :circle t)
					("D" vwe-move--swap-down-buffer "swap down buffer" :circle t)
					("L" vwe-move--swap-left-buffer "swap left buffer" :circle t)
					("R" vwe-move--swap-right-buffer "swap right buffer" :circle t)
					("+" vwe-layout--text-scale-increase "text scale +" :circle t)
					("-" vwe-layout--text-scale-decrease "text scale -" :circle t)
					("=" vwe-layout--text-scale-adjust "text scale adjust" :circle t)
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold)))))

  (vwe-key-define diff
				  (("diff" :face (:background "DarkOrange" :foreground "white" :weight bold))
				   (("d" ediff-files "diff 2 files")
					("u" ediff-backup "diff backup files")
					("b" ediff-buffers "diff 2 buffer")
					("C-d" ediff-files3 "diff 3 files")
					("C-b" ediff-buffers3 "diff 3 buffer")
					("f" ediff-directories "diff 2 dir")
					("C-f" ediff-directories3 "diff 3 dir")
					("v" ediff-directory-revisions "diff dir revisions")
					("V" ediff-merge-directory-revisions "merge dir revisions")
					("M-c" ediff-merge-directory-revisions-with-ancestor "merge dir ancestor revisions")
					("w" ediff-windows-wordwise "diff 2 window by word")
					("l" ediff-windows-linewise "diff 2 window by line")
					("w" ediff-regions-wordwise "diff 2 region by word")
					("r" ediff-regions-linewise "diff 2 region by line")
					("e" ediff-revision "diff vision")
					("m" ediff-merge-files "merge 2 files")
					("M-d" ediff-merge-files-with-ancestor "merge 2 file to other file")
					("B" ediff-merge-buffers "merge 2 buffers")
					("M-b" ediffer-merge-buffers-with-ancestor "merge 2 buffer to other buffer")
					("M-f" ediff-merge-directories "merge 2 dir")
					("M-a" ediff-merge-directories-with-ancestor "merge 2 dir to other dir")
					("C-v" ediff-merge-revisions "merge vision")
					("M-v" ediff-merge-revisions-with-ancestor "merge 2 vision to other vision")
					("h" ediff-documentation "document")
					("s" ediff-show-registry "show registry")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold)))))

  (vwe-key-define check
				  ("check"
				   (("c" flycheck-mode "flycheck mode")
					("v" flycheck-verify-setup "verify checker")
					("l" flycheck-list-errors "error list")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold)))))

  (vwe-key-define project
				  ("project"
				   (("p" vwe-project-mode "project mode")
					("a" vwe-project--add-project "add project")
					("s" vwe-project--switch-project "switch project")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold)))))

  (vwe-key-define magit
				  (("magit" :face (:background "red" :foreground "white" :weight bold))
				   (("s" magit-status "status")
					("g" magit-diff-dwim "magit diff")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  )

  (vwe-key-define dired
				  (("dired" :face (:background "red" :foreground "white" :weight bold))
				   (("p" dired-filter-pop "filter pop")
					("P" dired-filter-pop-all "filter pop all")
					("f" dired-filter-by-file "filter by file")
					("n" dired-filter-by-name "filter by name")
					("m" dired-filter-by-mode "filter by mode")
					("d" dired-filter-by-directory "filter by directory")
					("t" dired-filter-by-dot-files "filter by dot files")
					("e" dired-filter-by-executable "filter by executable")
					("F" dired-filter-group-mode "filter group mode")
					("C" dired-collapse-mode "collapse mode")
					("q" vwe-term--exit "exit terminal")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (dired-mode))

  (vwe-key-define undo
				  (("undo" :face (:background "red" :foreground "white" :weight bold))
				   (("v" undo-tree-visualize "visualize")
					("u" undo-tree-undo "undo" :circle t)
					("r" undo-tree-redo "redo" :circle t)
					("w" undo-tree-save-history "save history")
					("l" undo-tree-load-history "load history")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (dired-mode))

  (vwe-key-define term
				  (("term" :face (:background "red" :foreground "white" :weight bold))
				   (("c" term-char-mode "char mode")
					("l" term-line-mode "line mode")
					("b" vwe-term--show-buffer "switch normal buffer")
					("s" vwe-term--switch-terminal "switch term")
					("r" vwe-term--reset-shell-located "reset located")
					("k" vwe-term--exit "exit terminal")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (term-mode))

  (vwe-key-define tags
				  (("tags" :face (:background "red" :foreground "white" :weight bold))
				   (("c" vwe-tags--tags "create tags")
					("u" vwe-tags--tags-update "update tags")
					("m" vwe-tags--manual-build-tags "manual shell")
					("r" vwe-tags--remove-tags "remove tags")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold)))))

  (vwe-key-define edit
				  (("edit" :face (:background "red" :foreground "white" :weight bold))
				   (("r" cua-rectangle-mark-mode "rectangle mark")
					("e" iedit-mode "iedit mode")
					("E" iedit-rectangle-mode "iedit rectangle mode")
					("x" er/expand-region "expand region")
					("i" imenu-list-smart-toggle "imenu list")
					("c" company-complete "company complete")
					("y" company-yasnippet "company yasnippet")
					("b" wve-edit-bound--temp-show "edit bound")
					("B" vwe-edit-bound--remove-overlays "remove edit bound")
					("a" embark-act "embark act")
					("m" eacl-complete-multiline "eacl complete multiline")
					("s" hs-show-block "show block")
					("h" hs-hide-block "show block")
					("S" hs-show-all "show block")
					("H" hs-hide-all "show block")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold)))))

  (vwe-key-define move
				  (("move" :face (:background "red" :foreground "white" :weight bold))
				   (("[" vwe-move-mark--backward-mark-line "backward line")
					("]" vwe-move-mark--forward-mark-line "forward line")
					("<" vwe-move-mark--backward-mark-word "backward word")
					(">" vwe-move-mark--forward-mark-word "forward word")
					("l" vwe-move-change--goto-last "goto last")
					("c" vwe-move-change--goto-last-cycle "goto last cycle" :circle t)
					("g" vwe-move-goto-line--goto "goto")
					("r" vwe-move-goto-line--recovery "goto line recovery")
					("m" vwe-move-marker-point--marker "point marker")
					("p" vwe-move-marker-point--goto-marker "goto point marker")
					("r" vwe-move-marker-point--clear-marker "clear point marker")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold)))))

  (vwe-key-define consult
				  (("consult" :face (:background "DarkOrange" :foreground "white" :weight bold))
				   (("d" consult-file-externally "file externally")
					("c" consult-complex-command "complex command")
					("u" consult-line-multi "line multi")
					("i" consult-imenu "imenu")
					("p" consult-yank-pop "yank pop")
					("r" consult-yank-from-kill-ring "yank from kill ring")
					("m" consult-minor-mode-menu "minor mode")
					("o" consult-org-heading "org heading")
					("g" consult-ripgrep "rg")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold)))))

  (vwe-key-define treemacs
				  (("treemacs" :face (:background "DarkOrange" :foreground "white" :weight bold))
				   (("d" treemacs-select-directory "select directory")
					("f" treemacs-find-file "find file")
					("t" treemacs-find-tag "find tag")
					("b" treemacs-bookmark "bookmark")
					("R" treemacs-delete-other-windows "delete other windows")
					("d" treemacs-delete-file "delete file")
					("r" treemacs-rename-file "rename file")
					("D" treemacs-create-dir "create directory")
					("y" treemacs-copy-file "copy file")
					("m" treemacs-move-file "move file")
					("p" treemacs-copy-absolute-path-at-point "copy path")
					("C-w" treemacs-create-workspace "create workspace")
					("C-t" treemacs-switch-workspace "switch workspace")
					("C-r" treemacs-rename-workspace "rename workspace")
					("C-d" treemacs-remove-workspace "remove workspace")
					("C-e" treemacs-edit-workspaces "edit workspaces")
					("C-p" treemacs-add-project-to-workspace "add project to workspace")
					("h" treemacs-advanced-helpful-hydra "advanced helpful")
					("H" treemacs-common-helpful-hydra "common helpful")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (treemacs-mode))

  (vwe-key-define emacs-lisp
				  (("emacs lisp" :face (:background "red" :foreground "white" :weight bold))
				   (("b" eval-buffer "eval buffer")
					("d" eval-defun "eval defun")
					("r" eval-region "eval region")
					("e" eval-expression "eval expression")
					("k" describe-key "key")
					("f" describe-function "function")
					("v" describe-variable "variable")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (emacs-lisp-mode))

  (vwe-key-define org-template
				  ("org template"
				   (("a" (lambda () (interactive) (vwe@org--template "<a"))                              "ascii")
					("c" (lambda () (interactive) (vwe@org--template "<c"))                              "center")
					("m" (lambda () (interactive) (vwe@org--template "<C"))                              "comment")
					("e" (lambda () (interactive) (vwe@org--template "<e"))                              "example")
					("E" (lambda () (interactive) (vwe@org--template "<E"))                              "export")
					("h" (lambda () (interactive) (vwe@org--template "<h"))                              "html")
					("x" (lambda () (interactive) (vwe@org--template "<l"))                              "latex")
					("n" (lambda () (interactive) (vwe@org--template "<n"))                              "note")
					("u" (lambda () (interactive) (vwe@org--template "<q"))                              "quote")
					("v" (lambda () (interactive) (vwe@org--template "<v"))                              "verse")
					("I" (lambda () (interactive) (vwe@org--template "<i"))                              "index")
					("A" (lambda () (interactive) (vwe@org--template "<A"))                              "ASCII")
					("i" (lambda () (interactive) (vwe@org--template "<I"))                              "include")
					("H" (lambda () (interactive) (vwe@org--template "<H"))                              "html")
					("L" (lambda () (interactive) (vwe@org--template "<L"))                              "laTeX")
					("s" (lambda () (interactive) (vwe@org--template "<s"))                              "src")
					("l" (lambda () (interactive) (vwe@org--template "<s" "emacs-lisp"))                 "elisp")
					("p" (lambda () (interactive) (vwe@org--template "<s" "python :results output"))     "python")
					("P" (lambda () (interactive) (vwe@org--template "<s" "perl"))                       "perl")
					("r" (lambda () (interactive) (vwe@org--template "<s" "ruby"))                       "ruby")
					("S" (lambda () (interactive) (vwe@org--template "<s" "shell"))                      "shell")
					("g" (lambda () (interactive) (vwe@org--template "<s" "go :imports '\(\"fmt\"\)"))   "golang")
					("t" (lambda () (interactive) (vwe@org--template "<s" "plantuml :file CHANGE.png"))  "plantuml")
					("R" vwe@org--reveal-insert-split                                                    "reveal split")
					("DEL" vwe-key:org "org" :footer t :face (:background "magenta" :foreground "white" :weight bold))
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold)))))

  (vwe-key-define org-extend
				  ("org extend"
				   (("a" org-agenda "agenda")
					("c" org-capture "capture")
					("m" org-roam-insert "roam insert")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold)))))

  (vwe-key-define org
				  ("org"
				   (("l" org-insert-link "+link")
					("L" org-store-link "store link")

					("C-c" org-ctrl-c-ctrl-c "C-c")
					("w" org-open-at-point "open browser")
	  				("a" outline-show-all "show all")
					("c" org-show-subtree "show subtree")
					("t" org-cycle "cycle")

					("s" org-shifttab "shifttab")
	  				("e" org-meta-return  "+meta elem")
					("m" org-meta-return "meta return")
					("i" vwe@org--insert-sub-level-element "+sub elem")
					("d" org-shiftmetaright "element down level")
					("u" org-shiftmetaleft "element up level")
					("h" org-toggle-heading "toggle head")

					("U" (lambda()(interactive)(outline-up-heading 1)) "parent head" :circle t)
					("f" org-forward-heading-same-level "forward head" :circle t)
					("b" org-backward-heading-same-level "backward head" :circle t)
					("n" (lambda()(interactive) (call-interactively 'org-next-visible-heading))  "next heading" :circle t)
					("p" (lambda()(interactive) (call-interactively 'org-previous-visible-heading)) "prev heading" :circle t)

					("<" org-shiftmetaup "move up" :circle t)
					(">" org-shiftmetadown "move down" :circle t)
					("," org-up-element "goto parent elem" :circle t)
					("." org-down-element "goto sub elem" :circle t)

					("T" org-table-toggle-coordinate-overlays "show table point")
					("M" org-edit-special "edit special")

					("P" org-latex-preview "latex preview")
					("X" vwe@org--reset-latex-fonts-size "reset latex font size")

					("g" org-set-tags-command "set tags")
					("r" org-tags-sparse-tree "tags tree")
					("x" org-export-dispatch "export")
					("t" vwe@org--todo-current-line "todo")
					("o" org-insert-todo-heading "insert todo")
					("y" org-rich-yank "rich yank")
					("D" org-tree-slide-mode "tree slide")
					("r" vwe@org--reveal-load "reveal load")
					("v" org-preview-html/preview "preview html")
					("DEL" vwe-key:org-template "org template" :footer t :face (:background "magenta" :foreground "white" :weight bold))
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (org-mode)
				  (if vwe@lib--sys-win-p "C-<return>" "M-<return>"))

  (vwe-key-define markdown
				  ("markdown"
				   (("l" markdown-live-preview-mode "live preview mode")
					("b" markdown-insert-gfm-code-block "code block")
					("c" markdown-insert-code "code")
					("t" markdown-insert-table "table")
					("l" markdown-insert-link "link")
					("i" markdown-insert-image "insert image")
					("u" markdown-insert-uri "insert uri")
					("k" markdown-insert-kbd "kbd")
					("g" markdown-toc-generate-toc "generate")
					("o" markdown-toc-generate-or-refresh-toc "generate/refresh")
					("p" markdown-toc-follow-link-at-point "follow link")
					("1" markdown-insert-header-atx-1 "atx 1")
					("2" markdown-insert-header-atx-2 "atx 2")
					("3" markdown-insert-header-atx-3 "atx 3")
					("4" markdown-insert-header-atx-4 "atx 4")
					("5" markdown-insert-header-atx-5 "atx 5")
					("6" markdown-insert-header-atx-6 "atx 6")
					("7" markdown-insert-header-setext-1 "setext 1")
					("8" markdown-insert-header-setext-2 "setext 2")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (markdown-mode)
				  "C-M-<return>")

  (vwe-key-define gdb-breakpoint
				  ("gdb-breakpoint"
				   (("t" gdb-toggle-breakpoint "toggle breakpoint")
					("d" gdb-delete-breakpoint "delete breakpoint")
					("g" gdb-goto-breakpoint "goto breakpoint")
					("C-u" vwe-key:gud "gud" :face (:foreground "yellow" :underline t :weight bold))
					("C-d" vwe-key:gdb "gdb" :face (:foreground "yellow" :underline t :weight bold))
					("C-b" vwe-key:gdb-buffers "gdb buffers" :face (:foreground "yellow" :underline t :weight bold))
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (gdb-breakpoints-mode))

  (vwe-key-define gdb-thread
				  ("gdb-thread"
				   (("d" gdb-display-disassembly-for-thread "display disassembly")
					("s" gdb-display-stack-for-thread "display stack")
					("l" gdb-display-locals-for-thread "display locals")
					("r" gdb-display-registers-for-thread "display registers")
					("C-u" vwe-key:gud "gud" :face (:foreground "yellow" :underline t :weight bold))
					("C-d" vwe-key:gdb "gdb" :face (:foreground "yellow" :underline t :weight bold))
					("C-b" vwe-key:gdb-buffers "gdb buffers" :face (:foreground "yellow" :underline t :weight bold))
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (gdb-threads-mode))

  (vwe-key-define gdb-memory
				  ("gdb-memory"
				   (("C-u" vwe-key:gud "gud" :face (:foreground "yellow" :underline t :weight bold))
					("C-d" vwe-key:gdb "gdb" :face (:foreground "yellow" :underline t :weight bold))
					("C-b" vwe-key:gdb-buffers "gdb buffers" :face (:foreground "yellow" :underline t :weight bold))
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (gdb-memory-mode))

  (vwe-key-define gdb-disassembly
				  ("gdb-disassembly"
				   (("C-u" vwe-key:gud "gud" :face (:foreground "yellow" :underline t :weight bold))
					("C-d" vwe-key:gdb "gdb" :face (:foreground "yellow" :underline t :weight bold))
					("C-b" vwe-key:gdb-buffers "gdb buffers" :face (:foreground "yellow" :underline t :weight bold))
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (gdb-disassembly-mode))

  (vwe-key-define gdb-locals
				  ("gdb-locals"
				   (("C-u" vwe-key:gud "gud" :face (:foreground "yellow" :underline t :weight bold))
					("C-d" vwe-key:gdb "gdb" :face (:foreground "yellow" :underline t :weight bold))
					("C-b" vwe-key:gdb-buffers "gdb buffers" :face (:foreground "yellow" :underline t :weight bold))
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (gdb-locals-mode))

  (vwe-key-define gdb-registers
				  ("gdb-registers"
				   (("C-u" vwe-key:gud "gud" :face (:foreground "yellow" :underline t :weight bold))
					("C-d" vwe-key:gdb "gdb" :face (:foreground "yellow" :underline t :weight bold))
					("C-b" vwe-key:gdb-buffers "gdb buffers" :face (:foreground "yellow" :underline t :weight bold))
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (gdb-registers-mode))

  (vwe-key-define gdb-inferior-io
				  ("gdb-inferior-io"
				   (("C-u" vwe-key:gud "gud" :face (:foreground "yellow" :underline t :weight bold))
					("C-d" vwe-key:gdb "gdb" :face (:foreground "yellow" :underline t :weight bold))
					("C-b" vwe-key:gdb-buffers "gdb buffers" :face (:foreground "yellow" :underline t :weight bold))
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (gdb-inferior-io-mode))

  (vwe-key-define gdb-frames
				  ("gdb-frames"
				   (("C-u" vwe-key:gud "gud" :face (:foreground "yellow" :underline t :weight bold))
					("C-d" vwe-key:gdb "gdb" :face (:foreground "yellow" :underline t :weight bold))
					("C-b" vwe-key:gdb-buffers "gdb buffers" :face (:foreground "yellow" :underline t :weight bold))
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (gdb-frames-mode))

  (vwe-key-define gdb
				  ("GDB"
				   (("g" gdb "gdb")
					("m" gdb-many-windows "many windos")
					("r" gdb-restore-windows "restore windos")
					("d" gdb-display-buffertype-buffer "display buffer")
					("f" gdb-frame-buffertype-buffer "frame buffer")
					("C-u" vwe-key:gud "gud" :face (:foreground "yellow" :underline t :weight bold))
					("C-b" vwe-key:gdb-buffers "gdb buffers" :face (:foreground "yellow" :underline t :weight bold))
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold)))))

  (vwe-key-define gdb-buffers
				  ("gdb-buffers"
				   (("m" gdb-display-memory-buffer "display memory buffer")
					("a" gdb-display-disassembly-buffer "display disassembly buffer")
					("s" gdb-display-stack-buffer "display stack buffer")
					("i" gdb-display-io-buffer "display io buffer")
					("g" gdb-display-gdb-buffer "display gdb buffer")
					("l" gdb-display-locals-buffer "display localsbuffer")
					("t" gdb-display-threads-buffer "display threads buffer")
					("r" gdb-display-registers-buffer "display registers buffer")
					("b" gdb-display-breakpoints-buffer "display breakpoints buffer")
					("M" gdb-frame-memory-buffer "frame memory buffer")
					("A" gdb-frame-disassembly-buffer "frame disassembly buffer")
					("S" gdb-frame-stack-buffer "frame stack buffer")
					("I" gdb-frame-io-buffer "frame io buffer")
					("G" gdb-frame-gdb-buffer "frame gdb buffer")
					("L" gdb-frame-locals-buffer "frame localsbuffer")
					("T" gdb-frame-threads-buffer "frame threads buffer")
					("R" gdb-frame-registers-buffer "frame registers buffer")
					("B" gdb-frame-breakpoints-buffer "frame breakpoints buffer")
					("C-d" vwe-key:gdb "gdb" :face (:foreground "yellow" :underline t :weight bold))
					("C-u" vwe-key:gud "gud" :face (:foreground "yellow" :underline t :weight bold))
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold)))))

  (vwe-key-define gud
				  ("GUD"
				   (("G" gud-gdb "gud")
					("g" vwe@prog--gud-gdb-exec "gud/gdb exec")
					("d" vwe@prog--gdb-disable "disable gdb")
					("k" vwe@prog--gud-proc-kill "kill gud")
					("b" gud-break "breakpoint")
					("B" gud-remove "remove breakpoint")
					("C-t" gud-tbreak "temp breakpoint")
					("R" gud-refresh "refresh")
					("p" gud-print "print")
					("c" gud-cont "continue")
					("n" gud-next "next")
					("s" gud-step "step")
					("S" gud-stepi "stepi")
					("u" gud-until "until")
					("f" gud-finish "finish")
					("j" gud-jump "jump")
					("r" gud-run "run")
					("U" gud-up "up")
					("D" gud-down "down")
					("T" gud-stop-subjob "stop subjob")
					("N" gud-nexti "nexti")
					("F" gud-jdb-find-source "find source")
					("w" gud-watch "watch")
					("C" gud-gdb-complete-command "complete command")
					("T" gud-tooltip-mode "tooltip mode")
					("C-d" vwe-key:gdb "gdb" :face (:foreground "yellow" :underline t :weight bold))
					("C-b" vwe-key:gdb-buffers "gdb buffers" :face (:foreground "yellow" :underline t :weight bold))
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (gud-mode gdb-mode sdb-mode dbx-mode perldb-mode xdb-mode jdb-mode))

  (vwe-key-define clang
				  ("C/C++"
				   (("b" rmsbolt-mode "rmsbolt")
					("t" rmsbolt-starter "create run file")
					("c" rmsbolt-compile "compile")
					("x" smart-compile "compile")
					("e" compile-goto-error "goto error")
					("o" quickrun-compile-only "compile only")
					("r" quickrun "quickrun")
					("s" quickrun-shell "run shell")
					("g" gdb "gdb")
					("u" vwe-key:gud "gud" :face (:foreground "yellow" :underline t :weight bold))
					("p" gdb-restore-windows "restore windows")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (c-mode c++-mode))

  (vwe-key-define asm
				  ("assembly"
				   (("c" smart-compile "compile")
					("e" compile-goto-error "goto error")
					("g" gdb "gdb")
					("u" vwe-key:gud "gud" :face (:foreground "yellow" :underline t :weight bold))
					("p" gdb-restore-windows "restore windows")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (asm-mode nasm-mode))

  (vwe-key-define python
				  ("python"
				   (("x" run-python "run python")
					("c" conda-env-activate "conda activate")
					("d" conda-env-deactivate "conda deactivate")
					("l" conda-env-list "conda list")
					("e" elpy-enable "elpy enable")
					("s" elpy-shell-switch-to-shell "swithc shell")
					("j" elpy-django-runserver "run django")
					("b" python-shell-send-buffer "send buffer")
					("r" python-shell-send-region "send region")
					("k" vwe@python--kill-python-shell "quit")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (python-mode))

  (vwe-key-define golang
				  ("golang"
				   (("i" go-import-add "import add")
					("m" go-remove-unused-imports "remove imports")
					("t" go-tag-add "add tag")
					("T" go-tag-remove "remove tag")
					("r" go-tag-refresh  "refresh tag")
					("o" quickrun-compile-only "compile only")
					("x" quickrun "quickrun")
					("s" quickrun-shell "run shell")
					("u" vwe-key:gud "gud" :face (:foreground "yellow" :underline t :weight bold))
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (go-mode))

  (vwe-key-define ruby
				  ("ruby"
				   (("a" rvm-activate-corresponding-ruby "rvm activate")
					("u" rvm-use "rvm use")
					("i" inf-ruby "irb")
					("s" (lambda ()
						   (interactive)
						   (funcall
							(vwe-key-define ruby-send
											("ruby send func"
											 (("b" ruby-send-buffer "send buffer")
											  ("j" ruby-send-region-and-go "region Go")
											  ("x" ruby-send-definition "send definition")
											  ("X" ruby-send-definition-and-go "definition go")
											  ("k" ruby-send-block "send block")
											  ("b" ruby-send-buffer "send buffer")))))) "send ..." :face (:foreground "yellow" :underline t :weight bold))
					("c" inf-ruby-console-auto "lanuch repl")
					("r" robe-start "start rebo")
					("t" ruby-switch-to-inf "switch to ibr")
					("n" inf-ruby-switch-setup "switch setup")
					("p" ruby-switch-to-last-ruby-buffer "back last buffer")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (ruby-mode inf-ruby-mode))

  (vwe-key-define rust
				  ("rust"
				   (("n" rustic-cargo-new "cargo new")
					("c" rustic-compile "compile")
					("C" rustic-recompile "recompile")
					("f" rustic-format-buffer "format buffer")
					("F" rustic-format-file "format File")
					("r" rustic-cargo-run "cargo run")
					("R" rust-run "run")
					("W" rustic-cargo-fmt "cargo fmt workspace")
					("a" rustic-cargo-add "cargo add")
					("d" rustic-cargo-rm "cargo rm")
					("u" rustic-cargo-upgrade "cargo upgrade")
					("t" rustic-cargo-test "cargo test")
					("T" rustic-cargo-current-test "gargo test current")
					("o" rustic-cargo-outdated "cargo outdated")
					("p" rustic-cargo-clippy "cargo clippy")
					("l" rustic-cargo-clean "cargo clean")
					("h" rustic-doc-mode "doc mode")
					("x" quickrun "quickrun")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (rust-mode rustic-mode))

  (vwe-key-define java
				  ("java"
				   (("i" vwe@java--init "java lsp init")
					("o" quickrun-compile-only "compile only")
					("q" quickrun "quickrun")
					("s" quickrun-shell "run shell")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (java-mode))

  (vwe-key-define lua
				  ("lua"
				   (("s" lua-start-process "new repl")
					("k" lua-kill-process "kill repl")
					("t" lua-send-buffer "send buffer")
					("d" lua-send-defun "send defun")
					("l" lua-send-current-line "send current line")
					("r" lua-restart-with-whole-file "send buffer restart repl")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (lua-mode))

  (vwe-key-define web
				  ("web"
				   (("r" (lambda () (interactive) (vwe@web--open-to-brower nil)) "open to brower")
					("w" vwe-key:eww "eww" :face (:foreground "yellow" :underline t :weight bold))
					("s" run-skewer "run skewer")
					("e" skewer-repl "repl")
					("e" skewer-eval-last-expression "eval expression")
					("f" skewer-eval-defun "eval defun")
					("b" skewer-load-buffer "load buffer")
	   				("t" skewer-html-eval-tag "html eval tag")
					("c" skewer-css-eval-current-declaration "css eval declaration")
					("u" skewer-css-eval-current-rule "css eval rule")
					("v" skewer-css-eval-buffer "css eval buffer")
					("l" skewer-css-clear-all "css clear all")
					("e" js2-mode-toggle-element "show/hide element")
					("h" js2-mode-toggle-hide-functions "show/hide func")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (web-mode js2-mode js-mode css-mode))

  (vwe-key-define w3m
				  ("w3m"
				   (("w" w3m "w3m")
					("h" w3m-gohome "go home")
					("f" w3m-view-url-with-browse-url "to browse")
					("u" w3m-browse-url "browse url")
					("." w3m-next-buffer "next buffer")
					("," w3m-previous-buffer "prev buffer")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (w3m-mode))

  (vwe-key-define eww
				  ("eww"
				   (("f" eww-browse-with-external-browser "external browser")
					("s" eww-view-source "view source")
					("r" eww-reload "reload")
					("o" eww-open-file "open file")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (eww-mode))
  (vwe-key-define epub
				  ("epub"
				   (("t" nov-goto-toc "table of contents")
					("r" nov-render-document "render document")
					("b" nov-history-back "back history")
					("f" nov-history-forward "forward history")
					("s" nov-view-source "view source")
					("n" nov-next-document "next document")
					("p" nov-previous-document "previous document")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (nov-mode)))

(defun vwe@keybind--qwerty-layout ()
  "Show Qwerty layout."
  (interactive)
  (message "\n%s"
		   "/* Qwerty layer
 * ,-----------------------------------------------------------------------------------------.
 * | Esc |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |  0  |  -  |  =  |  ~  |  |  |
 * |-----------------------------------------------------------------------------------------+
 * | Tab    |  Q  |  W  |  E  |  R  |  T  |  Y  |  U  |  I  |  O  |  P  |  [  |  ]  |  BSPC  |
 * |-----------------------------------------------------------------------------------------+
 * | RCtrl   |  A  |  S  |  D  |  F  |  G  |  H  |  J  |  K  |  L  |  ;  |  '  |    Enter    |
 * |-----------------------------------------------------------------------------------------+
 * | LShift    |  Z  |  X  |  C  |  V  |  B  |  N  |  M  |  ,  |  .  |  /  |   RShift        |
 * |-----------------------------------------------------------------------------------------+
 * | CapsLock | Fn | LAlt  |              Space               |   RAlt | Menu |  Fn  | RCtrl |
 * `-----------------------------------------------------------------------------------------'
 */"))

;; ***************************************************************************
;; config
;; ***************************************************************************
(vwe@keybind--init)

(vwe@lib--log "Initialization of Keybinds configuration is complete.")

(provide 'vwe-keybinds)
;;; vwe-keybinds.el ends here
