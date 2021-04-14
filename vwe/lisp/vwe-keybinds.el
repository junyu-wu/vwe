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
  '(("<f1>"                               (lambda () (interactive) (find-file (vwe@lib--path-vwe-lisp "vwe-main.el" t))))
	("<f2>"                               vwe-layout-zoom-mode)
	("<f3>"                               split-window-horizontally)
	("C-<f3>"                             split-window-vertically)
	("<f4>"                               vwe@theme--init)
	("<f12>"                              toggle-frame-fullscreen)
	("C-r"                                recentf-open-files)
	("C-'"                                imenu-list-smart-toggle)
	("C-s"                                swiper)
	("C-x C-b"                            ibuffer)
	("C-x C-f"                            find-file)
	("C-;"                                iedit-mode)
	("C-M-;"                              iedit-rectangle-mode)
	("C-="                                er/expand-region)
	("C-,"                                goto-last-change)
	("M-x"                                counsel-M-x)
	("M-/"                                company-complete)
	("C-M-/"                              company-yasnippet)
	("M-y"                                undo-tree-redo)
	("M-@"                                vwe@lib--minibuffer-switch)
	("M-C-k"                              vwe@lib--buffer-kill-current)
	("M-C-y"                              vwe@lib--window-kill-current)
	("M-C-o"                              vwe@lib--window-maximize)
	("M-C-r"                              browse-kill-ring)
	("M-C-z"                              eval-defun)
	("M-\""                               vwe-move--switch-buffer)
	("C-@"                                vwe@lib--minibuffer-switch)
	("C-c C-f"                            format-all-buffer)
	("C-M-|"                              set-rectangular-region-anchor)
	("C-M-="                              cua-rectangle-mark-mode)
	("M-'"                                sp-up-sexp)
	("M-)"                                sp-up-sexp)
	("M-("                                sp-down-sexp)
	("M-C-'"                              sp-backward-unwrap-sexp)
	("M-l"                                vwe-edit-toggle-case--lower-case)
	("M-u"                                vwe-edit-toggle-case--upper-case)
	("M-c"                                vwe-edit-toggle-case--select-convert)
	("M-C-,"                              eacl-complete-multiline)
	("C-]"                                counsel-etags-find-tag-at-point)
	("M-C-?"                              youdao-dictionary-search-at-point+)
	("s-s"                                toggle-frame-maximized)
	("M-&"                                (lambda () (interactive) (if (bound-and-true-p hs-minor-mode) (hs-toggle-hiding) (hs-minor-mode t) (hs-toggle-hiding))))
	("C-1"                                vwe-move--windmove-up)
	("C-2"                                vwe-move--windmove-down)
	("C-3"                                vwe-move--windmove-left)
	("C-4"                                vwe-move--windmove-right)
	("C-7"                                vwe-move--swap-up-buffer)
	("C-8"                                vwe-move--swap-down-buffer)
	("C-9"                                vwe-move--swap-left-buffer)
	("C-0"                                vwe-move--swap-right-buffer)
	("M-RET"                              vwe-key:global)

	([remap comment-dwim]                 comment-dwim-2)
	([remap move-beginning-of-line]       mwim-beginning-of-code-or-line)
	([remap move-end-of-line]             mwim-end-of-code-or-line)
	([remap goto-line]                    vwe-move-line-preview--dynamic-goto-line))
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
					("p" vwe-proxy--enable "proxy enable")
					("P" vwe-proxy--enable-global "proxy global enable")
					("," toggle-frame-maximized "maximized")
					("." toggle-frame-fullscreen "fullscreen")
					("F" helpful-function "help func")
					("V" helpful-variable "help var")
					("K" helpful-key "help key")
					("m" (lambda () (interactive) (set-mark (point))) "mark")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold)))))

  (vwe-key-define global
				  (("global" :face (:background "DarkOrange" :foreground "white" :weight bold))
				   (("g" counsel-etags-grep "grep")
					("B" ibuffer "ibuffer")
					("@" vwe@lib--minibuffer-switch "switch minibuffer")
					("i" recentf-open-files "recent files")
					("d" dired "dired")
					("s" swiper-thing-at-point "thing at point")
					("R" vwe@lib--replace "replace")
					("r" vwe-search--rg "search")
					("x" vwe@lib--frame-reset "reset frame")
					("t" vwe@theme--toggle "toggle theme")
					("f" format-all-buffer "format code")
					("p" vwe@prog--switch-mode "switch mode")
					("n" switch-to-next-buffer "next buffer ")
					("N" display-line-numbers-mode "line number")
					("c" vwe-edit-toggle-case--select-convert "covert string")
					("H" vwe-headerline-mode "header line")
					("M" vwe-modeline-mode "modeline")
					("T" vwe-tray-mode "tray")
					("E" vwe-editor-mode "editor")
					("v" vwe-terminal "term")
					("l" vwe@lsp--run "lsp")
					("L" vwe-key:layout "layout")
					("o" vwe-key:org-extend "org extend")
					("C" vwe-key:check "check")
					("P" vwe-key:project "project")
					("T" treemacs "treemacs")
					("e" vwe-edit-region--mark-edit "edit region")
					("U" esup "esup")
					("z" customize-group "customize group")
					("=" vwe-layout--zoom-type-toggle "toggle zoom")
					("*" treemacs-select-window "treemacs window")
					("S" sudo-edit "sudo")
					("b" revert-buffer-with-coding-system "revert coding system")
					("!" winum-mode "win number")
					("~" vwe-mark-paren--paren-pair "paren mark")
					("(" vwe@base--paren-toggle-style "paren style")
					("-" vwe@lib--font-reset "font reset")
					("$" youdao-dictionary-search-at-point+ "translate")
					("?" (lambda () (interactive) (find-file (vwe@lib--path-vwe-lisp "vwe-main.el" t))) "main file")
					("C-f" (lambda () (interactive)
							 (vwe@base--init)
							 (vwe@ui--init)
							 (vwe@theme--init)) "reinit" :footer t)
					("RET" vwe-key:common "common" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold)))))

  (vwe-key-define layout
				  (("window" :face (:background "DarkOrange" :foreground "white" :weight bold))
				   (("z" vwe-layout-zoom-mode "zoom mode")
					("2" split-window-vertically "split vertically")
					("3" split-window-horizontally "split horizontally")
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

  (vwe-key-define check
				  ("check"
				   (("c" flycheck-mode "flycheck mode")
					("v" flycheck-verify-setup "verify checker")
					("l" flycheck-list-errors "error list")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold)))))

  (vwe-key-define project
				  ("project"
				   (("p" projectile-mode "projectile mode")
					("g" projectile-grep "grep")
					("a" projectile-add-known-project "add project")
					("o" projectile-switch-open-project "switch open project")
					("s" projectile-switch-project "switch project")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold)))))

  (vwe-key-define treemacs
				  ("treemacs"
				   (("t" treemacs "treemacs mode")
					("b" treemacs-bookmark "bookmark")
					("f" treemacs-find-file "find file")
					("c" treemacs-create-workspace "create workspace")
					("s" treemacs-switch-workspace "switch workspace")
					("r" treemacs-remove-workspace "remove workspace")
					("W" treemacs-rename-workspace "rename workspace")
					("P" treemacs-rename-project "rename project name")
					("e" treemacs-edit-workspaces "edit workspace")
					("a" treemacs-add-project-to-workspace "add project to workspace")
					("d" treemacs-remove-project-from-workspace "remove project form workspace")
					("p" treemacs-copy-path-at-point "copy path at point")
					("i" treemacs-copy-file "copy file")
					("u" treemacs-goto-parent-node "goto parent node")
					("D" treemacs-set-width "set width")
					("R" treemacs-refresh "refresh")
					("k" treemacs-delete "delete")
					("x" treemacs-rename "rename")
					("w" treemacs-create-file "create file")
					("l" treemacs-create-dir "create dir")
					("z" treemacs-copy-file "copy file")
					("m" treemacs-move-file "move file")
					("h" treemacs-collapse-parent-node "collapse parent node")
					("M-RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (treemacs-mode)
				  "C-M-<return>")

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
					("q" vwe-term--exit "exit terminal")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (dired-mode))

  (vwe-key-define term
				  (("term" :face (:background "red" :foreground "white" :weight bold))
				   (("c" term-char-mode "char mode")
					("l" term-line-mode "line mode")
					("q" vwe-term--exit "exit terminal")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (term-mode))

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
					("o" org-open-at-point "open browser")
	  				("a" outline-show-all "show all")
					("f" org-shifttab "shift tab")
					("s" org-show-subtree "show subtree")
					("T" org-table-toggle-coordinate-overlays "show table point")
					("C" org-ctrl-c-ctrl-c "C-c")
					("m" org-edit-special "edit special")
	  				("e" org-meta-return  "+meta elem")
					("L" org-store-link "store link")
					("i" vwe@org--insert-sub-level-element "+sub elem")
					("d" org-shiftmetaright "element down level")
					("u" org-shiftmetaleft "element up level")
					("<" org-shiftmetaup "move up")
					(">" org-shiftmetadown "move down")
					("," org-up-element "up elem")
					("." org-down-element "down elem")
					("F" org-forward-heading-same-level "forward head")
					("B" org-backward-heading-same-level "backward head")
					("P" org-export-dispatch "export")
					("h" org-toggle-heading "toggle head")
					("t" vwe@org--todo-current-line "todo")
					("I" org-insert-todo-heading "insert todo")
					("y" org-rich-yank "rich yank")
					("S" org-tree-slide-mode "tree slide")
					("r" vwe@org--reveal-load "reveal load")
					("v" org-preview-html/preview "preview html")
					("DEL" vwe-key:org-template "org template" :footer t :face (:background "magenta" :foreground "white" :weight bold))
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (org-mode)
				  (if vwe@lib--sys-win-p "C-<return>" "C-M-<return>"))

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

  (vwe-key-define gud
				  ("GUD"
				   (("g" gdb "gbd")
					("x" vwe@prog--gud-or-gud-go "gud go")
					("d" vwe@prog--gdb-disable "disable gdb")
					("b" vwe@prog--gud-breakpoint-set-or-remove "breakpoint")
					("k" vwe@prog--gud-proc-kill "kill gud")
					("c" gud-cont "continue")
					("n" gud-next "next")
					("s" gud-step "step")
					("u" gud-until "until")
					("r" gud-run "run")
					("i" gud-nexti "nexti")
					("f" gud-finish "finish")
					("j" gud-jdb-find-source "find source")
					("t" gud-stepi "stepi")
					("p" gud-print "print")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold)))))

  (vwe-key-define clang
				  ("C/C++"
				   (("r" rmsbolt-mode "rmsbolt")
					("t" rmsbolt-starter "create run file")
					("c" rmsbolt-compile "compile")
					("x" smart-compile "compile")
					("e" compile-goto-error "goto error")
					("o" quickrun-compile-only "compile only")
					("q" quickrun "quickrun")
					("s" quickrun-shell "run shell")
					("g" gdb "gdb")
					("u" vwe-key:gud "gud")
					("p" gdb-restore-windows "restore windows")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (c-mode c++-mode))

  (vwe-key-define asm
				  ("assembly"
				   (("c" smart-compile "compile")
					("e" compile-goto-error "goto error")
					("g" gdb "gdb")
					("u" vwe-key:gud "gud")
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
					("q" vwe@python--kill-python-shell "quit")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (python-mode))

  (vwe-key-define golang
				  ("golang"
				   (("r" go-run "go run")
					("i" go-impl "impl")
					("a" go-import-add "import add")
					("m" go-remove-unused-imports "remove imports")
					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (go-mode))

  (vwe-key-define ruby
				  ("ruby"
				   (("a" rvm-activate-corresponding-ruby "rvm activate")
					("u" rvm-use "rvm use")
					("i" inf-ruby "irb")
					("c" inf-ruby-console-auto "lanuch repl")
					("r" robe-start "start rebo")
					("s" ruby-switch-to-inf "switch to ibr")
					("j" ruby-send-region-and-go "region Go")
					("x" ruby-send-definition "send definition")
					("X" ruby-send-definition-and-go "definition go")
					("k" ruby-send-block "send block")
					("b" ruby-send-buffer "send buffer")
					("I" inf-ruby-switch-setup "switch setup")

					("RET" vwe-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (ruby-mode))

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
					("w" vwe-key:eww "eww")
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
				  (eww-mode)))

;; ***************************************************************************
;; config
;; ***************************************************************************
(vwe@keybind--init)

(provide 'vwe-keybinds)
;;; vwe-keybinds.el ends here
