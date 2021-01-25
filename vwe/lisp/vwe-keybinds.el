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
	("<f3>"                               split-window-horizontally)
	("<f5>"                               vwe@theme--init)
	("C-<f3>"                             split-window-vertically)
	("<f12>"                              toggle-frame-fullscreen)
	("C-r"                                recentf-open-files)
	("C-'"                                imenu-list-smart-toggle)
	("C-s"                                swiper)
	("C-x C-b"                            ibuffer)
	("C-x C-f"                            counsel-find-file)
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
	("M-\""                               vwe@buffer--switch-to)
	("C-@"                                vwe@lib--minibuffer-switch)
	("C-c C-f"                            format-all-buffer)
	("C-M-["                              mc/edit-beginnings-of-lines)
	("C-M-]"                              mc/edit-ends-of-lines)
	("C-M-|"                              set-rectangular-region-anchor)
	("C-M-="                              cua-rectangle-mark-mode)
	("M-'"                                sp-up-sexp)
	("M-)"                                sp-up-sexp)
	("M-("                                sp-down-sexp)
	("M-C-'"                              sp-backward-unwrap-sexp)
	("M-l"                                string-inflection-lower-camelcase)
	("M-u"                                string-inflection-upcase)
	("M-*"                                treemacs-select-window)
	("M-C-,"                              eacl-complete-multiline)
	("C-]"                                counsel-etags-find-tag-at-point)
	("M-C-?"                              youdao-dictionary-search-at-point+)
	("M-RET"                              mum-key:global)

	([remap comment-dwim]                 comment-dwim-2)
	([remap move-beginning-of-line]       mwim-beginning-of-code-or-line)
	([remap move-end-of-line]             mwim-end-of-code-or-line)
	([remap goto-line]                    goto-line-preview))
  "Default keybind list.")

(defun vwe@keybind--init ()
  "Key bind init."
  (interactive)
  (vwe@lib--keymap-global-set vwe@keybind--default-list))

(with-eval-after-load 'mum-key
  (mum-key-define common
				  (("common" :face (:background "DarkOrange" :foreground "white" :weight bold))
				   (("f" find-file "find file")
					("o" read-only-mode "read only")
					("s" save-buffer "save buffer")
					("w" write-file "save as")
					("k" vwe@lib--buffer-kill-current "kill buffer")
					("n" (lambda () (interactive) (vwe@lib--buffer-kill-other (buffer-list) "^*")) "kill ohter")
					("d" delete-window "del window")
					("x" save-buffers-kill-terminal "kill emacs")
					("S" toggle-frame-fullscreen "fullscreen")
					("F" helpful-function "help func")
					("V" helpful-variable "help var")
					("K" helpful-key "help key")
					("m" (lambda () (interactive) (set-mark (point))) "mark")
					("RET" mum-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold)))))

  (mum-key-define global
				  (("global" :face (:background "DarkOrange" :foreground "white" :weight bold))
				   (("g" counsel-etags-grep "grep")
					("B" ibuffer "ibuffer")
					("@" vwe@lib--minibuffer-switch "switch minibuffer")
					("i" recentf-open-files "recent files")
					("b" vwe@buffer--switch-to "switch buffer")
					("d" dired "dired")
					("s" swiper-thing-at-point "thing at point")
					("r" vwe@lib--replace "replace")
					("x" vwe@lib--frame-reset "reset frame")
					("t" vwe@theme--toggle "toggle theme")
					("f" format-all-buffer "format code")
					("p" vwe@prog--switch-mode "switch mode")
					("n" switch-to-next-buffer "next buffer ")
					("L" display-line-numbers-mode "line nuber")
					("H" mum-headerline-mode "header line")
					("M" mum-modeline-mode "modeline")
					("T" mum-tray-mode "tray")
					("E" mum-editor-mode "editor")
					("v" mum-terminal "term")
					("l" vwe@lsp--run "lsp")
					("o" mum-key:org-extend "org extend")
					("C" mum-key:check "check")
					("P" mum-key:project "project")
					("T" treemacs "treemacs")
					("e" esup "esup")
					("z" customize-group "customize group")
					("*" treemacs-select-window "treemacs window")
					("S" sudo-edit "sudo")
					("!" winum-mode "win number")
					("~" mum-mark-paren--paren-pair "paren mark")
					("(" vwe@base--paren-toggle-style "paren style")
					("+" vwe@ui--text-scale-reset "+/- text scale")
					("?" (lambda () (interactive) (find-file (vwe@lib--path-vwe-lisp "vwe-main.el" t))) "main file")
					("C-f" (lambda () (interactive)
							 (vwe@base--init)
							 (vwe@ui--init)
							 (vwe@theme--init)) "reinit" :footer t)
					("RET" mum-key:common "common" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold)))))

  (mum-key-define check
				  ("check"
				   (("c" flycheck-mode "flycheck mode")
					("v" flycheck-verify-setup "verify checker")
					("l" flycheck-list-errors "error list")
					("RET" mum-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold)))))

  (mum-key-define project
				  ("project"
				   (("p" projectile-mode "projectile mode")
					("g" projectile-grep "grep")
					("a" projectile-add-known-project "add project")
					("o" projectile-switch-open-project "switch open project")
					("s" projectile-switch-project "switch project")
					("RET" mum-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold)))))

  (mum-key-define treemacs
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
					("M-RET" mum-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (treemacs-mode)
				  "C-M-<return>")

  (mum-key-define term
				  (("term" :face (:background "red" :foreground "white" :weight bold))
				   (("c" term-char-mode "char mode")
					("l" term-line-mode "line mode")
					("q" mum-term--exit "exit terminal")
					("RET" mum-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (term-mode))

  (mum-key-define emacs-lisp
				  (("emacs lisp" :face (:background "red" :foreground "white" :weight bold))
				   (("b" eval-buffer "eval buffer")
					("d" eval-defun "eval defun")
					("r" eval-region "eval region")
					("e" eval-expression "eval expression")
					("k" describe-key "key")
					("f" describe-function "function")
					("v" describe-variable "variable")
					("RET" mum-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (emacs-lisp-mode))

  (mum-key-define org-template
				  ("org template"
				   (("a" (vwe@org--template "<a")                              "ascii")
					("c" (vwe@org--template "<c")                              "center")
					("m" (vwe@org--template "<C")                              "comment")
					("e" (vwe@org--template "<e")                              "example")
					("E" (vwe@org--template "<E")                              "export")
					("h" (vwe@org--template "<h")                              "html")
					("x" (vwe@org--template "<l")                              "latex")
					("n" (vwe@org--template "<n")                              "note")
					("u" (vwe@org--template "<q")                              "quote")
					("v" (vwe@org--template "<v")                              "verse")
					("I" (vwe@org--template "<i")                              "index")
					("A" (vwe@org--template "<A")                              "ASCII")
					("C" (vwe@org--template "<I")                              "include")
					("H" (vwe@org--template "<H")                              "html")
					("L" (vwe@org--template "<L")                              "laTeX")
					("s" (vwe@org--template "<s")                              "src")
					("l" (vwe@org--template "<s" "emacs-lisp")                 "elisp")
					("p" (vwe@org--template "<s" "python :results output")     "python")
					("P" (vwe@org--template "<s" "perl")                       "perl")
					("r" (vwe@org--template "<s" "ruby")                       "ruby")
					("S" (vwe@org--template "<s" "shell")                      "shell")
					("g" (vwe@org--template "<s" "go :imports '\(\"fmt\"\)")   "golang")
					("t" (vwe@org--template "<s" "plantuml :file CHANGE.png")  "plantuml")
					("i" self-insert-command                                   "ins")
					("DEL" mum-key:org "org" :footer t :face (:background "magenta" :foreground "white" :weight bold))
					("RET" mum-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold)))))

  (mum-key-define org-extend
				  ("org extend"
				   (("a" org-agenda "agenda")
					("c" org-capture "capture")
					("m" org-roam-insert "roam insert")
					("RET" mum-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold)))))

  (mum-key-define org
				  ("org"
				   (("l" org-insert-link "+link")
					("o" org-open-at-point "open browser")
	  				("a" outline-show-all "show all")
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
					("T" org-todo "todo")
					("I" org-insert-todo-heading "insert todo")
					("y" org-rich-yank "rich yank")
					("s" org-tree-slide-mode "tree slide")
					("r" vwe@org--reveal-load "reveal load")
					("v" org-preview-html/preview "preview html")
					("DEL" mum-key:org-template "org template" :footer t :face (:background "magenta" :foreground "white" :weight bold))
					("RET" mum-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (org-mode)
				  "C-M-<return>")

  (mum-key-define markdown
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
					("RET" mum-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (markdown-mode)
				  "C-M-<return>")

  (mum-key-define gud
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
					("RET" mum-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold)))))

  (mum-key-define clang
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
					("u" mum-key:gud "gud")
					("p" gdb-restore-windows "restore windows")
					("RET" mum-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (c-mode c++-mode))

  (mum-key-define asm
				  ("assembly"
				   (("c" smart-compile "compile")
					("e" compile-goto-error "goto error")
					("g" gdb "gdb")
					("u" mum-key:gud "gud")
					("p" gdb-restore-windows "restore windows")
					("RET" mum-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (asm-mode nasm-mode))

  (mum-key-define python
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
					("RET" mum-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (python-mode))

  (mum-key-define golang
				  ("golang"
				   (("r" go-run "go run")
					("i" go-impl "impl")
					("a" go-import-add "import add")
					("m" go-remove-unused-imports "remove imports")
					("RET" mum-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (go-mode))

  (mum-key-define ruby
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

					("RET" mum-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (ruby-mode))

  (mum-key-define java
				  ("java"
				   (("i" vwe@java--init "java lsp init")
					("o" quickrun-compile-only "compile only")
					("q" quickrun "quickrun")
					("s" quickrun-shell "run shell")
					("RET" mum-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (java-mode))

  (mum-key-define web
				  ("web"
				   (("r" run-skewer "run skewer")
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
					("RET" mum-key:global "global" :footer t :face (:background "DarkOrange" :foreground "white" :weight bold))))
				  (web-mode js2-mode js-mode css-mode))

  (mum-key-define w3m
				  ("w3m"
				   (("w" w3m "w3m")
					("h" w3m-gohome "go home")
					("f" w3m-view-url-with-browse-url "to browse")
					("u" w3m-browse-url "browse url")
					("." w3m-next-buffer "next buffer")
					("," w3m-previous-buffer "prev buffer")))
				  (w3m-mode)))

;; ***************************************************************************
;; config
;; ***************************************************************************
(vwe@keybind--init)

(provide 'vwe-keybinds)
;;; vwe-keybinds.el ends here
