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
  '(("<f3>"                               split-window-horizontally)
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
	("M-*"                                treemacs)
	("M-C-,"                              eacl-complete-multiline)
	("C-]"                                counsel-etags-find-tag-at-point)
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
  (mum-key-define global
				  ("global"
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
					("T" mum-modeline-tray-mode "tray")
					("l" vwe@lsp--run "lsp")
					("a" org-agenda "agenda")
					("c" org-capture "capture")
					("e" esup "esup")
					("!" winum-mode "win number")
					("~" mum-mark--paren--paren-pair "paren mark")
					("(" vwe@base--paren-toggle-style "paren style")
					("+" vwe@ui--text-scale-reset "+/- text scale"))))

  (mum-key-define emacs-lisp
				  ("emacs lisp"
				   (("b" eval-buffer "eval buffer")
					("d" eval-defun "eval defun")
					("r" eval-region "eval region")
					("e" eval-expression "eval expression")
					("k" describe-key "key")
					("f" describe-function "function")
					("v" describe-variable "variable")
					("RET" mum-key:global "global" :footer t)))
				  emacs-lisp-mode)

  (define-key emacs-lisp-mode-map (kbd "C-c a") 'mum-key:emacs-lisp)
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
					("DEL" mum-key:org "org" :footer t)
					("RET" mum-key:global "global" :footer t))))

  (mum-key-define org
				  ("org"
				   (("l" org-insert-link "+link")
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
					("DEL" mum-key:org-template "org template" :footer t)
					("RET" mum-key:global "global" :footer t)))
				  org-mode
				  "C-M-<return>")

  (mum-key-define markdown
				  ("markdown"
				   (("l" markdown-live-preview-mode "live preview mode")
					("b" markdown-insert-gfm-code-block "code block")
					("c" markdown-insert-code "code" :color teal)
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
					("RET" mum-key:global "global" :footer t)))
				  markdown-mode
				  "C-M-<return>")

  (mum-key-define python
				  ("python"
				   (("r" run-python "run python")
                    ("c" conda-env-activate "conda activate")
					("d" conda-env-deactivate "conda deactivate")
					("l" conda-env-list "conda list")
					("e" elpy-enable "elpy enable")
					("s" elpy-shell-switch-to-shell "swithc shell")
					("j" elpy-django-runserver "run django")
					("b" python-shell-send-buffer "send buffer")
					("r" python-shell-send-region "send region")
					("RET" mum-key:global "global" :footer t)))
				  python-mode)
  )

;; ***************************************************************************
;; config
;; ***************************************************************************

(vwe@keybind--init)

(provide 'vwe-keybinds)
;;; vwe-keybinds.el ends here
