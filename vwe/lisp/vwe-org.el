;;; vwe-org.el --- ORG Mode Config          -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Wu Junyu

;; Author: Wu Junyu <wujunyu@live.com>
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
;; apt install imagemagick

;;; Code:
;; ***************************************************************************
;; lib
;; ***************************************************************************
(defvar vwe@org--language-alist
  '((emacs-lisp . t)
	(perl . t)
	(python . t)
	(ruby . t)
	(js . t)
	(css . t)
	(C . t)
	(java . t)
	(go . t)
	(rust . t)
	(shell . t)
	(mermaid . t))
  "Org language alist.")

(with-eval-after-load 'mum-key
  (mum-key-define org-template
				  ("org template"
				   (("a" (vwiss/org--template-expand "<a")                              "ascii")
					("c" (vwiss/org--template-expand "<c")                              "center")
					("m" (vwiss/org--template-expand "<C")                              "comment")
					("e" (vwiss/org--template-expand "<e")                              "example")
					("E" (vwiss/org--template-expand "<E")                              "export")
					("h" (vwiss/org--template-expand "<h")                              "html")
					("x" (vwiss/org--template-expand "<l")                              "latex")
					("n" (vwiss/org--template-expand "<n")                              "note")
					("u" (vwiss/org--template-expand "<q")                              "quote")
					("v" (vwiss/org--template-expand "<v")                              "verse")
					("I" (vwiss/org--template-expand "<i")                              "index")
					("A" (vwiss/org--template-expand "<A")                              "ASCII")
					("C" (vwiss/org--template-expand "<I")                              "include")
					("H" (vwiss/org--template-expand "<H")                              "html")
					("L" (vwiss/org--template-expand "<L")                              "laTeX")
					("s" (vwiss/org--template-expand "<s")                              "src")
					("l" (vwiss/org--template-expand "<s" "emacs-lisp")                 "elisp")
					("p" (vwiss/org--template-expand "<s" "python :results output")     "python")
					("P" (vwiss/org--template-expand "<s" "perl")                       "perl")
					("r" (vwiss/org--template-expand "<s" "ruby")                       "ruby")
					("S" (vwiss/org--template-expand "<s" "shell")                      "shell")
					("g" (vwiss/org--template-expand "<s" "go :imports '\(\"fmt\"\)")   "golang")
					("t" (vwiss/org--template-expand "<s" "plantuml :file CHANGE.png")  "plantuml")
					("i" self-insert-command                                            "ins")
					("RET" mum-key:org                                                  "org" :footer t)))))

(with-eval-after-load 'mum-key
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
					("RET" mum-key:global "global" :footer t)))))

(defun vwe@org--template-expand (str &optional mod)
  "Expand org template.
insert STR and MOD."
  (let (text)
	(when (region-active-p)
	  (setq text (buffer-substring (region-beginning) (region-end)))
	  (delete-region (region-beginning) (region-end)))
	(insert str)
	(if (fboundp 'org-try-structure-completion)
		(org-try-structure-completion) ; < org 9
	  (progn
		;; New template expansion since org 9
		(require 'org-tempo nil t)
		(org-tempo-complete-tag)))
	(when mod (insert mod) (forward-line))
	(when text (insert text))))

(defun vwe@org--insert-sub-level-element()
  "Insert sub level element."
  (interactive)
  (org-meta-return)
  (org-shiftmetaright))

(defun vwe@org--reveal-load()
  "Load reveal."
  (interactive)
  (vwe@lib--package-load 'ox-reveal))

;; ***************************************************************************
;; config
;; ***************************************************************************
(use-package org
  :ensure nil
  :hook
  (org-mode . toggle-truncate-lines)
  :bind
  (:map org-mode-map
		("C-RET" . org-meta-return)
		("M-RET" . mum-key:org))
  :init
  (setq org-hide-leading-start t
		org-src-fontify-natively t
		org-log-done 'time
		org-image-actual-width '(400)
		org-todo-keywords '((sequence "TODO(t)"
									  "DOING(i)"
									  "WAITING(w)"
									  "|"
									  "DONE(d)"
									  "CANCEL(c)")))
  :config

  (with-eval-after-load 'org
	(define-key org-mode-map (kbd "M-<return>") #'mum-key:org))

  ;; 代码运行环境
  (use-package ob-go)
  (use-package ob-rust)
  (use-package ob-mermaid)

  (org-babel-do-load-languages 'org-babel-load-languages vwe@org--language-alist)

  ;; 托拽图片
  (use-package org-download
	:hook
	(org-mode-hook . org-download-enable))

  ;; 快速复制代码块导org
  (use-package org-rich-yank)

  ;; 刷新目录
  (use-package toc-org
	:hook
	(org-mode . toc-org-mode)
	:config
	(add-to-list 'org-tag-alist '("TOC" . T)))

  ;; html方式查看org,通过eww
  (use-package org-preview-html
	:diminish
	(org-preview-html-mode))

  ;; 只显示org单一节点内容
  (use-package org-tree-slide
	:diminish
	:functions
	(org-display-inline-images
	 org-remove-inline-images)
	:hook
	((org-tree-slide-play . (lambda ()
							  (text-scale-increase 4)
							  (org-display-inline-images)
							  (read-only-mode 1)))
	 (org-tree-slide-stop . (lambda ()
							  (text-scale-increase 0)
							  (org-remove-inline-images)
							  (read-only-mode -1))))
	:init
	(setq org-tree-slide-skip-outline-level 2)
	:config
	(org-tree-slide-simple-profile))

  ;; org缩进
  (use-package org-indent-mode
	:ensure nil
	:diminish
	(org-indent-mode . nil)
	:hook
	(org-mode . org-indent-mode))

  (use-package org-superstar
	:hook
	(org-mode . (lambda() (org-superstar-mode 1)))
	:init
	(setq org-hidden-keywords '(title)
		  org-cycle-level-faces nil
		  ;; org-n-level-faces 7
		  org-superstar-cycle-headline-bullets nil
		  org-superstar-first-inlinetask-bullet ?㊕
		  org-superstar-leading-fallback ?.
		  org-superstar-headline-bullets-list '("☰" "☱" "☲" "☳"
												"☴" "☶" "☵" "☷"
												"☯")))

  ;; 演示文件生成
  ;; https://github.com/hakimel/reveal.js/
  ;; npm install reveal.js
  (use-package ox-reveal
	:init
	(defun vwe@pkg--org-ox-reveal-load ()
	  "Load reveal."
	  (interactive)
	  (load-library "ox-reveal"))
	(setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"
		  org-reveal-theme "night"
		  org-reveal-transition "slide"
		  org-reveal-init-options "slideNumber:true"
		  org-reveal-plugins '(classList markdown highlight zoom notes)
		  org-reveal-title-slide
		  "<h2>%t</h2>
         <h3>%s</h3><br>
         <h6 style='text-align:right;color:gray'>%a</h6>
         <h6 style='text-align:right;color:gray'>%d</h6>"))

  (use-package org-agenda
	:ensure nil
	:init
	(setq org-agenda-files (list (vwe@lib--path-cache "org"))))

  ;; 提醒计时器
  (use-package org-pomodoro)

  ;; 快速笔记
  (use-package org-capture
	:ensure nil
	:defines
	(default-notes-file)
	:init
	(setq default-notes-file (vwe@lib--path-cache "org/notes.org" t)
		  org-capture-templates '(("t" "Todo" entry
								   (file+headline (vwe@lib--path-cache "org/task.org" t) "Task")
								   "* TODO %?\n  %i\n  %a")
								  ("q" "QuickNote" entry
								   (file+headline (vwe@lib--path-cache "org/quicknote.org" t) "QuickNote")
								   "* TODO %?\n  %i\n  %a")
								  ("j" "Journal" entry
								   (file+olp+datetree (vwe@lib--path-cache "org/journal.org" t))
								   "* %?\n Entered on: %U\n %i\n %a")
								  ("a" "Appointment" entry
								   (file (vwe@lib--path-cache "org/appointment.org" t)
										 "* %?\n%^T\n** Note:\n\n")))))

  ;; 便签
  (use-package org-roam
	:diminish
	(org-roam "")
	:init
	(setq org-roam-directory (vwe@lib--path-cache "org/roam")))

  ;; org文件组织,类似思维导图
  (use-package org-brain
	:hook
	(before-save-hook . org-brain-ensure-ids-in-buffer)
	:init
	(setq org-brain-path (vwe@lib--path-cache "org/brain"))
	:config
	(setq org-id-track-globally t
		  org-id-locations-file (vwe@lib--path-cache "org/brain/.org-id-locations" t)
		  org-brain-visualize-default-choices 'all
		  org-brain-title-max-length 12
		  org-brain-include-file-entries nil
		  org-brain-file-entries-use-title nil)
	(push '("b" "Brain" plain (function org-brain-goto-end)
			"* %i%?" :empty-lines 1)
		  org-capture-templates)))

(provide 'vwe-org)
;;; vwe-org.el ends here
