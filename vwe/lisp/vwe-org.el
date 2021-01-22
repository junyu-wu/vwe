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

(defun vwe@org--template (str &optional mod)
  "Insert STR and MOD expand org template."
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

;;
;; `org'
;;
(vwe@lib--package 'org
				  (add-hook 'org-mode-hook #'toggle-truncate-lines)
				  (progn
					(define-key org-mode-map (kbd "C-RET") #'org-meta-return)

					;; 代码运行环境
					(vwe@lib--package 'ob-go)
					(vwe@lib--package 'ob-rust)
					(vwe@lib--package 'ob-mermaid)

					(org-babel-do-load-languages 'org-babel-load-languages vwe@org--language-alist)

					;;
					;; `org-download' 托拽图片
					;;
					(vwe@lib--package 'org-download
					  				  (add-hook 'org-mode-hook #'org-download-enable))

					;;
					;; `org-rich-yank' 快速复制代码块导org
					;;
					(vwe@lib--package 'org-rich-yank)

					;;
					;; `toc-org' 刷新目录
					;;
					(vwe@lib--package 'toc-org
					  				  (add-hook 'org-mode-hook #'toc-org-mode)
									  (add-to-list 'org-tag-alist '("TOC" . T)))

					;;
					;; `org-preview-html' html方式查看org,通过eww
					;;
					(vwe@lib--package 'org-preview-html)

					;;
					;; `org-tree-slide' 只显示org单一节点内容
					;;
					(vwe@lib--package 'org-tree-slide
					  				  (progn
										(add-hook 'org-tree-slide-play (lambda ()
																		 (text-scale-increase 4)
																		 (org-display-inline-images)
																		 (read-only-mode 1)))
										(add-hook 'org-tree-slide-stop (lambda ()
																		 (text-scale-increase 0)
																		 (org-remove-inline-images)
																		 (read-only-mode -1))))
									  (org-tree-slide-simple-profile)
									  (setq org-tree-slide-skip-outline-level 2))

					;;
					;; `org-indent-mode' org缩进
					;;
					(vwe@lib--package 'org-indent-mode
					  				  (add-hook 'org-mode-hook #'org-indent-mode)
									  nil nil nil nil t)

					;;
					;; `org-superstart'
					;;
					(vwe@lib--package 'org-superstar
					  				  (add-hook 'org-mode-hook (lambda() (org-superstar-mode 1)))
									  nil
									  (setq org-hidden-keywords '(title)
											org-cycle-level-faces nil
											;; org-n-level-faces 7
											org-superstar-cycle-headline-bullets nil
											org-superstar-first-inlinetask-bullet ?㊕
											org-superstar-leading-fallback ?.
											org-superstar-headline-bullets-list '("☰" "☱" "☲" "☳" "☴" "☶" "☵" "☷" "☯")))

					;;
					;; `ox-reveal' 演示文件生成 ;; https://github.com/hakimel/reveal.js/
					;; npm install reveal.js
					;;
					(vwe@lib--package 'ox-reveal
					  				  (defun vwe@pkg--org-ox-reveal-load ()
										"Load reveal."
										(interactive)
										(load-library "ox-reveal"))
									  nil
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

					;;
					;; `org-agenda'
					;;
					(vwe@lib--package 'org-agenda
									  nil
									  (setq org-agenda-files (list (vwe@lib--path-cache "org")))
									  nil nil nil t)

					;;
					;; `org-pomodoro' 提醒计时器
					;;
					(vwe@lib--package 'org-pomodoro)

					;;
					;; `org-capture' 快速笔记
					;;
					(vwe@lib--package 'org-capture
									  nil
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
																		   "* %?\n%^T\n** Note:\n\n"))))
									  nil nil nil t))
				  (setq org-hide-leading-start t
						org-src-fontify-natively t
						org-log-done 'time
						org-image-actual-width '(400)
						org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "WAITING(w)" "|" "DONE(d)" "CANCEL(c)"))))

;;
;; `org-roam'
;;
(vwe@lib--package 'org-roam
				  nil nil
				  (setq org-roam-directory (vwe@lib--path-cache "org/roam")
						org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory)))

;;
;; `org-brain'
;;
(vwe@lib--package 'org-brain
				  (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
				  (progn
					(setq org-id-track-globally t
						  org-id-locations-file (vwe@lib--path-cache "org/brain/.org-id-locations" t)
						  org-brain-visualize-default-choices 'all
						  org-brain-title-max-length 12
						  org-brain-include-file-entries nil
						  org-brain-file-entries-use-title nil))
				  (setq org-brain-path (vwe@lib--path-cache "org/brain")))

(provide 'vwe-org)
;;; vwe-org.el ends here
