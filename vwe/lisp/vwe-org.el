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

(defvar vwe@org--emphasis-alist
  '(("bold (*)" . "*")
	("italic (/)" . "/")
	("underlined (_)" . "_")
	("strikethrough (+)" . "+")
	("code (~)" . "~")
	("verbatim (=)" . "="))
  "Org emphasis alist.")

(defun vwe@org--template (str &optional mod)
  "Insert STR and MOD expand org template."
  (let* ((text))
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


(defun vwe@org--emphasis-text (&optional emph)
  "Insert STR and EMPH expand org sign."
  (interactive
   (list
	(completing-read "emphasis:" (mapcar (lambda(item)
										   (car item))
										 vwe@org--emphasis-alist))))

  (setq emph (cdr (assoc emph vwe@org--emphasis-alist)))

  (if (region-active-p)
	  (save-excursion
		(let* ((begin (region-beginning))
			   (end (region-end)))
		  (goto-char begin)
		  (if (or (equal (line-beginning-position) begin) (equal 32 (char-before)))
			  (insert emph)
			(insert (concat " " emph))
			(setq end (1+ end)))
		  (goto-char (1+ end))
		  (if (or (equal (line-end-position) end) (equal 32 (char-after)))
			  (insert emph)
			(insert (concat emph " ")))))

	(save-excursion
	  (goto-char (line-beginning-position))
	  (insert emph)
	  (goto-char (line-end-position))
	  (insert emph))))

(defun vwe@org--insert-sub-level-element()
  "Insert sub level element."
  (interactive)
  (org-meta-return)
  (org-shiftmetaright))

(defun vwe@org--todo-current-line ()
  "Todo current line."
  (interactive)
  (if (org-at-heading-p)
	  (org-todo)
	(org-toggle-heading)
	(org-todo)))

(defun vwe@org--reveal-load()
  "Load reveal."
  (interactive)
  (vwe@lib--package-load 'ox-reveal))

(defun vwe@org--reveal-insert-split ()
  "Insert split."
  (interactive)
  (insert (format "#+REVEAL: split:t")))

(defun vwe@org--reset-latex-fonts-size (&optional scale)
  "Reset latex fonts size SCALE."
  (interactive)
  (unless scale
	(setq scale (read-number
				 (format "scale(%d):"
						 (plist-get org-format-latex-options :scale)))))
  (setq org-format-latex-options (plist-put org-format-latex-options
											:scale scale)))

(defun vwe@org--create-inline-image-advice (img)
  "Create inline IMG advice."
  (let* ((color (if (equal (frame-parameter nil 'background-mode) 'dark)
					(format "%s" "#5f5f5f")
				  (format "%s" "#c5c5c5c"))))
	(nconc img (list :background color))))

(defun vwe@org--insert-link-and-clipboard-image (width)
  "Org insert clipboard image and insert link.
WIDTH insert to org image width."
  (interactive (list
                (read-string (format "image width: ") nil nil "800")))

  (let* ((dir (concat (file-name-base (buffer-file-name)) ".assets/"))
		 (name (concat "img_" (format-time-string "%Y%m%d_%H%M%S") ".png"))
		 (path (concat (file-name-base (buffer-name)) ".assets/" name)))
	(unless (file-exists-p dir)
      (mkdir dir))

	(cond ((string-equal system-type "gnu/linux")
		   (progn
			 (shell-command (concat "xclip -selection clipboard -t image/png -o > " path))
			 (insert (concat "#+ATTR_NAME: " name
							 "\n#+ATTR_HTML: :width " width
							 "\n[[file:" path "]]\n")))))

	(org-redisplay-inline-images)))

;; ***************************************************************************
;; config
;; ***************************************************************************

;;
;; `org'
;;
(vwe@lib--pkg org
  :init ((add-hook 'org-mode-hook #'toggle-truncate-lines))
  :config (;; (vwe@lib--keymap-set org-mode-map '(("C-<return>" nil)
		   ;; 									   ("C-<return>" org-meta-return)))

		   (advice-add 'org--create-inline-image
					   :filter-return #'vwe@org--create-inline-image-advice)

		   ;; 代码运行环境
		   (vwe@lib--pkg ob-go)
		   (vwe@lib--pkg ob-rust)
		   (vwe@lib--pkg ob-mermaid)

		   (org-babel-do-load-languages 'org-babel-load-languages vwe@org--language-alist)

		   ;;
		   ;; `org-appear' 显示修饰符号
		   ;;
		   (vwe@lib--pkg org-appear
			 :init ((add-hook 'org-mode-hook #'org-appear-mode))
			 :variable ((setq org-appear-autolinks t)))

		   ;;
		   ;; `org-download' 托拽图片
		   ;;
		   (vwe@lib--pkg org-download
			 :init ((add-hook 'org-mode-hook #'org-download-enable)))

		   ;;
		   ;; `org-rich-yank' 快速复制代码块导org
		   ;;
		   (vwe@lib--pkg org-rich-yank)

		   ;;
		   ;; `toc-org' 刷新目录
		   ;;
		   (vwe@lib--pkg toc-org
			 :init ((add-hook 'org-mode-hook #'toc-org-mode))
			 :config ((add-to-list 'org-tag-alist '("TOC" . T))))

		   ;;
		   ;; `org-preview-html' html方式查看org,通过eww
		   ;;
		   (vwe@lib--pkg org-preview-html)

		   ;;
		   ;; `org-tree-slide' 只显示org单一节点内容
		   ;;
		   (vwe@lib--pkg org-tree-slide
			 :init ((add-hook 'org-tree-slide-play (lambda ()
													 (text-scale-increase 4)
													 (org-display-inline-images)
													 (read-only-mode 1)))
					(add-hook 'org-tree-slide-stop (lambda ()
													 (text-scale-increase 0)
													 (org-remove-inline-images)
													 (read-only-mode -1))))
			 :config ((org-tree-slide-simple-profile))
			 :variable ((setq org-tree-slide-skip-outline-level 2)))

		   ;;
		   ;; `org-indent-mode' org缩进
		   ;;
		   (vwe@lib--pkg org-indent-mode
			 :init ((add-hook 'org-mode-hook #'org-indent-mode))
			 :buildin t)

		   ;;
		   ;; `org-superstart'
		   ;;
		   ;; (vwe@lib--pkg org-superstar
		   ;; 	 :init ((add-hook 'org-mode-hook (lambda() (org-superstar-mode 1))))
		   ;; 	 :variable ((setq org-hidden-keywords '(title)
		   ;; 					  org-cycle-level-faces nil
		   ;; 					  ;; org-n-level-faces 7
		   ;; 					  org-superstar-cycle-headline-bullets nil
		   ;; 					  org-superstar-first-inlinetask-bullet ?㊕
		   ;; 					  org-superstar-leading-fallback ?.
		   ;; 					  org-superstar-headline-bullets-list ;; '("☰" "☱" "☲" "☳" "☴" "☶" "☵" "☷" "☯")
		   ;; 					  ;; '("◯" "Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ" "Ⅶ" "Ⅷ" "Ⅸ" "Ⅹ" "Ⅺ" "Ⅻ")
		   ;; 					  ;; '("0" "ⅰ" "ⅱ" "ⅲ" "ⅳ" "ⅴ" "ⅵ" "ⅶ" "ⅷ" "ⅸ" "ⅹ")
		   ;; 					  '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))))

		   ;;
		   ;; `org-num'
		   ;;
		   (vwe@lib--pkg org-num
			 :init ((add-hook 'org-mode-hook #'org-num-mode))
			 :buildin t)

		   ;;
		   ;; `org-bars'
		   ;;
		   (vwe@lib--pkg org-bars
			 :init ((autoload 'org-bars-mode (vwe@lib--path-vwe-site-lisp "org-bars/org-bars.el" t) "Org bars mode." t t)
					(add-hook 'org-mode-hook #'org-bars-mode))
			 :config ((setq org-bars-stars
							'(:empty "○" :invisible "▸" :visible "▾")
							org-bars-color-options
							'(:bar-color "#8c8c8c" :desaturate-level-faces 50 :darken-level-faces 30)))
			 :path (vwe@lib--path-vwe-site-lisp "org-bars"))

		   ;;
		   ;; `ox-reveal' 演示文件生成 ;; https://github.com/hakimel/reveal.js/
		   ;; npm install reveal.js
		   ;;
		   (vwe@lib--pkg ox-reveal
			 :init ((defun vwe@pkg--org-ox-reveal-load ()
					  "Load reveal."
					  (interactive)
					  (load-library "ox-reveal")))
			 :variable ((setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"
							  org-reveal-theme "night"
							  org-reveal-transition "slide"
							  org-reveal-init-options "slideNumber:true"
							  org-reveal-plugins '(classList markdown highlight zoom notes)
							  org-reveal-title-slide
							  "<h2>%t</h2><h3>%s</h3><br><h6 style='text-align:right;color:gray'>%a</h6><h6 style='text-align:right;color:gray'>%d</h6>")))

		   ;;
		   ;; `org-agenda'
		   ;;
		   (vwe@lib--pkg org-agenda
			 :config ((setq org-agenda-files (list (vwe@lib--path-cache "org"))))
			 :buildin t)

		   ;;
		   ;; `org-pomodoro' 提醒计时器
		   ;;
		   (vwe@lib--pkg org-pomodoro)

		   ;;
		   ;; `org-capture' 快速笔记
		   ;;
		   ;; 时间、日期相关
		   ;; 标记 	描述
		   ;; %&lt;…&gt; 	自定义格式的 timestamp，如: %&lt;%Y-%m-%d&gt;，会得到 &lt;2018-03-04 日&gt;
		   ;; %t 	当前仅包含日期的 timestamp，如: &lt;2018-03-04 日&gt;
		   ;; %T 	当前包含日期和时间的 timestamp，如: &lt;2018-03-04 日 19:26&gt;
		   ;; %u 	当前包含日期的未激活的 timestamp，如: [2018-03-04 日]
		   ;; %U 	当前包含日期和时间的未激活的 timestamp，如: [2018-03-04 日 19:26]
		   ;; %^t 	类似 %t，但是弹出日历让用户选择日期
		   ;; %^T 	类似 %T，但是弹出日历让用户选择日期和时间
		   ;; %^u 	类似 %u，但是弹出日历让用户选择日期
		   ;; %^U 	类似 %U，但是弹出日历让用户选择日期和时间
		   ;; 注: 激活(active)和未激活(inactive)的 timestamp 的区别在于，后者不会出现在 agenda 中 —— 所以如果是新建一个 headline 到 org-agenda-files 中并且不希望它出现在 agenda 列表中时，应当使用未激活的 timestamp。
		   ;; 剪贴板相关
		   ;; 标记 	描述
		   ;; %c 	当前 kill ring 中的第一条内容
		   ;; %x 	当前系统剪贴板中的内容
		   ;; %^C 	交互式地选择 kill ring 或剪贴板中的内容
		   ;; %^L 	类似 %^C，但是将选中的内容作为链接插入
		   ;; 标签相关
		   ;; 标记 	描述
		   ;; %^g 	交互式地输入标签，并用 target 所在文件中的标签进行补全
		   ;; %^G 	类似 %^g，但用所有 org-agenda-files 涉及文件中的标签进行补全
		   ;; 文件相关
		   ;; 标记 	描述
		   ;; %[file] 	插入文件 file 中的内容
		   ;; %f 	执行 org-capture 时当前 buffer 对应的文件名
		   ;; %F 	类似 %f，但输入该文件的绝对路径
		   ;; 任务相关
		   ;; 标记 	描述
		   ;; %k 	当前在计时的任务的标题
		   ;; %K 	当前在计时的任务的链接
		   ;; 外部链接的信息
		   ;; 这里的链接不仅仅指如 http://www.google.com 这样的网页链接，还包括文件、邮箱、新闻组、IRC 会话等，详情见 Org mode 手册的 External links 一节。
		   ;; link type 	description
		   ;; bbdb 	BBDB 联系人数据库记录链接
		   ;; irc 	IRC 会话链接
		   ;; vm 	View Mail 邮件阅读器中的消息、目录链接
		   ;; wl 	Wunder Lust 邮件/新闻阅读器中的消息、目录链接
		   ;; mh 	MH-E 邮件用户代理中的消息、目录链接
		   ;; mew 	MEW 邮件阅读器中的消息链接
		   ;; rmail 	Emacs 的默认邮件阅读器 Rmail 中的消息链接
		   ;; gnus 	GNUS 邮件/新闻阅读器中的群组、消息等资源链接
		   ;; eww/w3/w3m 	在eww/w3/w3m 中存储的网页链接
		   ;; calendar 	日历链接
		   ;; org-protocol 	遵循 org-protocol 协议的外部应用链接
		   ;; 这些外部链接，大部分都会在 Emacs 中通过 org-store-link-pros 记录起来，其中会包含这些链接的各个属性，而在 capture 的模板里面，就支持以 %:keyword 的形式来访问这些属性，比如 vm/wl/mh/mew/rmail/gnus 消息中的发件人名称、发件人地址之类的。
		   ;; eww 可用的特殊标记有如下三个
		   ;; 标记 	描述
		   ;; %:type 	固定值，eww
		   ;; %:link 	页面的链接
		   ;; %:description 	页面的标题，如无则为页面的链接
		   ;; org-protocol 可用的特殊标记有如下六个
		   ;; 标记 	描述
		   ;; %:type 	链接的类型，如 http/https/ftp 等
		   ;; %:link 	链接地址，在 org-protocol 里的 url 字段
		   ;; %:description 	链接的标题，在 org-protocol 里的 title 字段
		   ;; %:annotation 	靠 url 和 title 完成的 org 格式的链接
		   ;; %:initial 	链接上选中的文本，在 org-protocol 里的 body 字段
		   ;; %:query 	org-protocol 上除掉开头和子协议部分的剩下部分
		   ;; 此外，在内容模板中还支持自定义函数来插入内容，以 %(sexp) 的形式，比如说我们可以自己写一个 get-current-time 函数来插入当前的时间，那么内容模板可以是这个样子的
		   ;; "%(get-current-time)"
		   ;; 而在内容模板中使用自定义函数时，可以将上面 eww 和 org-protocol 的这些特殊标记作为函数的参数，比如一个场景是，用 org-protocol 捕获的网页 title 中包含中括号，会导致下面这样的内容模板出错
		   ;; "[[%:link][%:description]]"
		   ;; 这个时候可以定一个一个函数来将 %:description 中的中括号替换成下划线
		   ;; (defun replace-bracket-in-title (title)
		   ;;   ;; TODO)
		   ;; 那么上面那个内容模板可以改成这样
		   ;; "[[%:link][%(replace-bracket-in-title \"%:description\")]]"
		   ;; 其他
		   ;; 还有一些特殊标记
		   ;; "%i" 可以插入一段初始化内容，通常是 org-store-link-plist 中 "initial" 属性的值；如果没有的话，会使用当前 buffer 中被选中的内容；都没有的话就什么也不插入。
		   ;; "%^{prop}p" 会提示输入内容，这将会在新增内容中插入一个 property 到 target 中，并且这个 property 的名字是 prop，值则是我们输入的文本。
		   ;; "%^{prompt}" 则会用 prompt 作为提示符要求我们输入，并且用我们输入的文本替换模板中相应的内容。比如说 "%{姓名}" 会用 "姓名" 作为提示符要求输入。当有多个标记时，可以用 "%\N" 来插入第 N 个提示输入标记产生的内容，举个例子，下面的内容模板
		   ;; "- name: %^{姓名}\n- age: %^{年龄}\n\n%\\1的年龄是%\\2"
		   ;; (注: 此处的反斜线「\」需要转义，否则「\1」会被视作值为 1 的 ASCII 码特殊字符，感谢 Emacs China 网友 slack-py 指出该问题)
		   ;; 会要求我们输入姓名和年龄，假如我们输入姓名是 "张三"，年龄是 "25"，那么最后得到的内容是
		   ;; - name: 张三
		   ;; - age: 25
		   ;; 张三的年龄是25
		   ;; "%?" 是一个更特殊的标记，它不会产生任何内容，当所有其他的特殊标记都展开完毕或者输入完毕后，光标将会停留在这个标记所在的位置。
		   (vwe@lib--pkg org-capture
			 :config ((setq org-default-notes-file (vwe@lib--path-cache "org/notes.org" t)
							org-capture-templates `(("t" "Todo" entry
													 (file+headline ,(vwe@lib--path-cache "org/task.org" t) "Task")
													 "* TODO %?\n%^g\nbegin:%^T-end:%^T\n%i\n  %a")
													("q" "QuickNote" entry
													 (file+headline ,(vwe@lib--path-cache "org/quicknote.org" t) "QuickNote")
													 "* TODO %?\n %u\n %i\n  %a")
													("m" "Misc" entry
													 (file+olp+datetree ,(vwe@lib--path-cache "org/misc.org" t))
													 "* %?\n Entered on: %U\n %i\n %a"))))
			 ;; (progn (vwe@lib--path-cache "org/notes.org" t)
			 ;; 		 (vwe@lib--path-cache "org/task.org" t)
			 ;; 		 (vwe@lib--path-cache "org/quicknote.org" t)
			 ;; 		 (vwe@lib--path-cache "org/journal.org" t)
			 ;; 		 (vwe@lib--path-cache "org/appointment.org" t))
			 :buildin t))
  :variable ((setq org-src-fontify-natively t
				   org-hide-emphasis-markers t
				   org-capture-bookmark nil
				   org-log-done 'time
				   org-image-actual-width '(400)
				   org-todo-keywords '((sequence "TODO(t@/!)" "DOING(r@/!)" "WAITING(w@/!)" "VERIFY(v@/!)" "|" "DONE(d@/!)" "CANCELED(c@/!)"))
				   org-todo-keyword-faces '(("TODO" . warning)
											("DOING" . success)
											("WAITING" . error)
											("VERIFY" . error)
											("DONE" . shadow)
											("CANCELED" . shadow)))))

;;
;; `org-roam'
;;
(vwe@lib--pkg org-roam
  :variable ((setq org-roam-directory (vwe@lib--path-cache "org/roam")
				   org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))))

;;
;; `org-brain'
;;
(vwe@lib--pkg org-brain
  :init ((add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer))
  :config ((setq org-id-track-globally t
				 org-id-locations-file (vwe@lib--path-cache "org/brain/.org-id-locations" t)
				 org-brain-visualize-default-choices 'all
				 org-brain-title-max-length 12
				 org-brain-include-file-entries nil
				 org-brain-file-entries-use-title nil))
  :variable ((setq org-brain-path (vwe@lib--path-cache "org/brain"))))

(provide 'vwe-org)
;;; vwe-org.el ends here
