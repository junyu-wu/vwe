;;; vwe-display.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2015  WuJunyu

;; Author: WuJunyu <vistar_w@hotmail.com>
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
(use-package  all-the-icons
  :if
  (display-graphic-p))
(require 'all-the-icons)



(when vwiss/sys--mac-p
  ;; MacOS 设置标题栏透明
  (use-package ns-auto-titlebar
	:hook
	(after-init . ns-auto-titlebar-mode)
	:config
	(add-to-list 'default-frame-alist
				 '(ns-transparent-titlebar . t))))

;; 根据当前行排序行号
(use-package linum-relative
  :init
  (setq linum-relative-backend 'display-line-numbers-mode))


;; 高亮关键字
(use-package hl-todo
  :diminish
  (hl-todo-mode "")
  :hook
  (after-init . global-hl-todo-mode)
  :config
  (dolist (keyword '("BUG" "DEFECT" "ISSUE"))
	(cl-pushnew `(,keyword . ,(face-foreground 'error))
				hl-todo-keyword-faces))
  (dolist (keyword '("WORKAROUND" "HACK" "TRICK"))
	(cl-pushnew `(,keyword . ,(face-foreground 'warning))
				hl-todo-keyword-faces))
  (with-eval-after-load 'pretty-hydra
	(pretty-hydra-define hl-todo-hydra
	  (:color amaranth :quit-key "q"
			  :title (vwiss/hydra--generate-mode-icon-title))
	  ("HL-Todo Keymap"
	   (("t" hl-todo-mode "hlTodo" :toggle t)
		("o" hl-todo-occur "occur"))
	   ""
	   (("p" hl-todo-previous "previous")
		("n" hl-todo-next "next"))
	   ""
	   (("RET" global-hydra/body nil :color teal)
		("?" (vwiss/keymap--goto-keymap-define "init-display.el" "HL-Todo Keymap") :color teal)
		("ESC" nil :exit t))))))

;; 高亮symbol
(use-package symbol-overlay
  :diminish
  (symbol-overlay-mode "")
  :hook
  (prog-mode . symbol-overlay-mode)
  :init
  (setq symbol-overlay-idle-time 0.1)
  (with-eval-after-load 'all-the-icons
	(setq symbol-overlay-faces
		  '((:inherit (all-the-icons-blue bold) :inverse-video t)
			(:inherit (all-the-icons-pink bold) :inverse-video t)
			(:inherit (all-the-icons-yellow bold) :inverse-video t)
			(:inherit (all-the-icons-maroon bold) :inverse-video t)
			(:inherit (all-the-icons-red bold) :inverse-video t)
			(:inherit (all-the-icons-orange bold) :inverse-video t)
			(:inherit (all-the-icons-green bold) :inverse-video t)
			(:inherit (all-the-icons-cyan bold) :inverse-video t))))
  :config
  (defun turn-off-symbol-overlay (&rest _)
	"Turn off symbol highlighting."
	(interactive)
	(symbol-overlay-mode -1))
  (advice-add #'set-mark :after #'turn-off-symbol-overlay)
  (defun turn-on-symbol-overlay (&rest _)
	"Turn on symbol highlighting."
	(interactive)
	(when (derived-mode-p 'prog-mode)
	  (symbol-overlay-mode 1)))
  (advice-add #'deactivate-mark :after #'turn-on-symbol-overlay))

;; 显示/隐藏部分块
(use-package hideshow
  :ensure nil
  :diminish
  (hs-minor-mode)
  :hook
  (prog-mode . hs-minor-mode)
  :config
  (with-eval-after-load 'pretty-hydra
	(pretty-hydra-define hideshow-hydra
	  (:color amaranth :quit-key "q"
			  :title (vwiss/hydra--generate-mode-icon-title))
	  ("Hideshow Keymap"
	   (("h" hs-toggle-hiding "hiding")
		("s" hs-show-block "show"))
	   ""
	   (("RET" global-hydra/body nil :color teal)
		("?" (vwiss/keymap--goto-keymap-define "init-display.el"
											   "Hideshow Keymap") :color teal)
		("ESC" nil :exit t))))))

;; 代码折叠
(use-package origami
  :hook
  (prog-mode . origami-mode)
  :init
  (setq origami-show-fold-header t)
  :config
  (with-eval-after-load 'pretty-hydra
	(pretty-hydra-define origami-hydra
	  (:color amaranth :quit-key "q"
			  :title (vwiss/hydra--generate-mode-icon-title))
	  ("Origami Kyemap"
	   (("o" origami-open-node "open node")
		("O" origami-open-node-recursively "recursive open")
		("s" origami-show-node "show node")
		("c" origami-close-node "close node")
		("C" origami-close-node-recursively "recursive close"))
	   ""
	   (("t" origami-toggle-node "toggle node")
		("f" origami-forward-toggle-node "forward node")
		("r" origami-recursively-toggle-node "recursive node")
		("a" origami-open-all-nodes "open all")
		("k" origami-close-all-nodes "close all"))
	   ""
	   (("A" origami-toggle-all-nodes "toggle all")
		("h" origami-show-only-node "show only")
		("p" origami-previous-fold "prev fold")
		("n" origami-next-fold "next fold")
		("l" origami-forward-fold "forward fold"))
	   ""
	   (("w" origami-forward-fold-same-level "forward fold")
		("W" origami-backward-fold-same-level "backward level")
		("u" origami-undo "undo")
		("e" origami-redo "redp")
		("R" origami-reset "reset"))
	   ""
	   (("RET" global-hydra/body nil :color teal)
		("?" (vwiss/keymap--goto-keymap-define "init-display.el" "Origami Keymap") :color teal)
		("ESC" nil :exit t))))))

;; 显示缩进样式
(use-package highlight-indent-guides
  :diminish
  (highlight-indent-guides "")
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character
		highlight-indent-guides-character ?\|
		highlight-indent-guides-responsive 'top)
  :config
  (defun vwiss/show--highlighter (level responsive display)
	(if (> 1 level)
		nil
	  (highlight-indent-guides--highlighter-default
	   level responsive display)))
  (setq highlight-indent-guides-highlighter-function
		'vwiss/show--highlighter))

;; 透明度
(use-package transparency
  :ensure nil
  :load-path
  (lambda ()
	(vwiss/home--site-lisp-load-path "trans"))
  ;; "~/.emacs.d/site-lisp/trans"
  :bind
  (:map global-map
		("C-M-z" . trans-frame-toggle-perspective)
		("M-z" . trans-frame-perspective))
  :init
  (with-eval-after-load 'pretty-hydra
	(pretty-hydra-define transparency-hydra
	  (:color amaranth :quit-key "q"
			  :title (vwiss/hydra--generate-mode-icon-title))
	  ("Transparency Keymap"
	   (("t" trans-frame-toggle-perspective "toggle transparency"))
	   ""
	   (("p" trans-frame-perspective "transparency"))
	   ""
	   (("RET" global-hydra/body nil :color teal)
		("?" (vwiss/keymap--goto-keymap-define "init-display.el" "Transparency Keymap") :color teal)
		("ESC" nil :exit t))))))

;; 光标提示，拖尾
(use-package beacon
  :diminish
  (beacon-mode)
  :hook
  (after-init . beacon-mode)
  :config
  (beacon-mode t))

;; 显示标记并快速跳转
(use-package virtual-mark
  :load-path
  (lambda ()
	(vwiss/home--site-lisp-load-path "virtual-mark"))
  :bind
  (("M-f" . vm/position--forward-word)
   ("M-b" . vm/position--backward-word))
  :hook
  (after-init . virtual-mark-mode))

(defun vwiss/display--init()
  "Display init."
  (interactive)
  ;; 禁止光标闪烁
  (blink-cursor-mode -1)
  (unless vwiss/display--show-menu-bar
	(menu-bar-mode -1))
  (unless vwiss/display--show-tool-bar
	(tool-bar-mode -1))
  (unless vwiss/display--show-scroll-bar
	(scroll-bar-mode -1))
  (when (display-graphic-p)
	(vwiss/display--default-frame)))

(defun vwiss/display--daemon-after-init()
  "Display daemon after init."
  (interactive)
  ;; 禁止光标闪烁
  (blink-cursor-mode -1)
  (unless vwiss/display--show-menu-bar
	(menu-bar-mode -1))
  (unless vwiss/display--show-tool-bar
	(tool-bar-mode -1))
  (unless vwiss/display--show-scroll-bar
	(scroll-bar-mode -1))
  (if (vwiss/sys--daemon-x-p)
	  (progn
		(vwiss/display--default-frame)
		(with-eval-after-load 'hydra-popframe
		  (hydra-posframe-enable)))
	(progn
	  (with-eval-after-load 'hydra-popframe
		(hydra-popframe-mode -1)))))

(vwiss/display--init)

(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define show-line-hydra
	(:color amaranth :quit-key "q"
			:title (vwiss/hydra--generate-mode-icon-title))
	("Show Line Keymap"
	 (("l" toggle-truncate-lines "truncate lines" :toggle truncate-lines)
	  ("w" toggle-word-wrap "word wrap" :toggle word-wrap))
	 ""
	 (("f" display-fill-column-indicator-mode "fill column" :toggle t)
	  ("v" visual-line-mode "visual line" :toggle t))
	 ""
	 (("RET" global-hydra/body nil :color teal)
	  ("?" (vwiss/keymap--goto-keymap-define "init-display.el" "Show Line Keymap") :color teal)
	  ("ESC" nil :exit t)))))

(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define display-hydra
	(:color amaranth :quit-key "q"
			:title (vwiss/hydra--generate-mode-icon-title))
    ("Display Keymap"
	 (("h" hydra-popframe-mode "hydra posframe" :toggle t)
	  ("l" display-line-numbers-mode "line number" :toggle t)
	  ("r" linum-relative-mode "relative line" :toggle t)
	  ("i" hideshow-hydra/body (vwiss/face--with-face "hide block" :inherit 'button) :color teal))
	 ""
	 (("s" show-line-hydra/body (vwiss/face--with-face "show line" :inherit 'button) :color teal)
	  ("t" transparency-hydra/body (vwiss/face--with-face "transparency" :inherit 'button) :color teal)
	  ("o" origami-hydra/body (vwiss/face--with-face "origami" :inherit 'button) :color teal)
	  ("d" hl-todo-hydra/body (vwiss/face--with-face "hl-todo" :inherit 'button) :color teal))
	 ""
	 (("e" vwiss/env--set-font "set font" :color teal)
	  ("n" vwiss/env--set-font-size "set font size" :color teal)
	  ("f" toggle-frame-fullscreen "fullscrenn" :color teal)
	  ("m" toggle-frame-maximized "maximized" :color teal))
	 ""
	 (("a" vwiss/display--default-frame "default frame" :color teal)
	  ("p" vwiss/color--regexp-at-point "regexp color" :color teal)
	  ("u" vwiss/color--unhighlight-regexp "unregexp color" :color teal)
	  ("v" vm/paren--paren-pair "show paren mark" :color teal))
	 ""
	 (("RET" global-hydra/body nil :color teal)
	  ("?" (vwiss/keymap--goto-keymap-define "init-display.el" "Display Keymap") :color teal)
	  ("ESC" nil :exit t)))))

(provide 'vwe-display)
;;; vwe-display.el ends here
