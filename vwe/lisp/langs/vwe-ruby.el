;;; vwe-ruby.el --- Ruby Programming     -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Wu Junyu

;; Author: Wu Junyu <vistar_@hotmail.com>
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
;; gem install pry pry-doc rubocop
;; gem install solargraph  ;; lsp

;;; Code:
;; ***************************************************************************
;; lib
;; ***************************************************************************

;; ***************************************************************************
;; config
;; ***************************************************************************

;;
;; `ruby'
;;
(vwe@lib--pkg ruby-mode
  :config (;;
		   ;; `rvm'
		   ;;
		   ;; (vwe@lib--pkg rvm
		   ;; 	 :init ((add-hook 'ruby-mode-hook
		   ;; 					  (lambda ()
		   ;; 						(rvm-activate-corresponding-ruby)
		   ;; 						(rvm-use-default)))))

		   ;;
		   ;; `rbenv'
		   ;;
		   (vwe@lib--pkg rbenv)

		   ;;
		   ;; `inf-ruby' 连接ruby repl
		   ;;
		   (vwe@lib--pkg inf-ruby
			 :init ((add-hook 'ruby-mode-hook #'inf-ruby-minor-mode))
			 ;; :config ((with-eval-after-load 'rvm
			 ;; 			(advice-add 'inf-ruby-console-auto
			 ;; 						:before #'rvm-activate-corresponding-ruby)))
			 :variable ((vwe@lib--keymap-set compilation-shell-minor-mode-map
											 '(("M-RET" nil)))))
		   ;;
		   ;; `robe' 辅助ruby repl加载程序或gem.包括位置与跳转
		   ;;
		   (vwe@lib--pkg robe
			 :init ((add-hook 'ruby-mode-hook #'robe-mode)
					(with-eval-after-load 'company
					  (add-hook 'ruby-mode-hook
								(lambda ()
								  (vwe@pkg--company-make-mode-local-backends
								   'company-robe))))))

		   ;;
		   ;; `ruby-electric' 自动添加 'end'
		   ;;
		   (vwe@lib--pkg ruby-electric
			 :init ((add-hook 'ruby-mode-hook #'ruby-electric-mode))
			 :config ((vwe@lib--keymap-set ruby-electric-mode-map
										   '(("SPC" nil)))))

		   ;;
		   ;; `rubocop' 代码分析与格式化 flycheck with rubocop
		   ;;
		   (vwe@lib--pkg rubocop
			 :init ((add-hook 'ruby-mode-hook #'rubocop-mode)))
		   (vwe@lib--pkg rubocopfmt
			 :init ((add-hook 'before-save-hook #'rubocopfmt nil t)))

		   ;;
		   ;; `ruby-test-mode'
		   ;;
		   (vwe@lib--pkg ruby-test-mode)

		   ;;
		   ;; `solargraph' 后端支持
		   ;;
		   (vwe@lib--pkg solargraph
			 :init ((add-hook 'ruby-mode-hook (lambda() (vwe@lib--package-load 'solargraph))))
			 :path (vwe@lib--path-vwe-site-lisp "emacs-solargraph"))
		   ))


(provide 'vwe-ruby)
;;; vwe-Ruby.el ends here
