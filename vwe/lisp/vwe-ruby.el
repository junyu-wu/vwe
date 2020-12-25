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
;; gem install pry pry-doc
;; gem install rufo
;; gem install rubocop
;; gem install ripper-tags
;; gem install method_source
;; gem install solargraph  ;; lsp

;;; Code:
;; ***************************************************************************
;; lib
;; ***************************************************************************

;; ***************************************************************************
;; config
;; ***************************************************************************
(use-package rvm
  :hook
  (ruby-mode . rvm-activate-corresponding-ruby)
  :config
  (rvm-use-default))

;; 连接ruby repl
(use-package inf-ruby
  :hook
  (ruby-mode . inf-ruby-minor-mode))

;; 自动添加 'end'
(use-package ruby-electric
  :hook
  (ruby-mode . ruby-electric-mode))

;; 辅助ruby repl加载程序或gem.包括位置与跳转
(use-package robe
  :hook
  (ruby-mode . robe-mode)
  :config
  (add-to-list (make-local-variable 'company-backends)
			   '(company-robe)))

;; 代码分析与格式化 flycheck with rubocop
(use-package rubocop
  :hook
  (ruby-mode . rubocop-mode))

;; 自动格式化代码
(use-package rufo
  :hook
  (ruby-mode . rufo-minor-mode)
  :init
  (setq rufo-minor-mode-use-bundler t))

;; 后端支持
(use-package solargraph
  :load-path
  (lambda ()
	(vwe@lib--path-vwe-site-lisp "emacs-solargraph"))
  :hook
  (ruby-mode . (lambda() (vwe@lib--package-load 'solargraph))))

(provide 'vwe-ruby)
;;; vwe-Ruby.el ends here
