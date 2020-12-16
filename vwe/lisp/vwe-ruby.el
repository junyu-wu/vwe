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
(with-eval-after-load 'rvm
  	(defun vwe@ruby--current-rvm()
	  (format "%s" rvm--current-ruby))
	(defun vwe@ruby--current-gem()
	  (format "%s" rvm--current-gemset)))


;; ***************************************************************************
;; config
;; ***************************************************************************
(use-package ruby-mode
  :ensure nil
  :mode
  ("\\.rb\\'" "Rakefile\\'" "Gemfile\\'")
  :interpreter "ruby"
  :config
  (use-package rvm
	:hook
	(ruby-mode . rvm-activate-corresponding-ruby)
    :config
    (rvm-use-default))

   ;; 连接ruby repl
  (use-package inf-ruby
	:hook
	(ruby-mode . inf-ruby-minor-mode)
	(inf-ruby-mode . (lambda()
					   (company-mode -1))))

  (defun inf-ruby-color()
	(interactive)
	(ansi-color-apply-on-region (point-min) (point-max)))

  ;; 自动添加 'end'
  (use-package ruby-electric
	:diminish
	(ruby-electric-mode)
	:hook
	(ruby-mode . ruby-electric-mode))

  ;; 辅助ruby repl加载程序或gem.包括位置与跳转
  (use-package robe
	:diminish
	(robe-mode . nil)
	:hook
	(ruby-mode . robe-mode)
	:config
	(add-to-list (make-local-variable 'company-backends)
				 '(company-robe)))

  ;; 代码分析与格式化
  ;; flycheck with rubocop
  (use-package rubocop
	:diminish
	(rubocop-mode . nil)
	:hook
	(ruby-mode . rubocop-mode))

  ;; 自动格式化代码
  (use-package rufo
	:diminish
	(rufo-minor-mode)
	:hook
	(ruby-mode . rufo-minor-mode)
	:init
	(setq rufo-minor-mode-use-bundler t))

  (use-package projectile-rails
	:ensure t
	:hook
	(projectile-mode . projectile-rails-on))

  (use-package ruby-test-mode)

  ;; 美化注释
  (use-package yard-mode
	:diminish
	(yard-mode . nil)
	:hook
	(ruby-mode . yard-mode)
	(ruby-mode . eldoc-mode))

  ;; 测试框架
  (use-package rspec-mode
	:diminish
	(rspec-mode . nil)
	:hook
	(dired-mode . rspec-dired-mode)
	:init
	(setq rspec-use-rvm t
  		  compilation-scroll-output t
  		  rspec-allow-multiple-compilation-buffers t)
	:config
	(with-eval-after-load 'yasnippet
  	  (rspec-install-snippets))))

;; 后端支持
(use-package solargraph
  :load-path
  (lambda ()
	(vwe@lib--path-vwe-site-lisp "emacs-solargraph"))
  :hook
  (ruby-mode . (lambda() (vwe@lib--package-load 'solargraph))))

(provide 'vwe-ruby)
;;; vwe-Ruby.el ends here
