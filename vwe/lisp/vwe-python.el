;;; vwe-python.el --- Python Programming  -*- lexical-binding: t; -*-

;; Copyright(C) 2015  Wu Junyu

;; Author: Wu Junyu < vistar_w@hotmail.com >
;; Keywords:

;; This program is free software you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see < https: // www.gnu.org/licenses/>.


;;; Commentary:

;; pip install mypy
;; pip install pylint
;; pip install flake8
;; pip install autopep8
;; pip install 'python-language-server[all]'
;; pip install rope
;; pip install jedi
;; # importmagic 用来自动引入需要的包
;; pip install importmagic
;; # yapf 用来格式化代码
;; pip install yapf

;;; Code:
;; ***************************************************************************
;; lib
;; ***************************************************************************
(with-eval-after-load 'conda
  (defun vwe@pkg--conda-activate-env()
	(interactive)
	(conda-env-activate)
	(revert-buffer)))

;; ***************************************************************************
;; config
;; ***************************************************************************
(use-package python
  :ensure nil
  :mode
  ("\\.py\\'" . python-mode)
  :interpreter
  ("python3" . python-mode)
  :init
  (setq indent-tabs-mode nil
		python-indent-offset 4
		python-shell-interpreter "python3")
  :config
  ;; 根据autopep8格式化代码
  (use-package py-autopep8
	:after
	(python)
	:hook
	(python-mode . py-autopep8-enable-on-save))

  ;; company 后端
  (use-package company-anaconda
	:after
	(python)
	:init
	(add-to-list 'python-mode-hook(lambda ()
									(add-to-list
									 (make-local-variable
									  'company-backends)
									 'company-anaconda))))
  ;; 代码导航
  (use-package anaconda-mode
	:diminish
	(anaconda-mode "Cd")
	:after
	(python)
	:hook
	((python-mode . anaconda-mode)
	 (python-mode . anaconda-eldoc-mode))
	:init
	(setq anaconda-mode-installation-directory (vwe@lib--path-cache
												"python/anaconda"))
	:config
	(anaconda-mode t)
	(anaconda-eldoc-mode t))

  ;; python 编程支持
  (use-package elpy
	:diminish
	(elpy-mode)
	:init
	(advice-add 'python-mode :before 'elpy-enable)
	(setq elpy-shell-add-to-shell-history t
		  elpy-rpc-virtualenv-path (vwe@lib--path-cache "elpy/rpc-venv")
		  elpy-rpc-python-command "python3"
		  elpy-get-info-from-shell t)
	:config
	(elpy-enable)
	(add-hook 'elpy-mode-hook (lambda ()
								(elpy-shell-set-local-shell
								 (elpy-project-root)))))

  ;; anaconda
  (use-package conda
	:hook
	(python-mode . conda-env-autoactivate-mode)
	:init
	(setq conda-anaconda-home (getenv "CONDAHOME"))
	:config
	(conda-env-initialize-interactive-shells)
	(conda-env-initialize-eshell))

  ;; 整理美化python代码
  (use-package yapfify
	:hook
	(python-mode . yapf-mode)))

(provide 'vwe-python)
;;; vwe-python.el ends here
