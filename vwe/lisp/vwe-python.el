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

;; pip install mypy pylint flake8 autopep8
;; pip install 'python-language-server[all]'
;; pip install rope jedi
;; pip install importmagic ;; 用来自动引入需要的包
;; pip install yapf ;; 用来格式化代码

;;; Code:
;; ***************************************************************************
;; lib
;; ***************************************************************************
(with-eval-after-load 'mum-key
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
					("RET" mum-key:global "global" :footer t)))))

;; ***************************************************************************
;; config
;; ***************************************************************************
(use-package python
  :ensure nil
  :mode
  ("\\.py\\'" . python-mode)
  :interpreter
  ("python3" . python-mode)
  :bind
  (:map python-mode-map
		("M-RET" . mum-key:python))
  :init
  (setq indent-tabs-mode nil
		python-indent-offset 4
		python-shell-interpreter "python3")
  :config
  ;; 根据autopep8格式化代码
  (use-package py-autopep8
	:hook
	(python-mode . py-autopep8-enable-on-save))

  ;; company 后端
  (use-package company-anaconda
	:hook
	(python-mode-hook . (lambda ()
						  (add-to-list (make-local-variable 'company-backends)
									   'company-anaconda))))

  (use-package elpy
	:hook
	((python-mode . elpy-enable)
	 (elpy-mode-hook . (lambda ()
						 (elpy-shell-set-local-shell (elpy-project-root)))))
	:init
	(setq elpy-shell-add-to-shell-history t
		  elpy-rpc-virtualenv-path (vwe@lib--path-cache "elpy/rpc-venv")
		  elpy-rpc-python-command "python3"
		  elpy-get-info-from-shell t))

  ;; anaconda
  (use-package conda
	:init
	(setq conda-anaconda-home (getenv "CONDA_HOME"))
	:config
	(conda-env-initialize-interactive-shells))

  ;; 整理美化python代码
  (use-package yapfify
	:hook
	(python-mode . yapf-mode)))

(provide 'vwe-python)
;;; vwe-python.el ends here
