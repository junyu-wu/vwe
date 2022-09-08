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

;;; Code:
;; ***************************************************************************
;; lib
;; ***************************************************************************
(defun vwe@python--kill-python-shell ()
  "Kill python shell."
  (interactive)
  (let* ((proc (python-shell-get-process))
		 (buffer)
		 (window))
	(when proc
	  (setq buffer (process-buffer proc))
	  (python-shell-send-string "quit()"))
	(with-current-buffer buffer
	  (setq window (selected-window))
	  (kill-buffer buffer)
	  (delete-window window))))

;; ***************************************************************************
;; config
;; ***************************************************************************

;;
;; `python'
;;
(vwe@lib--pkg python
  :init ((push '("\\.py\\'" . python-mode) auto-mode-alist))
  :config (;;
		   ;; `py-autopep8' 根据autopep8格式化代码
		   ;;
		   (vwe@lib--pkg py-autopep8
			 :init((add-hook 'python-mode-hook #'py-autopep8-enable-on-save)))

		   ;;
		   ;; `anaconda-mode'
		   ;;
		   (vwe@lib--pkg anaconda-mode
			 :init ((add-hook 'python-mode-hook 'anaconda-mode)))

		   ;;
		   ;; `company-anaconda'
		   ;;
		   (vwe@lib--pkg company-anaconda
			 :config ((with-eval-after-load 'company
						(add-hook 'python-mode-hook
								  (lambda ()
									(vwe@pkg--company-make-mode-local-backends
									 'company-anaconda))))))

		   ;;
		   ;; `elpy'
		   ;;
		   (vwe@lib--pkg elpy
			 :init ((add-hook 'elpy-mode-hook (lambda () (elpy-shell-set-local-shell (elpy-project-root)))))
			 :variable ((setq elpy-shell-add-to-shell-history t
							  elpy-rpc-virtualenv-path (vwe@lib--path-cache "elpy/rpc-venv")
							  elpy-rpc-python-command "python3"
							  elpy-get-info-from-shell t)))

		   ;;
		   ;; `conda' anaconda
		   ;;
		   (vwe@lib--pkg conda
			 :config ((conda-env-initialize-interactive-shells))
			 :variable ((setq conda-anaconda-home (getenv "CONDA_HOME")))))
  :variable ((setq indent-tabs-mode nil
				   python-indent-offset 4
				   python-shell-interpreter "python3")))

(provide 'vwe-python)
;;; vwe-python.el ends here
