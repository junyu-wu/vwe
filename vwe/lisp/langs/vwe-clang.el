;;; vwe-clang.el --- C / C++              -*- lexical-binding: t; -*-

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

;; apt install llvm libclang-dev clang clang-format clang-tools clangd libcppunit-dev cppcheck gdb or lldb
;; install ctags

;;; Code:
;; ***************************************************************************
;; lib
;; ***************************************************************************

;; ***************************************************************************
;; config
;; ***************************************************************************

(vwe@lib--pkg nil
  :init ((with-eval-after-load 'company
		   (add-hook 'c-mode-hook
					 (lambda ()
					   (vwe@pkg--company-make-mode-local-backends
						'(company-semantic company-cmake company-clang))))
		   (add-hook 'c++-mode-hook
					 (lambda ()
					   (vwe@pkg--company-make-mode-local-backends
						'(company-semantic company-cmake company-clang)))))))

;;
;; `modern-c++-font-lock'
;;
(vwe@lib--pkg modern-cpp-font-lock
  :init ((add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))
  :config ((modern-c++-font-lock-mode t)))

;;
;; `disaster' 查看当前的汇编代码
;;
(vwe@lib--pkg disaster)

(provide 'vwe-clang)
;;; vwe-clang.el ends here
