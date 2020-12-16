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

;; apt install cmake llvm llvm-dev ccls cppcheck
;; apt install clang-6.0 or +
;; apt install clang-tools
;; apt install clangd
;; apt install libcppunit-dev
;; apt install clang clang-format
;; install rtags or ggtags
;; before must install cmake clang and libclang-dev
;; after install irony server

;;; Code:
;; ***************************************************************************
;; config
;; ***************************************************************************
(use-package c++-mode
  :ensure nil
  :config
  (use-package modern-cpp-font-lock
	:hook
	(c++-mode-hook . modern-c++-font-lock-mode)
	:config
	(modern-c++-font-lock-mode t)))

(use-package google-c-style
  :hook
  (c-mode-common-hook . google-set-c-style)
  (c-mode-common-hook . google-make-newline-indent))

(use-package clang-format
  :init
  (setq clang-format-style-option "llvm")
  :config
  (add-hook 'c-mode-hook
			(lambda ()
			  (add-hook 'before-save-hook 'clang-format-buffer)))
  (add-hook 'c++-mode-hook
			(lambda ()
			  (add-hook 'before-save-hook 'clang-format-buffer))))

;; 查看当前的汇编代码
(use-package disaster)

(provide 'vwe-clang)
;;; vwe-clang.el ends here
