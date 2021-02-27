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

;; apt install llvm libclang-dev clang clang-format clang-tools
;; apt install clangd-9 libcppunit-dev cppcheck
;; apt install gdb or lldb
;; install ctags

;;; Code:
;; ***************************************************************************
;; lib
;; ***************************************************************************

;; ***************************************************************************
;; config
;; ***************************************************************************

;;
;; `modern-c++-font-lock'
;;
(vwe@lib--package 'modern-cpp-font-lock
				  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
				  (modern-c++-font-lock-mode t))

;;
;; `disaster' 查看当前的汇编代码
;;
(vwe@lib--package 'disaster)

(provide 'vwe-clang)
;;; vwe-clang.el ends here