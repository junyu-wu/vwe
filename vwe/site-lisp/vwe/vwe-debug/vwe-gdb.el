;;; vwe-gdb.el ---  GDB                   -*- lexical-binding: t; -*-

;; Copyright (C) 2021  WuJunyu

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

(defgroup vwe-gdb nil
  "GDB.."
  :group 'vwiss-vwe
  :prefix "vwe-gdb--")

(defconst vwe-gdb--buffers
  '((:nmae "gdb" :buffer nil)
	(:nmae "breakpoints" :buffer nil)
	(:nmae "stack" :buffer nil)
	(:nmae "locals" :buffer nil)
	(:nmae "io" :buffer nil)
	(:nmae "registers" :buffer nil)
	(:nmae "memory" :buffer nil)
	(:nmae "threads" :buffer nil)
	(:nmae "disassembly" :buffer nil)
	(:nmae "code" :buffer nil))
  "GDB mode buffers name.")

(defvar vwe-gdb--keymap
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "GDB mode map.")

(defun vwe-gdb-mode-enable ()
  "Enable mode.")

(defun vwe-gdb-mode-disable ()
  "Disable mode.")

;;;###autoload
(define-minor-mode vwe-gdb-mode
  "GDB mode."
  :group 'vwe-gdb
  :keymap vwe-gdb--keymap
  :global t
  (if vwe-gdb-mode
	  (vwe-gdb-mode-enable)
	(vwe-gdb-mode-disable)))

(provide 'vwe-gdb)
;;; vwe-gdb.el ends here
