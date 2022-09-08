;;; vwe-rust.el --- Rust             -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Wu Junyu

;; Author: Wu Junyu <vistar_w@hotmail.com>
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
;; cargo install cargo-edit
;; git clone https://github.com/rust-analyzer/rust-analyzer.git
;; && cd rust-analyzer && cargo xtask install --server ;;  rust-analyzer
;; rustup component add rust-src ;; rls


;;; Code:

;;
;; `rustic'
;;
(vwe@lib--pkg rustic
  :init ((add-hook 'rustic-mode-hook
				   ((lambda ()
					  (vwe@lib--server-lsp
					   vwe@custom--lsp
					   :lsp (progn
							  (setq rustic-format-on-save nil
									rustic-lsp-formatto t)))))
				   nil t))
  :variable ((setq rustic-format-on-save t)))

(provide 'vwe-rust)
;;; vwe-rust.el ends here
