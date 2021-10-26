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
;; rustup toolchain add nightly
;; rustup component add rls rust-analysis rust-src
;; rustup component add --toolchain nightly clippy
;; cargo install cargo-edit   ;; libssl-dev

;;; Code:

;;
;; `rust-mode'
;;
(vwe@lib--package 'rust-mode
				  (progn
					(add-hook 'rust-mode-hook 'rustic-mode))
				  (progn
					;;
					;; `rustic-mode'
					;;
					(vwe@lib--package 'rustic
									  (progn
										(push '("\\.rs\\'" . rustic-mode) auto-mode-alist))
									  (progn
										(add-hook 'before-save-hook #'rustic-format-buffer nil t)
										(push 'rustic-clippy flycheck-checkers))
									  (setq rustic-lsp-server nil))))

(provide 'vwe-rust)
;;; vwe-rust.el ends here
