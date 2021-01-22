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
;; rustup component add rust-src
;; cargo fmt
;; cargo +nightly install racer

;;; Code:

;;
;; `rust-mode'
;;
(vwe@lib--package 'rust-mode
				  (progn
					(add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil))))
				  (progn
					(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
					;;
					;; `cargo'
					;;
					(vwe@lib--package 'cargo
					  				  (add-hook 'rust-mode-hook #'cargo-minor-mode)
					  				  (setq compilation-filter-hook (append compilation-filter-hook '(cargo-process--add-errno-buttons))))

					;;
					;; `racer' 补全代码
					;;
					(vwe@lib--package 'racer
					  				  (add-hook 'rust-mode-hook #'racer-mode))

					;;
					;; `rustic' rust-mode 扩展
					;;
					(vwe@lib--package 'rustic)

					;;
					;; `rust-playground'
					;;
					(vwe@lib--package 'rust-playground))
				  (setq rust-format-on-save t
						company-tooltip-align-annotations t))

(provide 'vwe-rust)
;;; vwe-rust.el ends here
