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

(use-package rust-mode
  :bind
  (:map rust-mode-map
		("TAB" . company-indent-or-complete-common))
  :init
  (setq rust-format-on-save t
		company-tooltip-align-annotations t)
  :hook
  (rust-mode-hook . (lambda ()
					  (setq indent-tabs-mode nil)))
  :config
  (use-package cargo
	:hook
	(rust-mode . cargo-minor-mode)
	:config
	(setq compilation-filter-hook
		  (append compilation-filter-hook '(cargo-process--add-errno-buttons))))

  ;; 补全代码
  (use-package racer
	:hook
	(rust-mode . racer-mode))

  ;; rust-mode 扩展
  (use-package rustic)

  (use-package rust-playground))

(provide 'vwe-rust)
;;; vwe-rust.el ends here
