;;; vwe-layout.el --- Layout Config       -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Wu Junyu

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

;;; Code:

;; ***************************************************************************
;; config
;; ***************************************************************************

;;
;; `popwin'
;;
(vwe@lib--package 'popwin
				  (add-hook 'after-init-hook #'popwin-mode))

;;
;; `persp-mode'
;;
(vwe@lib--package 'persp-mode
				  nil
				  (setq persp-autokill-buffer-on-remove 'kill-weak
						persp-save-dir (vwe@lib--path-cache "persp-confs/")
						persp-nil-name "default"
						persp-set-last-persp-for-new-frames nil
						persp-kill-foreign-buffer-behaviour 'kill
						persp-auto-resume-time 0
						persp-common-buffer-filter-functions
						(list #'(lambda (b)
								  "Ignore temporary buffers."
								  (or (string-prefix-p " " (buffer-name b))
									  (and (string-prefix-p "*" (buffer-name b))
										   (not (string-equal "*scratch*" (buffer-name b))))
									  (string-prefix-p "magit" (buffer-name b))
									  (string-prefix-p "Pfuture-Callback" (buffer-name b))
									  (eq (buffer-local-value 'major-mode b) 'nov-mode)
									  (eq (buffer-local-value 'major-mode b) 'vterm-mode))))))

;;
;; `winum'
;;
(vwe@lib--package 'winum
				  (add-hook 'after-init-hook #'winum-mode)
				  (vwe@lib--keymap-global-set '(("M-`" winum-select-window-by-number)
												("M-0" winum-select-window-0-or-10)
												("M-1" winum-select-window-1)
												("M-2" winum-select-window-2)
												("M-3" winum-select-window-3)
												("M-4" winum-select-window-4)
												("M-5" winum-select-window-5)
												("M-6" winum-select-window-6)
												("M-7" winum-select-window-7)
												("M-8" winum-select-window-8)
												("M-9" winum-select-window-9)))
				  (setq winum-auto-setup-mode-line nil))

(provide 'vwe-layout)
;;; vwe-layout.el ends here
