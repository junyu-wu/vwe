;;; vwe-package.el ---                               -*- lexical-binding: t; -*-

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
(defvar vwe-package--update-success-log
  nil
  "Update packages log.")

(defvar vwe-package--update-fail-log
  nil
  "Update packages log.")

(defvar vwe-package--update-log-buffer-name
  "*vwe-pkg-update-log*"
  "Update packages log.")

(defvar vwe-package--update-log-keymap
  (let* ((keymap (make-sparse-keymap)))
	(define-key keymap (kbd "q") (lambda () (interactive) (kill-buffer vwe-package--update-log-buffer-name)))
	keymap)
  "Key mapping.")

(defun vwe-package--should-update-pkgs-p (pkg)
  "When PKG due reutrn t."
  (let* (;; (pkg )
		 (pkg-cache (assq pkg package-archive-contents))
		 (pkg-active (assq pkg package-alist))
		 (pkg-builtin (assq pkg package--builtins))
		 (pkg-desc (cadr pkg-cache))
		 (pkg-install (cadr (or pkg-active pkg-builtin))))
	(when (and (package-installed-p pkg) (cadr pkg-cache))
      (version-list-< (package-desc-version pkg-install)
					  (package-desc-version pkg-desc)))))

(defun vwe-package--install (pkg)
  "Install PKG."
  (let* ((pkg-cache (assq pkg package-archive-contents))
		 (pkg-active (assq pkg package-alist))
		 (pkg-builtin (assq pkg package--builtins))
		 (pkg-desc (cadr pkg-cache))
		 (pkg-install (cadr (or pkg-active pkg-builtin)))
		 (pkg-trans (package-compute-transaction (list pkg-desc)
												 (package-desc-reqs pkg-desc)))
		 (pkg-from (package-desc-version pkg-install))
		 (pkg-to (package-desc-version pkg-desc)))
	(condition-case nil
		(progn
		  (package-download-transaction pkg-trans)
          (add-to-list 'vwe-package--update-success-log
					   (concat (propertize (format "%S" (symbol-name pkg))
										   'face 'warning)
							   " current version "
							   (propertize (format "%S" pkg-from)
										   'face 'warning)
							   " update to version "
							   (propertize (format "%S" pkg-to)
										   'face 'warning)
							   (propertize (format " succeeded")
										   'face 'success)
							   ".")))
      ('error (add-to-list 'vwe-package--update-fail-log
						   (concat (propertize (format "%S" (symbol-name pkg))
											   'face 'warning)
								   " current version "
								   (propertize (format "%S" pkg-from)
											   'face 'warning)
								   " update to version "
								   (propertize (format "%S" pkg-to)
											   'face 'warning)
								   (propertize (format " failed")
											   'face 'error)
								   "."))))))

(defun vwe-package--pop-update-log-to-buffer ()
  "Pop update log to buffer."
  (let* ((logs-len (append vwe-package--update-success-log
						   vwe-package--update-fail-log))
		 (logs (mapconcat #'identity
						  (cons "[PACKAGES UPDATED INFO]:"
								(append vwe-package--update-success-log
										vwe-package--update-fail-log))
						  "\n"))
		 (buffer (or (get-buffer vwe-package--update-log-buffer-name)
					 (get-buffer-create vwe-package--update-log-buffer-name))))
	(with-current-buffer buffer
	  (erase-buffer)
	  (if (= (length logs-len) 0)
		  (setq logs (format "all packages is latest version."))
		(setq logs (concat (concat "update "
								   (propertize (format "%S" (length logs-len))
											   'face 'warning)
								   " finished. "
								   "success "
								   (propertize (format "%S" (length vwe-package--update-success-log))
											   'face 'success)
								   " and fail "
								   (propertize (format "%S" (length vwe-package--update-fail-log))
											   'face 'error)
								   ".\n")
						   logs)))
	  (insert logs)
	  (read-only-mode 1)
	  (set-transient-map vwe-package--update-log-keymap nil nil))
	(switch-to-buffer buffer)))

;;;###autoload
(defun vwe-package--update-packages ()
  "Update installed packages."
  (interactive)
  (setq vwe-package--update-success-log nil
		vwe-package--update-fail-log nil)
  (package-refresh-contents)
  (dolist (pkg package-activated-list)
	(when (vwe-package--should-update-pkgs-p pkg)
	  (vwe-package--install pkg)))
  (vwe-package--pop-update-log-to-buffer))

(provide 'vwe-package)
;;; vwe-package.el ends here
