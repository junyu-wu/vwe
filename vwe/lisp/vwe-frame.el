;;; vwe-frame.el --- Frame and window Config       -*- lexical-binding: t; -*-

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
;; lib
;; ***************************************************************************
(defvar vwe@pkg--desktop-before-restore-hook
  nil
  "Before restore hook.")

(defun vwe@pkg--desktop-auto-save ()
  "Desktop auto save.
add `auto-save-hook' hook."
  (interactive)
  (if (and vwe@custom--frame-save-and-recover-layout?
		   (eq (desktop-owner) (emacs-pid)))
      (desktop-save desktop-dirname)))

(defun vwe@pkg--desktop-session-saved-p ()
  "Session saved."
  (file-exists-p (format "%s%s"
						 (or desktop-dirname user-emacs-directory)
						 desktop-base-file-name)))

(defun vwe@pkg--desktop-session-restore ()
  "Restore a saved Emacs session."
  (interactive)
  (if (vwe@pkg--desktop-session-saved-p)
	  (progn
		(desktop-read))
    (message "No desktop found.")))

(defun vwe@pkg--desktop-session-save ()
  "Save an Emacs session."
  (interactive)
  (when vwe@custom--frame-save-and-recover-layout?
	(if (vwe@pkg--desktop-session-saved-p)
		(if (y-or-n-p "Overwrite existing desktop? ")
			(desktop-save-in-desktop-dir))
	  (if (y-or-n-p "Save deskktop? ")
		  (desktop-save-in-desktop-dir))
	  )
	;; (cond
	;;  ((vwe@pkg--desktop-session-saved-p) (if (y-or-n-p "Overwrite existing desktop? ")
	;; 										 (desktop-save-in-desktop-dir)))
	;;  (t (if (y-or-n-p "Save deskktop? ")
	;; 		(desktop-save-in-desktop-dir))))
	))

(defun vwe@pkg--desktop-session-load ()
  "Load session."
  (if (and (vwe@pkg--desktop-session-saved-p)
		   vwe@custom--frame-save-and-recover-layout?)
	  (if (y-or-n-p "Restore desktop? ")
		  (vwe@pkg--desktop-session-restore))))

(defun vwe@pkg--desktop-remove-session ()
  "Remove session."
  (let* ((dir desktop-dirname))
	(desktop-remove)
	(setq desktop-dirname dir)))

(defun vwe@pkg--desktop-owner-advice (original &rest args)
  "Desktop owner advice.
apply ORIGINAL and ARGS."
  (let ((owner (apply original args)))
    (if (and (and owner (/= owner (emacs-pid)))
			 (and (car (member owner (list-system-processes)))
				  (let (cmd (attrlist (process-attributes owner)))
					(when cmd
					  (if (not attrlist) owner
						(dolist (attr attrlist)
						  (and (string= "comm" (car attr))
							   (setq cmd (car attr))))
						(and cmd (string-match-p "[Ee]macs" cmd) owner))))))
		owner)))

;; ***************************************************************************
;; config
;; ***************************************************************************
;;
;; `desktop'
;;
(vwe@lib--package 'desktop
				  (progn
					(when vwe@custom--frame-save-and-recover-layout?
					  (add-hook 'after-init-hook #'desktop-save-mode)
					  (add-hook 'after-init-hook #'vwe@pkg--desktop-session-load)
					  (add-hook 'desktop-after-read-hook #'vwe@pkg--desktop-remove-session)
					  (add-hook 'kill-emacs-hook #'vwe@pkg--desktop-session-save))
					;; (add-hook 'auto-save-hook #'vwe@pkg--desktop-auto-save)
					)
				  (progn
					(when vwe@custom--frame-save-and-recover-layout?
					  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
					  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
					  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
					  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
					  (advice-add #'desktop-owner :around #'vwe@pkg--desktop-owner-advice)))
				  (setq desktop-path (append (list (vwe@lib--path-cache "desktop/"))
											 desktop-path)
						desktop-dirname (vwe@lib--path-cache "desktop/")
						desktop-base-file-name ".vwe.desktop"
						desktop-buffers-not-to-save (concat "\\("
															"^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
															"\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
															"\\)$")
						desktop-locals-to-save (append '(vwe-editor-mode vwe-editor-view-mode)
													   desktop-locals-to-save))
				  t nil t)

;;
;; `popwin'
;;
(vwe@lib--package 'popwin
				  (add-hook 'after-init-hook #'popwin-mode))

;;
;; `winum'
;;
(vwe@lib--package 'winum
				  (add-hook 'after-init-hook #'winum-mode)
				  (vwe@lib--keymap-global-set '(("M-~" winum-select-window-by-number)
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

(provide 'vwe-frame)
;;; vwe-frame.el ends here
