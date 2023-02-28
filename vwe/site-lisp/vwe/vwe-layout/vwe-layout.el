;;; vwe-layout.el ---  Vwe layout              -*- lexical-binding: t; -*-

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

(defgroup vwe-layout nil
  "Window layout."
  :group 'windows)

;;
;; zoom
;;
(defcustom vwe-layout--zoom-size
  #'vwe-layout--window-zoom-size-callback
  "Zoom size."
  :type 'list
  :group 'vwe-layout)

(defcustom vwe-layout--zoom-ignored-major-modes nil
  "List of ignored major modes."
  :type 'symbol
  :group 'vwe-layout)

(defcustom vwe-layout--zoom-ignored-buffer-names nil
  "List of ignored buffer names."
  :type 'string
  :group 'vwe-layout)

(defcustom vwe-layout--zoom-ignored-buffer-name-regexps nil
  "List of ignored buffer name regexps."
  :type 'regexp
  :group 'vwe-layout)

(defcustom vwe-layout--zoom-ignore-predicates nil
  "List of additional predicates that allow to ignore windows."
  :type 'function
  :group 'vwe-layout)

(defvar vwe-layout--last-window
  nil
  "Track of the currently selected window.")

(defvar vwe-layout--zoom-balance-p
  nil
  "Windows `ratio' or `balance'.")

;;;###autoload
(defun vwe-layout--text-scale-increase ()
  "Text scale increase."
  (interactive)
  (text-scale-increase 1))

;;;###autoload
(defun vwe-layout--text-scale-decrease ()
  "Text scale decrease."
  (interactive)
  (text-scale-increase -1))

;;;###autoload
(defun vwe-layout--text-scale-adjust ()
  "Text scale adjust."
  (interactive)
  (text-scale-increase 0))

;;;###autoload
(defun vwe-layout--window-height-enlarge (&optional delta)
  "Window height enlarge DELTA."
  (interactive)
  (unless delta (setq delta 1))
  (condition-case nil (enlarge-window delta)
	(error (set-frame-height (selected-frame) (+ (frame-height (selected-frame)) delta)))))

;;;###autoload
(defun vwe-layout--window-height-shrink (&optional delta)
  "Window height shrink DELTA."
  (interactive)
  (unless delta (setq delta 1))
  (condition-case nil (shrink-window delta)
	(error (set-frame-height (selected-frame) (- (frame-height (selected-frame)) delta)))))

;;;###autoload
(defun vwe-layout--window-width-enlarge (&optional delta)
  "Window width enlarge DELTA."
  (interactive)
  (unless delta (setq delta 1))
  (condition-case nil (enlarge-window delta t)
	(error (set-frame-width (selected-frame) (+ (frame-width (selected-frame)) delta)))))

;;;###autoload
(defun vwe-layout--window-width-shrink (&optional delta)
  "Window width shrink DELTA."
  (interactive)
  (unless delta (setq delta 1))
  (condition-case nil (shrink-window delta t)
	(error (set-frame-width (selected-frame) (- (frame-width (selected-frame)) delta)))))

(defun vwe-layout--window-zoom-size-callback ()
  "Window zoom size callback."
  (cond ((> (frame-pixel-width) 1280) '(0.75 . 0.75))
        (t                            '(0.5 . 0.5))))

(defun vwe-layout--get-windows-info-to-string ()
  "Get window info string in frame."
  (format "%s" (list (default-value 'track-mouse)
                     (mapcar (lambda (window) (list window
                                                    (window-total-width)
                                                    (window-total-height)))
                             (window-list)))))

(defun vwe-layout--zoom-window-ignored-p ()
  "Check whether the selected window will be ignored or not."
  (or
   (frame-root-window-p (selected-window))
   (window-minibuffer-p)
   (member major-mode vwe-layout--zoom-ignored-major-modes)
   (member (buffer-name) vwe-layout--zoom-ignored-buffer-names)
   (catch 'ignored
     (dolist (regex vwe-layout--zoom-ignored-buffer-name-regexps)
       (when (string-match regex (buffer-name))
         (throw 'ignored t))))
   (catch 'ignored
     (dolist (predicate vwe-layout--zoom-ignore-predicates)
       (when (funcall predicate)
         (throw 'ignored t))))))

(defun vwe-layout--windows-zoom (&optional ignored)
  "Redisplay windows zoom.
Argument IGNORED is ignored."
  (let* ((windows-info (vwe-layout--get-windows-info-to-string)))
	(unless (equal (frame-parameter nil 'vwe-layout--zoom-parameter) windows-info)
	  (set-frame-parameter nil 'vwe-layout--zoom-parameter windows-info)
	  (with-selected-window (if (or (equal (selected-window) vwe-layout--last-window)
									(window-minibuffer-p)
									(default-value 'track-mouse))
								vwe-layout--last-window
							  (selected-window))
		(setq vwe-layout--last-window (selected-window))
		(vwe-layout--windows-zoom-redisplay)))))

(defun vwe-layout--windows-zoom-redisplay ()
  "Resize windows zoom."
  (let* ((vwe-layout-zoom-mode nil)
		 (window-configuration-change-hook nil)
		 (window-combination-resize t)
		 (window-resize-pixelwise t))
	(balance-windows)
	(unless vwe-layout--zoom-balance-p
	  (unless (vwe-layout--zoom-window-ignored-p)
		(progn (vwe-layout--windows-zoom-resize) (vwe-layout--windows-zoom-resize t))
		(unless (derived-mode-p 'image-mode)
          (scroll-right (window-hscroll))
		  (when (and truncate-lines
					 (> (current-column) (- (window-body-width) hscroll-margin)))
			(scroll-left (- (current-column) (/ (window-body-width) 2)))))))))

(defun vwe-layout--windows-zoom-resize (&optional horizontal?)
  "Resize windows.
HORIZONTAL horizontal or vertical."
  (let* ((size (if (funcall vwe-layout--zoom-size) (funcall vwe-layout--zoom-size) vwe-layout--zoom-size))
		 (custom-size (if horizontal? (car size) (cdr size)))
		 (frame-size (if horizontal? (frame-width) (frame-height)))
		 (win-size (if (floatp custom-size)
					   ;; ratios
					   (if horizontal? (window-total-width) (window-total-height))
					 ;; absolute
					 (if horizontal? (window-body-width) (window-body-height))))
		 (win-ratio (if (floatp custom-size) (round (* custom-size frame-size)) custom-size))
		 (delta (window-resizable nil (max (- win-ratio win-size) 0) horizontal?)))
	(window-resize nil delta horizontal?)))

;;;###autoload
(defun vwe-layout--zoom-type-toggle ()
  "Toggle zoom type."
  (interactive)
  (unless vwe-layout-zoom-mode (vwe-layout-zoom-mode 1))
  (if vwe-layout--zoom-balance-p
	  (setq vwe-layout--zoom-balance-p nil)
	(setq vwe-layout--zoom-balance-p t))
  (vwe-layout--windows-zoom))

(defun vwe-layout--zoom-enable ()
  "Enable mode."
  (add-function :after pre-redisplay-function #'vwe-layout--windows-zoom)
  ;; disable mouse resizing
  (advice-add #'mouse-drag-mode-line :override #'ignore)
  (advice-add #'mouse-drag-vertical-line :override #'ignore)
  (advice-add #'mouse-drag-header-line :override #'ignore)
  ;; update the layout once loaded
  (dolist (frame (frame-list))
    (with-selected-frame frame
      (vwe-layout--windows-zoom))))

(defun vwe-layout--zoom-disable ()
  "Disable mode."
  (remove-function pre-redisplay-function #'vwe-layout--windows-zoom)
  (advice-remove #'mouse-drag-mode-line #'ignore)
  (advice-remove #'mouse-drag-vertical-line #'ignore)
  (advice-remove #'mouse-drag-header-line #'ignore)
  (dolist (frame (frame-list))
    (balance-windows frame)))

;;;###autoload
(define-minor-mode vwe-layout-zoom-mode
  "Zoom Mode."
  :global t
  (if vwe-layout-zoom-mode
	  (vwe-layout--zoom-enable)
    (vwe-layout--zoom-disable)))

;;
;; mode
;;
(defun vwe-layout--enable ()
  "Enable mode.")

(defun vwe-layout--disable ()
  "Disable mode.")

;;;###autoload
(define-minor-mode vwe-layout-mode
  "Vwe layout mode."
  :global t
  (if vwe-layout-mode
	  (vwe-layout--enable)
    (vwe-layout--disable)))

;;
;; windows config layout
;;
(defvar vwe-layout-config--list
  '()
  "Windows layout list.")

(defvar vwe-layout-config--last-config
  nil
  "Last layout config.")

(defun vwe-layout-config--store-last-config (func &rest args)
  "Store last config and apply FUNC(ARGS).
Notice: Shortcut keys only."
  (let* ((config (current-window-configuration))
		 (buf (buffer-name)))
	(apply func args)
	(when (and cursor-type
			   cursor-in-non-selected-windows
			   (not (equal buf (buffer-name))))
	  (setq vwe-layout-config--last-config config))))

(defun vwe-layout-config--toggle-last-config ()
  "Toggle last CONFIG."
  (interactive)
  (set-window-configuration vwe-layout-config--last-config))

;;;###autoload
(defun vwe-layout-config--store (name)
  "Store NAME layout config."
  (interactive (let* ((name (read-string "store layout: ")))
				 (list name)))
  (unless name
	(setq name (format "%s-%s"
					   (buffer-name)
					   (car (time-convert nil t)))))
  (setq vwe-layout-config--list (append
								 (list (cons name (current-window-configuration)))
								 vwe-layout-config--list)))

(defun vwe-layout-config--toggle-config (name)
  "Toggle NAME layout config."
  (interactive
   (list
	(completing-read "toggle layout: "
					 (mapcar (lambda(item)
							   (car item))
							 vwe-layout-config--list))))
  (when (or vwe-layout-config--list vwe-layout-config--last-config)
	(if (and name vwe-layout-config--list)
		(progn
		  (setq name (cdr (assoc name vwe-layout-config--list)))
		  (set-window-configuration name))
	  (set-window-configuration vwe-layout-config--list))))

;;
;; windows config layout mode
;;
(defun vwe-layout-config--enable ()
  "Enable mode."
  (interactive)
  (advice-add #'vwe-layout-config--toggle-config :around #'vwe-layout-config--store-last-config)
  (advice-add #'vwe-layout-config--toggle-last-config :around #'vwe-layout-config--store-last-config)
  (advice-add #'find-file :around #'vwe-layout-config--store-last-config)
  (advice-add #'switch-to-buffer :around #'vwe-layout-config--store-last-config)
  (advice-add #'delete-window :around #'vwe-layout-config--store-last-config))

(defun vwe-layout-config--disable ()
  "Disable mode."
  (interactive)
  (advice-remove #'vwe-layout-config--toggle-config #'vwe-layout-config--store-last-config)
  (advice-remove #'find-file #'vwe-layout-config--store-last-config)
  (advice-remove #'switch-to-buffer #'vwe-layout-config--store-last-config)
  (advice-remove #'delete-window #'vwe-layout-config--store-last-config))

;;;###autoload
(define-minor-mode vwe-layout-config-mode
  "Vwe layout config mode."
  :global t
  (if vwe-layout-config-mode
	  (vwe-layout-config--enable)
    (vwe-layout-config--disable)))

(provide 'vwe-layout)
;;; vwe-layout.el ends here
