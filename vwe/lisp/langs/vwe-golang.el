;;; vwe-golang.el --- Golang Programming    -*- lexical-binding: t; -*-

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
;; go install golang.org/x/tools/cmd/goimports@latest
;; go install golang.org/x/tools/cmd/godoc@latest
;; go install golang.org/x/tools/cmd/guru@latest
;; go install golang.org/x/tools/cmd/gorename@latest
;; go install golang.org/x/lint/golint@latest
;; go install github.com/rogpeppe/godef@latest
;; go install github.com/josharian/impl@latest
;; go install github.com/davidrjenni/reftools/cmd/fillstruct@latest
;; go install github.com/cweill/gotests/...@latest
;; go install github.com/kisielk/errcheck@latest
;; go install github.com/mdempsky/unconvert@latest
;; go install golang.org/x/tools/gopls@latest  ;; go lsp

;;; Code:
;; ***************************************************************************
;; lib
;; ***************************************************************************

;; ***************************************************************************
;; config
;; ***************************************************************************
;;
;; `go-mode'
;;
(vwe@lib--pkg go-mode
  :init ((push '("\\.go\\'" . go-mode) auto-mode-alist)
		 (add-hook 'go-mode-hook (lambda () (add-hook 'before-save-hook #'gofmt-before-save nil t)))
		 ;; (add-hook 'go-mode-hook (lambda () (vwe@lib--server-lsp vwe@custom--lsp
		 ;; 														:lsp (progn
		 ;; 															   (add-hook 'before-save-hook #'lsp-format-buffer nil t)
		 ;; 															   (add-hook 'before-save-hook #'lsp-organize-imports nil t)))))
		 )
  :config ((vwe@lib--keymap-set go-mode-map
								'(("M-." godef-jump)))
		   (with-eval-after-load 'exec-path-from-shell
			 (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY")))

		   ;;
		   ;; `go-guru' go代码编辑扩展
		   ;;
		   (vwe@lib--pkg go-guru
			 :init ((add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)))

		   ;;
		   ;; `go-tag'
		   ;; go get github.com/fatih/gomodifytags
		   ;;
		   (vwe@lib--pkg go-tag
			 :variable ((setq go-tag-args (list "-transform" "lispcase"))))
		   )
  :variable ((setq go-command (concat (getenv "GOROOT") "/bin/go")
				   gofmt-command (concat (getenv "GOPATH") "/bin/goimports")
				   flycheck-go-gofmt-executable (concat (getenv "GOPATH") "/bin/goimports")
				   flycheck-go-golint-executable (concat (getenv "GOPATH") "/bin/golint")
				   flycheck-go-build-executable (concat (getenv "GOROOT") "/bin/go")
				   flycheck-go-vet-executable (concat (getenv "GOROOT") "/bin/go")
				   flycheck-go-test-executable (concat (getenv "GOROOT") "/bin/go"))))

(provide 'vwe-golang)
;;; vwe-golang.el ends here
