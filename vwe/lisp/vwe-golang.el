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
;; go get golang.org/x/tools/cmd/goimports
;; go get golang.org/x/tools/cmd/godoc
;; go get golang.org/x/tools/cmd/guru
;; go get golang.org/x/tools/cmd/gorename
;; go get golang.org/x/lint/golint
;; go get github.com/rogpeppe/godef
;; go get github.com/nsf/gocode  ;; gocode set autobuild true
;; go get -u github.com/josharian/impl
;; go get -u github.com/davidrjenni/reftools/cmd/fillstruct
;; go get -u github.com/cweill/gotests/...
;; go get -u github.com/kisielk/errcheck
;; go get github.com/mdempsky/unconvert
;; go get golang.org/x/tools/gopls@latest  ;; go lsp

;;; Code:

(use-package go-mode
  :mode
  (("\\.go\\'" . go-mode))
  :bind
  (:map go-mode-map
		("M-." . godef-jump))
  :hook
  ((before-save . gofmt-before-save))
  :init
  (setq go-command
		(concat (getenv "GOROOT") "/bin/go")
		gofmt-command
		(concat (getenv "GOPATH") "/bin/goimports")
		flycheck-go-gofmt-executable
		(concat (getenv "GOPATH") "/bin/goimports")
		flycheck-go-golint-executable
		(concat (getenv "GOPATH") "/bin/golint")
		flycheck-go-build-executable
		(concat (getenv "GOROOT") "/bin/go")
		flycheck-go-vet-executable
		(concat (getenv "GOROOT") "/bin/go")
		flycheck-go-test-executable
		(concat (getenv "GOROOT") "/bin/go"))
  :config
  (with-eval-after-load 'exec-path-from-shell
	(exec-path-from-shell-copy-envs
	 '("GOPATH" "GO111MODULE" "GOPROXY")))

  (use-package company-go
	:after
	(go-mode)
	:hook
	(go-mode . (lambda()
				 (add-to-list
				  (make-local-variable 'company-backends)
				  '(company-go)))))

  (use-package go-eldoc
	:hook
	(go-mode . go-eldoc-setup))

  ;; go代码编辑扩展
  (use-package go-guru
	:hook
	(go-mode . go-guru-hl-identifier-mode))

   ;; 与GUD交互地调试Go程序
  (use-package go-dlv)
  ;; 自动填充struct默认值
  ;; go get -u github.com/davidrjenni/reftools/cmd/fillstruct
  (use-package go-fill-struct))

(provide 'vwe-golang)
;;; vwe-golang.el ends here
