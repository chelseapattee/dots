;;; init --- Summary
;;; Commentary:
;;; Code:

;; cask

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(cond
 ((require 'cask "~/.cask/cask.el" t) t)
 ((require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el" t) t)
 (t (error
     "Failed to find cask.el in any of the standard locations.  Is Cask installed?")))

(cask-initialize)

;; non-cask libraries

(mapc 'load (directory-files "~/.emacs.d/lib/" t "\.el$"))

;; custom

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; configuration

; no pipes in vertical border
(set-display-table-slot
 standard-display-table
 'vertical-border
 (make-glyph-code 32))

(add-to-list 'custom-theme-load-path "~/.emacs.d/lib/color-theme-solarized")
(load-theme 'solarized t)

(set-face-italic 'font-lock-comment-face nil)
(set-face-italic 'font-lock-comment-delimiter-face nil)

(setq inhibit-startup-message t
      transient-mark-mode t
      require-final-newline t
      make-backup-files nil
      auto-save-default nil
      ring-bell-function 'ignore
      scroll-step 1
      scroll-conservatively 10000
      indent-tabs-mode nil
      tab-width 2
      sh-basic-offset 2
      sh-indentation 2
      c-basic-indent 2
      python-indent-offset 2

      web-mode-attr-indent-offset 2
      web-mode-markup-indent-offset 2
      web-mode-code-indent-offset 2
      web-mode-css-indent-offset 2

      echo-keystrokes 0.1
      split-height-threshold nil
      split-width-threshold nil
      resize-mini-windows nil
      )

(setq pop-up-windows nil)

(defun my-display-buffer-function (buf not-this-window)
  ;; Note: Some modules sets `pop-up-windows' to t before calling
  ;; `display-buffer' -- Why, oh, why!
  (let ((display-buffer-function nil)
        (pop-up-windows nil))
    (display-buffer buf not-this-window)))

(setq display-buffer-function 'my-display-buffer-function)

(menu-bar-mode -1)
(global-hl-line-mode 1)
(blink-cursor-mode t)
(show-paren-mode)

(defalias 'yes-or-no-p 'y-or-n-p)

;; hooks

(with-eval-after-load 'company
  ;; (add-to-list 'company-backends 'company-anaconda)
  (add-to-list 'company-backends 'company-jedi)
  ;; (add-to-list 'company-backends 'company-irony)
  ;;(add-to-list 'company-backends 'company-ghci)
  )

(add-hook 'prog-mode-hook 'paredit-everywhere-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
;; (add-hook 'python-mode-hook 'autopair-mode)
(add-hook 'org-mode-hook 'org-indent-mode)

(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(add-hook 'after-init-hook 'global-company-mode)
;(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'after-init-hook 'smart-split)

(defun indent-or-expand-c++ (&optional args)
  "haven't decided yet"
  (interactive "*P")
  (let ((can-complete (and
                       (or (bobp) (= ?w (char-syntax (char-before))))
                       (or (eobp) (not (= ?w (char-syntax (char-after))))))))
    (cond
     (can-complete (company-complete))
     ((use-region-p) (call-interactively 'clang-format-region))
     (t (indent-according-to-mode)))))

(add-hook 'haskell-mode-hook 'company-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-interactive-mode-hook 'company-mode)

(defconst fb-java-style
  '((c-basic-offset . 2)
    (c-offsets-alist . (
                        (arglist-intro . ++)
                        (brace-list-intro . ++)
                        (case-label . +)
                        (statement-cont . ++)
                        (arglist-close . c-lineup-close-paren))))
  "Facebook's Java Programming style")
(c-add-style "fb-java-style" fb-java-style)

(defun my-java-mode-hook ()
  (c-set-style "fb-java-style"))

(add-hook 'java-mode-hook 'my-java-mode-hook)

(require 'compile)
(setq compilation-error-regexp-alist
      (append (list
               '("^.*ERROR in \\(.+\\) .*at line \\([0-9]+\\)" 1 2))
                compilation-error-regexp-alist))

;; file modes
;; (add-to-list 'auto-mode-alist '("\\.react\\.js\\'" . js2-mode))

;; utils

(defun what-face (pos)
  "Display the face at POS."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
		  (get-char-property (point) 'face))))
        (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun show-current-file ()
  "Print the current buffer filename to the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(defun kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

;; keys

(bind-key "C-c q" 'join-line)
(bind-key "C-c r" 'revert-buffer)
(bind-key "M-N" 'windmove-right)
(bind-key "M-P" 'windmove-left)
(bind-key "C-c f" 'show-current-file)
(bind-key "C-j" 'newline)
(bind-key "C-x k" 'kill-current-buffer)

(let ((tags-file (concat default-directory "TAGS")))
  (if (file-exists-p tags-file)
      (progn
        (mural-add-tagfile tags-file)
        (bind-key "M-." 'mural-goto-ido))))

(bind-key* "C-M-i" 'company-try-hard)
(bind-key* "C-x C-f" 'lusty-file-explorer)
(bind-key* "C-x b" 'ido-switch-buffer)
(bind-key* "M-x" 'smex)
(bind-key* "M-/" 'undo)

(bind-key "M-." 'find-function emacs-lisp-mode-map)
;; (bind-key "TAB" 'indent-sexp emacs-lisp-mode-map)
(bind-key "RET" 'eval-print-last-sexp lisp-interaction-mode-map)

(with-eval-after-load 'cc-mode
  (bind-key "TAB" 'indent-or-expand-c++ c-mode-map)
  (bind-key "TAB" 'indent-or-expand-c++ c++-mode-map)
  (bind-key "M-z" 'clang-format-buffer))

(with-eval-after-load 'haskell-mode
  (bind-key "M-." 'haskell-mode-jump-to-def haskell-mode-map))

;; for some reason `with-eval-after-load` doesn't seem to work
;; with these modes
(add-hook 'js-mode-hook
          (lambda ()
            (progn
              (add-hook 'after-save-hook 'prettier-eslint nil t)
              (bind-key "M-z" 'prettier-js js-mode-map)
              (bind-key "M-." 'xref-find-definitions js-mode-map))))

(add-hook 'web-mode-hook
          (lambda ()
            (progn
              (add-hook 'after-save-hook 'prettier-eslint nil t)
              (bind-key "M-z" 'prettier-js js-mode-map)
              (bind-key "M-." 'xref-find-definitions js-mode-map))))

(add-hook 'python-mode-hook
          (lambda ()
            (progn
              (bind-key "M-z" 'py-yapf-buffer python-mode-map)
              (bind-key "M-." 'jedi:goto-definition)
              (setq python-indent-offset 2))))

(ido-mode)

(add-to-list 'compilation-search-path "~/src/aleph")
(add-to-list 'compilation-search-path "~/src/aleph/py")
(add-to-list 'compilation-search-path "~/src/aleph/proto")

(add-to-list 'compilation-search-path "~/src/multi-ordermatch-engine")
(add-to-list 'compilation-search-path "~/src/multi-ordermatch-engine/v2")
(add-to-list 'compilation-search-path "~/src/multi-ordermatch-engine/thrift")
(add-to-list 'compilation-search-path "~/src/multi-ordermatch-engine/v2/py")

(add-to-list 'compilation-search-path "~/src/celeris")
(add-to-list 'compilation-search-path "~/src/celeris/py")

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))

;; TODO(benr): sort this shit into appropriate sections

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(require 'whitespace)
(setq whitespace-line-column 100) ;; limit line length
(setq whitespace-style '(face lines-tail))

(add-hook 'prog-mode-hook 'whitespace-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package eglot :ensure t)

(add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
(setq eglot-ignored-server-capabilites '(:documentHighlightProvider))
(add-to-list 'eglot-server-programs '(c++-mode . ("clangd-11" "--completion-style=detailed")))
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'web-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs '(js-mode . ("typescript-language-server" "--stdio")))

(provide 'init)
;;; init ends here
