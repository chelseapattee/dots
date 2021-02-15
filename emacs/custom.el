;;; custom --- Summary
;;; Commentary:
;;; Code:

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-overstrike ((t (:inherit bold :foreground "blue"))))
 '(Man-reverse ((t (:inherit highlight :foreground "cyan"))))
 '(Man-underline ((t (:inherit underline :foreground "magenta"))))
 '(button ((t (:inherit link :foreground "yellow"))))
 '(company-scrollbar-bg ((t (:background "black" :foreground "white"))))
 '(company-scrollbar-fg ((t (:background "blue" :foreground "white"))))
 '(company-tooltip ((t (:background "black" :foreground "white"))))
 '(company-tooltip-annotation ((t (:background "black" :foreground "blue"))))
 '(company-tooltip-annotation-selection ((t (:background "blue" :foreground "white"))))
 '(company-tooltip-common ((t (:background "black" :foreground "brwhite"))))
 '(company-tooltip-common-selection ((t (:background "blue" :foreground "white"))))
 '(company-tooltip-selection ((t (:background "blue" :foreground "white"))))
 '(compilation-error ((t (:foreground "red"))))
 '(compilation-info ((t (:foreground "blue"))))
 '(error ((t (:foreground "red" :inverse-video nil))))
 '(flycheck-error ((t (:inherit error))))
 '(flycheck-warning ((t (:inherit warning))))
 '(flymake-warning ((t (:background "black" :foreground "blue"))))
 '(font-lock-builtin-face ((t (:foreground "yellow"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "blue"))))
 '(font-lock-comment-face ((t (:foreground "blue"))))
 '(font-lock-doc-face ((t (:foreground "cyan" :slant normal))))
 '(font-lock-keyword-face ((t (:foreground "yellow"))))
 '(font-lock-warning-face ((t (:foreground "blue"))))
 '(highlight ((t (:background "blue" :foreground "white"))))
 '(hl-line ((t (:background "black"))))
 '(isearch ((t (:background "white" :foreground "blue"))))
 '(lazy-highlight ((t (:background "white" :foreground "blue"))))
 '(match ((t (:foreground "yellow" :inverse-video nil))))
 '(mode-line ((t (:background "black" :foreground "white" :inverse-video nil :weight normal))))
 '(mode-line-buffer-id ((t (:weight normal))))
 '(mode-line-inactive ((t (:background "black" :foreground "black" :inverse-video nil :weight normal))))
 '(region ((t (:background "white" :foreground "black"))))
 '(show-paren-match ((t (:background "blue" :foreground "white"))))
 '(underline ((t nil)))
 '(vertical-border ((t (:background "black" :foreground "black"))))
 '(warning ((t (:foreground "magenta")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-header-file-path
   (quote
    ("~/usr/include" "/usr/include" "/usr/local/include")))
 '(c-basic-offset 2)
 '(clang-format-executable "/usr/bin/clang-format-11")
 '(company-backends
   (quote
    (company-ghci company-anaconda company-semantic company-capf company-files
                  (company-dabbrev-code)
                  company-dabbrev)))
 '(company-idle-delay nil)
 '(company-tooltip-minimum-width 80)
 '(compile-command
   "cd ~/src/celeris/build && cmake .. -DCMAKE_BUILD_TYPE=RelWithDebInfo && make -j && make test")
 '(flycheck-check-syntax-automatically (quote (save)))
 '(flycheck-display-errors-delay 0.5)
 '(graphviz-dot-indent-width 2)
 '(haskell-process-log t)
 '(haskell-process-path-ghci "~/local/ghci")
 '(help-at-pt-timer-delay 1.0)
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (eglot package-build shut-up epl git commander f dash s)))
 '(pop-up-windows nil)
 '(recenter-redisplay nil)
 '(show-paren-mode t)
 '(x86-lookup-pdf "~/.emacs.d/lib/x86-lookup-src.pdf"))

(provide 'custom)
;;; custom.el ends here
