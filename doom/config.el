;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(progn (define-key key-translation-map (kbd ";") (kbd ":"))
      (define-key key-translation-map (kbd ":") (kbd ";")))

(setq doom-font (font-spec :family "Iosevka" :size 13))

(setq doom-theme 'doom-solarized-light)
(setq org-directory "~/org/")
(setq display-line-numbers-type nil)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(use-package hl-line+
  :disabled
  :config
  (hl-line-when-idle-interval 0.3)
  (toggle-hl-line-when-idle t))

;; keybindings
;; -----------
(map! :leader
      :desc "swiper" "/" #'swiper
      :desc "counsel-yank-pop" "p" #'counsel-yank-pop
      :desc "search project" "d" #'+default/search-project)

(map! :desc "save" "s-." #'evil-write-all)
(map! :desc "universal argument" "C-s-u" #'universal-argument)

;; typescript
;; ----------
(with-eval-after-load 'flycheck
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t)))

(defvar hao/prettier-bin)

(defun hao/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                ".hao"))
         (prettier (and root
                      (expand-file-name "node_modules/prettier/bin-prettier.js"
                                        root)))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and prettier (file-executable-p eslint))
      (setq-local hao/prettier-bin prettier))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-eslint-args '("--cache"))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(defun hao/typescript-mode-hook ()
  (hao/use-eslint-from-node-modules)
  (typescript-format-on-save-mode))

(add-hook 'web-mode-hook 'hao/typescript-mode-hook)

(reformatter-define typescript-format
  :program hao/prettier-bin
  :args (list "--stdin-filepath" buffer-file-name)
  :lighter "")

;; counsel and ivy
(setq counsel-rg-base-command "rg --hidden -M 120 --with-filename --no-heading --line-number --color never %s")
(after! ivy
  (setq ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-cd-selected))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
