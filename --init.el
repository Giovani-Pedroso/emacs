;;remove o initial buffer
(setq inhibit-tar.up-message t)

;;Hide the tool bar
(tool-bar-mode   -1)

;;Hide the menu bar
(menu-bar-mode   -1)                 

;;Oculta dicase
(tooltip-mode    -1)                 

;;Disable the scroll bar
(scroll-bar-mode -1)                

;;Show the column number in the modeline
(column-number-mode t)              

;;Self closing ()  [] ""
(electric-pair-mode 1)

;;Enable the highlight line
(global-hl-line-mode t)

(kill-buffer)                            

;;Set the lines on in the relative mode
(setq display-line-numbers-type 'relative) 
(global-display-line-numbers-mode)

;;Disable the suspend frame
(global-unset-key (kbd "C-z"))

(delete-selection-mode t) 

;;disable num lines for the modes:
(dolist (mode '(org-mode-hook
                vterm-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;Smoth scroll
(setq mouse-wheel-scroll-amount '(8 ((shift) . 1)) 
      mouse-wheel-progressive-speed nil            
      mouse-wheel-follow-mouse 't                  
      scroll-step 1)                               

;;Broke line
(global-visual-line-mode t)

;;Space in the boards
(set-fringe-mode 0)

;;Cursor type
(setq-default cursor-type 'box)

(setq backup-directory-alist `(("." . "~/.saves")))

;;Verify and initialize the package.el
(require 'package)
;;Define the repos
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))

;;Init the package system
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;;Installs the use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "8d7b028e7b7843ae00498f68fad28f3c6258eda0650fe7e17bfb017d51d0e2a2" "a82ab9f1308b4e10684815b08c9cac6b07d5ccb12491f44a942d845b406b0296" "266ecb1511fa3513ed7992e6cd461756a895dcc5fef2d378f165fed1c894a78c" "76ed126dd3c3b653601ec8447f28d8e71a59be07d010cd96c55794c3008df4d7" "d47f868fd34613bd1fc11721fe055f26fd163426a299d45ce69bef1f109e1e71" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" default))
 '(package-selected-packages
   '(visual-fill-column org-bullets counsel-projectile forge ivy-rich which-key rainbow-delimiters ivy ivyy flycheck helm undo-fu undo-tree evil centaur-tabs dashboard doom-themes doom-modeline all-the-icons neotree auto-complete auto-package-update use-package))
 '(warning-suppress-log-types '((comp) (comp) (comp)))
 '(warning-suppress-types '((comp) (comp))))

(use-package evil
  :ensure t
  :config
  (evil-mode)
  (evil-set-undo-system 'undo-redo))

(use-package evil-nerd-commenter)
(global-set-key (kbd "C-c ;") 'evilnc-comment-or-uncomment-lines)

(use-package undo-fu)
(use-package undo-tree
      :config
      (undo-tree-mode)
  )

;;Create a

(use-package rainbow-mode
  :ensure t
  :config
  (rainbow-mode)
  )

;;Will initialize the rainbow mode when
;;the web, scss or css are active
(add-hook 'web-mode-hook 'rainbow-mode)
(add-hook 'scss-mode-hook 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)))

(use-package neotree
  :ensure t
  :config
  (progn
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))
  :bind (("C-\\" . 'neotree-toggle)))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;Description for commands
;not working - Emacs for scratch 2 ~= 28:00 min
;(use-package ivy-rich
;  :after ivy
;  :init
;  (ivy-rich-mode 1))

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)
  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)
  ;; Show more candidates
  ;; (setq vertico-count 20)
  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Optionally use the `orderless' completion style. See
;; `+orderless-dispatch' in the Consult wiki for an advanced Orderless style
;; dispatcher. Additionally enable `partial-completion' for file path
;; expansion. `partial-completion' is important for wildcard support.
;; Multiple files can be opened at once with `find-file' if you enter a
;; wildcard. You may also give the `initials' completion style a try.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        read-buffer-completion-ignore-case t
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))
;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
  :after magit)

;;Add evil keys to magit
;;for some reaseon this not work
;;(use-package evil-collection
;;:ensure t
;;:after evil
;;:init
;;(evil-collection-init))

(use-package helpful
  :ensure t)

;;Change the default description for the helpful description
(global-set-key (kbd "C-h f") #'helpful-callable)

(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)


;; Lookup the current symbol at point. C-c C-d is a common keybinding
;; for this in lisp modes.
(global-set-key (kbd "C-c C-d") #'helpful-at-point)

;; Look up *F*unctions (excludes macros).
;;
;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
;; already links to the manual, if a function is referenced there.
(global-set-key (kbd "C-h F") #'helpful-function)

;; Look up *C*ommands.
;;
;; By default, C-h C is bound to describe `describe-coding-system'. I
;; don't find this very useful, but it's frequently useful to only
;; look at interactive functions.
(global-set-key (kbd "C-h C") #'helpful-command)

;;Get the a random image to show on the dashboard
 (defun get-random-image()
   ;;Set the  directory of the images
   (setq-local directory-images "~/Pictures/Emacs-dashboard/to-show/")
   ;;Put in a list all images in the directory
   (setq-local images (directory-files directory-images nil ".png"))

   ;;Join the folder's path with the image path
   ;;and return the full path
   (concat directory-images
           ;;get a random image
           (nth (- (random (length images)) 1) images)
           ))

 (use-package dashboard
   :ensure t
   :init
 (progn;;This execult commands in the initialization process
   (setq dashboard-banner-logo-title "Quem desiste não cansa")
   (setq dashboard-set-init-info nil)
(setq dashboard-startup-banner (get-random-image))
   (setq dashboard-set-heading-icons t)

 ;; Content is not centered by default. To center, set
 ;;This variable to t
 ;;(setq dashboard-center-content t)
   (setq dashboard-set-file-icons t)
 ;;(setq dashboard-footer-messages '("Better than VSCoiso"))
   (setq dashboard-items '(
                         ;;(agenda . 4)
                           ;;(recents  . 6)
                           (bookmarks . 6)
                           (projects . 4)
                         ))
   )
   :config
   (dashboard-setup-startup-hook))

(use-package lorem-ipsum
  :ensure t
  )

(use-package all-the-icons
  :ensure t)

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-tomorrow-night  t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package page-break-lines)
(global-page-break-lines-mode)

;;(set-frame-font "Monofur 11" nil t)
;;(set-default-font “Terminus-9”)

;;  (defun efs/lsp-mode-setup ()
;;    (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
;;    (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :ensure t
  :commands
  ;;Activate the package when this functions are called
  (lsp lsp-deferred)
  :hook
  (lsp-mode . efs/lsp-mode-setup)
  ;;Init lsp for the modes:
  (c-mode . lsp-deferred)
  (c++-mode . lsp-deferred)
  (python-mode . lsp-deferred)
  (rust-mode . lsp-deferred)
  (web-mode . lsp-deferred)
  ;;(typescript-mode . lsp-deferred)
  (css-mode . lsp-deferred)
  (scss-mode . lsp-deferred)
  (svelte-mode . lsp-deferred)
  (dockerfile-mode . lsp-deferred)
  (yaml-mode . lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (setq lsp-log-io nil);;don't log everthing = speed
  ;;Sometimes the lsp stop for no reason
  ;;this will restart it
  (setq lsp-restart 'auto-restart)
  ;;Give the presscription of
  ;;the keys pressed using the
  ;;which-key packge
  (lsp-enable-which-key-integration t))

;;Avoid the lsp breaks the emacs
(setq gc-cons-threshold 10000000)
(setq read-process-output-max (* 1024 1024))

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-code-actions t)
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-treemacs
  :after lsp)

(use-package dap-mode)

(use-package company
  :after
  ;;Will run afther the lsp-mode
  lsp-mode
  :ensure t
  :hook
  (lsp-mode . company-mode)
  :init
  (add-hook 'after-init-hook 'global-company-mode)

  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))

  :config
  (global-company-mode t)
  (setq company-idle-delay 0.0
        company-minimum-prefix-lengh 1)
  )

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package paredit
  :ensure t)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))
;;Automatic display images in the orgmode
(setq org-startup-with-inline-images t)
(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"))


;;allow the org files display images
;;(org-toggle-inline-images)

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))
  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("cpp" . "src cpp"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"));
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("js" . "src js"))
  ;;C doenst work yet
  ;;(add-to-list 'org-structure-template-alist '("c" . "src C"))
  )
;;remove the massage ask you to exec the command
(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp .t)
   (python .t)
   (C .t)
   (css .t)
   (js .t)
   (lua .t)
   ))

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols));
  (lsp-headerline-breadcrumb-mode))


;;(defun efs/org-babel-tangle-config ()
;;  (when (string-equal (file-name-directory (buffer-file-name))
;;                      (expand-file-name user-emacs-directory))
;; Dynamic scoping to the rescue
;;    (let ((org-confirm-babel-evaluate nil))
;;      (org-babel-tangle))))

(setq org-agenda-files
      '("~/codes/Org/Agenda.org"))
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

(setq org-startup-folded t)
(setq org-startup-indented t)
(setq org-startup-with-inline-images t)
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
  )

(use-package org-superstar
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

;;   Set faces for heading levels
(dolist (face '((org-level-1 . 1.4)
                (org-level-2 . 1.3)
                (org-level-3 . 1.2)
                (org-level-4 . 1.1)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Monospace" :weight 'regular :height (cdr face)))

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
(set-face-attribute 'line-number nil :inherit 'fixed-pitch)
(set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)

(require 'ox-md)

(use-package python-mode
  :ensure t
  ;;:hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(use-package rjsx-mode
  :ensure t
  :mode "\\.js\\'")

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)


                                        ;typescripte config
                                        ;
                                        ;(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

;; enable typescript - tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(use-package tide
  :ensure t
  :after (rjsx-mode company flycheck)
  :hook (rjsx-mode . setup-tide-mode))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook
  ;;Start the lsp when the enter in the type script mode
  (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2)
  ;;Installs the dap for node applications
  (require 'dap-node)
  (dap-node-setup))

(setq-default tab-width 2)
(setq indent-tabs-mode nil)
(defun luke/webmode-hook ()
  "Webmode configurations."
  (setq web-mode-enable-comment-annotation t)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-attr-indent-offset 0)
  (setq web-mode-enable-auto-indentation t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  )
(use-package web-mode
  :ensure t
  :mode (;;("\\.js\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.js\\'" . web-mode)
         ;;("\\.ts\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.svelte\\'" . web-mode)
         ;;Was confiliting with lsp
         ;;("\\.css\\'" . web-mode)
         ("\\.html\\'" . web-mode))
  :commands web-mode
  :hook (web-mode . luke/webmode-hook)
  )

(use-package prettier
  :ensure t
  :hook
  ((mhtml-mode json-mode css-mode ts-mode scss-mode rjsx-mode js2-mode web-mode) . prettier-mode))

(use-package css-mode
  :mode "\\.css\\'")

(use-package scss-mode
  :mode "\\.scss\\'")

;; json-mode
(use-package json-mode
  :ensure t)

(use-package yaml-mode
  :ensure t
  )

(use-package svelte-mode
:ensure t
  )

(use-package dockerfile-mode)

(use-package rust-mode
  :ensure t)

(use-package format-all
  :ensure t
  :hook
  (c-mode . format-all-mode)
  )
(add-hook 'c-mode-hook 'format-all-mode)

;; Instalação do auto-update

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "21:00"))

;;Motion keys
(define-key evil-normal-state-map "n" 'evil-next-visual-line)
(define-key evil-normal-state-map "e" 'evil-previous-visual-line)
(define-key evil-normal-state-map "i" 'evil-forward-char)
(define-key evil-normal-state-map "m" 'evil-backward-char)

(define-key evil-visual-state-map "n" 'evil-next-line)
(define-key evil-visual-state-map "e" 'evil-previous-line)
(define-key evil-visual-state-map "i" 'evil-forward-char)
(define-key evil-visual-state-map "m" 'evil-backward-char)

(define-key evil-visual-state-map "l" 'evil-insert)
(define-key evil-visual-state-map "y" 'evil-yank)
(define-key evil-visual-state-map "o" 'evil-open-below )
(define-key evil-visual-state-map "O" 'evil-open-above )

;;Motion keys
;;Functions keys
(define-key evil-normal-state-map "l" 'evil-insert)
(define-key evil-normal-state-map "y" 'evil-yank)
(define-key evil-normal-state-map "o" 'evil-open-below )
(define-key evil-normal-state-map "O" 'evil-open-above )

(global-set-key (kbd "C-c ;") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "C-c <tab>") 'yas-expand)

;(global-set-key (kbd "C-l") (message "ola"))

(global-set-key (kbd "<f6>") 'flyspell-prog-mode)

(global-set-key (kbd "C-<f6>") 'flyspell-mode)

(global-set-key (kbd "<f7>") 'ispell-comments-and-strings)

(global-set-key (kbd "C-<f7>") 'ispell)

;; Enable ccls for all c++ files, and platformio-mode only
;; when needed (platformio.ini present in project root).
(add-hook 'c++-mode-hook (lambda ()
                           (lsp-deferred)
                           (platformio-conditionally-enable)))

(use-package dotenv-mode
  :ensure t) ; unless installed from a package
(add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode))
