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

;; Instalação do auto-update
(use-package auto-package-update

  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "21:00"))

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

(use-package general
  :ensure t
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer space-leader
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  (space-leader
   "p" '(:ignore t :wk "Programming"))

  (space-leader
   "b" '(:ignore t :wk "buffer")
   "b b" '(switch-to-buffer :wk "Switch buffer")
   ;;"b i" '(switch-to    (use-package python-mode
    ;;:hook (python-mode . lsp-deferred)
    ;:custom
    ;; NOTE: Set these if Python 3 is called "python3" on your system!
    ;; (python-shell-interpreter "python3")
    ;; (dap-python-executable "python3")
    ;(dap-python-debugger 'debugpy)
    ;:config
    ;(require 'dap-python)
   "b k" '(kill-this-buffer :wk "Kill this buffer")
   "b n" '(next-buffer :wk "Next buffer")
   "b p" '(previous-buffer :wk "Previous buffer")
   "b r" '(revert-buffer :wk "Reload buffer"))


  (space-leader
   "e" '(:ignore t :wk "Evaluate")    
   "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
   "e d" '(eval-defun :wk "Evaluate defun containing or after point")
   "e e" '(eval-expression :wk "Evaluate and elisp expression")
   "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
   "e r" '(eval-region :wk "Evaluate elisp in region")) 

  (space-leader
   "f" '(:ignore t :wk "File")    
   "f b" '(switch-to-buffer :wk "Find buffer")
   "f f" '(find-file :wk "Find finle")
   "f s" '(:ignore t :wk "Sudo with files")    
   "f s f" '(sudo-edit-find-file :wk "Sudo find file")
   "f s e" '(sudo-edit :wk "Sudo edit file"))

  (space-leader
   "x" '(execute-extended-command :wk "M-x sortcut"))


  (space-leader
   "h" '(:ignore t :wk "Help")
   "h f" '(describe-function :wk "Describe function")
   "h v" '(describe-variable :wk "Describe variable")
   "h r r" '((lambda () (interactive) (load-file "~/.config/emacs/init.el")) :wk "Reload emacs config"))
  ;;"h r r" '(reload-init-file :wk "Reload emacs config"))

  (space-leader
   "t" '(:ignore t :wk "Toggle")
   "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
   "t t" '(visual-line-mode :wk "Toggle truncated lines"))     


  (space-leader
   "w" '(:ignore t :wk "Windows")
   ;; Window splits
   "w c" '(evil-window-delete :wk "Close window")
                                        ;"w n" '(evil-window-new :wk "New window")
   "w s" '(evil-window-split :wk "Horizontal split window")
   "w v" '(evil-window-vsplit :wk "Vertical split window")
   ;; Window motions
   "w m" '(evil-window-left :wk "Window left")
   "w n" '(evil-window-down :wk "Window down")
   "w e" '(evil-window-up :wk "Window up")
   "w i" '(evil-window-right :wk "Window right")
   "w w" '(evil-window-next :wk "Goto next window")
   ;; Move Windows
   "w M" '(buf-move-left :wk "Buffer move left")
   "w N" '(buf-move-down :wk "Buffer move down")
   "w E" '(buf-move-up :wk "Buffer move up")
   "w I" '(buf-move-right :wk "Buffer move right"))
  )

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(use-package doom-themes
  :config
  (load-theme 'doom-tomorrow-night  t))

(use-package doom-modeline
:ensure t
  :init (doom-modeline-mode 1))

(use-package nerd-icons
  :ensure t
  :custom

  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package page-break-lines
:ensure t
  :config
  (global-page-break-lines-mode)
  )

(require 'windmove)

;;;###autoload
(defun buf-move-up ()
  "Swap the current buffer and the buffer above the split.
If there is no split, ie now window above the current one, an
error is signaled."
  ;;  "Switches between the current buffer, and the buffer above the
  ;;  split, if possible."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'up))
         (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No window above this one")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun buf-move-down ()
  "Swap the current buffer and the buffer under the split.
If there is no split, ie now window under the current one, an
error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'down))
         (buf-this-buf (window-buffer (selected-window))))
    (if (or (null other-win) 
            (string-match "^ \\*Minibuf" (buffer-name (window-buffer other-win))))
        (error "No window under this one")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun buf-move-left ()
  "Swap the current buffer and the buffer on the left of the split.
If there is no split, ie now window on the left of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'left))
         (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No left split")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun buf-move-right ()
  "Swap the current buffer and the buffer on the right of the split.
If there is no split, ie now window on the right of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'right))
         (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No right split")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

(use-package centaur-tabs
    :demand
    :config
    (setq centaur-tabs-style "bar"
          centaur-tabs-set-modified-marker t

)
    (centaur-tabs-mode t)
    :bind
    ("M-n" . centaur-tabs-backward)
    ("M-e" . centaur-tabs-forward))

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
    (setq dashboard-center-content t)
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

(use-package diminish
  :ensure t
  )

(use-package evil
:ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-below-right t)
  (evil-mode)
  )

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init)

;; Set key bind to comment lines
  (space-leader
    "g c" '(evilnc-comment-or-uncomment-lines :wk "Comment line"))
  )

(use-package evil-tutor :ensure t )

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

;;(global-set-key (kbd "g c") 'evilnc-comment-or-uncomment-lines)

(global-set-key (kbd "C-c <tab>") 'yas-expand)

(use-package helpful
  :ensure t
  :config
  (space-leader
   "h" '(:ignore t :wk "Help")
   "h k" '(helpful-key :wk "Help keybind")
   "h c" '(helpful-callable :wk "Help function")
   "h v" '(helpful-variable :wk "Help variable"))
  )

(use-package rainbow-mode
  :ensure
  :hook org-mode prog-mode
  )

(defun efs/org-mode-setup ()
   (org-indent-mode)
   (variable-pitch-mode 1)
   (visual-line-mode 1))
 ;;Automatic display images in the orgmode
 (setq org-startup-with-inline-images t)
 (use-package org
   :pin org
   :commands (org-capture org-agenda)
   :hook
(org-mode . efs/org-mode-setup)
(org-mode . org-toggle-inline-images)
  )

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 120
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

(electric-indent-mode -1)

(use-package toc-org
  :ensure
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets :ensure)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

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

(use-package projectile 
  :ensure
  :config
  (projectile-mode 1) 
  )

(use-package sudo-edit
  :ensure t
  :config
  (space-leader
   "f s" '(:ignore t :wk "Sudo with files")    
   "f s f" '(sudo-edit-find-file :wk "Sudo find file")
   "f s e" '(sudo-edit :wk "Sudo edit file"))
  )

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

(use-package which-key
  :ensure
  :init
  (which-key-mode 1)
  :config
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.25
        which-key-idle-delay 0.8
        which-key-max-description-length 25
        which-key-allow-imprecise-window-fit nil
        which-key-separator " → " ))

(use-package company
  :ensure t
  :diminish
  :after
  lsp-mode
  :hook
  (lsp-mode . company-mode)
  :bind
  (:map company-active-map
        ("<tab>" . company-select-next))
  ;;(:map lsp-mode-map
  ;;      ("<tab>" . company-select-next))
;;:config
;;(define-key company-active-map (kbd "<tab>") 'company-select-next)
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 1)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))

(use-package company-box
  :ensure t
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

(use-package prettier
  :ensure t
  :hook
  ((mhtml-mode json-mode css-mode ts-mode scss-mode rjsx-mode js2-mode web-mode) . prettier-mode))

(use-package flycheck
  :ensure t
  :defer t
  :diminish ;; Does not show in the mode line as a mode
  :init (global-flycheck-mode))

(use-package lsp-mode
  :ensure t
  :commands
  ;;Activate the package when this functions are called
  (lsp lsp-deferred)
  :hook
  (lsp-mode . efs/lsp-mode-setup)
  ;;Init lsp for the modes:
  (c-mode . lsp-deferred)
  (c-mode . lsp-deferred)
  (csharp-mode . lsp-deferred)
  (c++-mode . lsp-deferred)
  (rust-mode . lsp-deferred)
  ;;(typescript-mode . lsp-deferred)
  (css-mode . lsp-deferred)
  (scss-mode . lsp-deferred)
  (svelte-mode . lsp-deferred)
  (dockerfile-mode . lsp-deferred)
  (yaml-mode . lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-l")  ;; Or 'C-l', 's-l'
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



(use-package dockerfile-mode :ensure t)

;; json-mode
(use-package json-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

(use-package python-mode
  ;; It's already installed
  :ensure nil
  :hook 
  (python-mode . lsp-deferred)
:config 
  (programming-bindins
   "p" '(:ignore t :wk "Python")
   "p s" '(switch-to-buffer :wk "Opens a python shell"))

  ;;:hook (python-mode . lsp-deferred)
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
  ;;(dap-python-debugger 'debugpy)
  ;;:config
  ;;(require 'dap-python)
  )

(use-package rust-mode
  :ensure t)

(use-package svelte-mode
  :ensure t
  )

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
         ;;("\\.tsx\\'" . web-mode)
         ("\\.svelte\\'" . web-mode)
         ;;Was confiliting with lsp
         ;;("\\.css\\'" . web-mode)
         ("\\.html\\'" . web-mode))
  :commands web-mode
  :hook
  (web-mode . luke/webmode-hook)
  (web-mode . lsp-deferred)
  )

(use-package yaml-mode
  :ensure t
  )
