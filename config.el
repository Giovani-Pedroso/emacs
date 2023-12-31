(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

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

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(use-package doom-themes
  :config
  (load-theme 'doom-tomorrow-night  t))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(set-face-attribute 'default nil
                    :font "JetBrains Mono"
                    :height 110
                    :weight 'medium)
(set-face-attribute 'variable-pitch nil
                    :font "Ubuntu"
                    :height 120
                    :weight 'medium)
(set-face-attribute 'fixed-pitch nil
                    :font "JetBrains Mono"
                    :height 110
                    :weight 'medium)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
                    :slant 'italic)

;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-11"))

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

(use-package page-break-lines
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
  :demand t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-below-right t)
  (evil-mode)
  )

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init) )

(use-package evil-tutor :demand t )

(use-package rainbow-mode
  :hook org-mode prog-mode
  )

(electric-indent-mode -1)

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
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
  :config
  (projectile-mode 1) 
  )

(use-package which-key
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
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 1)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))

(use-package company-box
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

(use-package dockerfile-mode)

;; json-mode
(use-package json-mode
  :ensure t)

(use-package python-mode
    :ensure t
    ;;:hook (python-mode . lsp-deferred)
    ;:custom
    ;; NOTE: Set these if Python 3 is called "python3" on your system!
    ;; (python-shell-interpreter "python3")
    ;; (dap-python-executable "python3")
    ;(dap-python-debugger 'debugpy)
    ;:config
    ;(require 'dap-python)
)

(use-package rust-mode
  :ensure t)

(use-package svelte-mode
  :ensure t
  )

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

(use-package yaml-mode
  :ensure t
  )

(use-package general
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer dt/leader-keys
                          :states '(normal insert visual emacs)
                          :keymaps 'override
                          :prefix "SPC" ;; set leader
                          :global-prefix "M-SPC") ;; access leader in insert mode

  (dt/leader-keys
   "b" '(:ignore t :wk "buffer")
   "b b" '(switch-to-buffer :wk "Switch buffer")
   "b i" '(switch-to-ibuffer :wk "Ibuffer")
   "b k" '(kill-this-buffer :wk "Kill this buffer")
   "b n" '(next-buffer :wk "Next buffer")
   "b p" '(previous-buffer :wk "Previous buffer")
   "b r" '(revert-buffer :wk "Reload buffer"))

  (dt/leader-keys
   "e" '(:ignore t :wk "Evaluate")    
   "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
   "e d" '(eval-defun :wk "Evaluate defun containing or after point")
   "e e" '(eval-expression :wk "Evaluate and elisp expression")
   "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
   "e r" '(eval-region :wk "Evaluate elisp in region")) 

  (dt/leader-keys
   "f" '(:ignore t :wk "Find")    
   "f b" '(switch-to-buffer :wk "Find buffer")
   "f f" '(find-file :wk "Find finle"))

  (dt/leader-keys
   "x" '(execute-extended-command :wk "M-x sortcut"))


  (dt/leader-keys
   "h" '(:ignore t :wk "Help")
   "h f" '(describe-function :wk "Describe function")
   "h v" '(describe-variable :wk "Describe variable")
   "h r r" '((lambda () (interactive) (load-file "~/.config/emacs/init.el")) :wk "Reload emacs config"))
  ;;"h r r" '(reload-init-file :wk "Reload emacs config"))

  (dt/leader-keys
   "t" '(:ignore t :wk "Toggle")
   "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
   "t t" '(visual-line-mode :wk "Toggle truncated lines"))     


  (dt/leader-keys
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


