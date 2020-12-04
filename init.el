;; japanese
(set-locale-environment nil)
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; --------------------------------------------------------

;; packages

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))
  )

;; ------------------------------------------------------------------------

;; leaf

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init))

(leaf *delete-file-if-no-contents
  :preface (defun my:delete-file-if-no-contents ()
             (when (and (buffer-file-name (current-buffer))
                        (= (point-min) (point-max)))
               (delete-file
                (buffer-file-name (current-buffer)))))
  :config
  (if (not (memq 'my:delete-file-if-no-contents after-save-hook))
      (setq after-save-hook
            (cons 'my:delete-file-if-no-contents after-save-hook)))
  )

(leaf *trailing-white-space
  :preface
  (defvar my:delete-trailing-whitespace-exclude-suffix
    (list "\\.rd$" "\\.md$" "\\.rbt$" "\\.rab$"))
  (defun my:delete-trailing-whitespace ()
    (interactive)
    (eval-when-compile (require 'cl-lib))
    (cond
     ((equal nil
             (cl-loop for pattern in my:delete-trailing-whitespace-exclude-suffix
                      thereis (string-match pattern buffer-file-name)))
      (delete-trailing-whitespace))))
  :hook (before-save-hook . my:delete-trailing-whitespace)
  )

(leaf eldoc
  :hook (emacs-lisp-mode-hook . turn-on-eldoc-mode)
  :preface
  (defun my:shutup-eldoc-message (f &optional string)
    (unless (active-minibuffer-window)
      (funcall f string)))
  :advice
  (:around eldoc-message
           my:shutup-eldoc-message))

(leaf midnight
  :custom
  ((clean-buffer-list-delay-general . 1))
  :hook
  (emacs-startup-hook . midnight-mode))



(leaf autorevert
  :custom
  ((auto-revert-interval . 0.1))
  :hook
  (emacs-startup-hook . global-auto-revert-mode)
  )

(leaf migemo
  :if (executable-find "cmigemo")
  :ensure t
  :require t
  :custom
  '((migemo-user-dictionary  . nil)
    (migemo-regex-dictionary . nil)
    (migemo-options          . '("-q" "--emacs"))
    (migemo-command          . "cmigemo")
    (migemo-coding-system    . 'utf-8-unix))
  :init
  (cond
   ((and (eq system-type 'darwin)
         (file-directory-p "/usr/local/share/migemo/utf-8/"))
    (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict"))
   (t
    (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")))
  :config
  (migemo-init)
  )

(leaf ibuffer
  :after all-the-icons-in-terminal
  :defun (ibuffer-current-buffer)
  :defvar (ibuffer-formats)
  :preface
  (defun my:ibuffer-find-file ()
    "Like `find-file', but default to the directory of the buffer at point."
    (interactive)
    (let ((default-directory
            (let ((buf (ibuffer-current-buffer)))
              (if (buffer-live-p buf)
                  (with-current-buffer buf
                    default-directory)
                default-directory))))
      (find-file default-directory)))
  ;;
  :bind (("C-x C-b" . ibuffer-other-window)
         ("C-x b"   . ibuffer-other-window)
         ("C-x M-b" . ibuffer)
         (:ibuffer-mode-map
          ("C-x C-f" . my:ibuffer-find-file))
         )
  :config
  (define-ibuffer-column icon (:name "  ")
    (let ((icon
           (if (and (buffer-file-name)
                    (all-the-icons-auto-mode-match?))
               (all-the-icons-icon-for-file
                (file-name-nondirectory (buffer-file-name)))
             (all-the-icons-icon-for-mode major-mode ))))
      (if (symbolp icon)
          (setq icon
                (all-the-icons-faicon
                 "file-o"
                 :face 'all-the-icons-dsilver))
        icon)))
  ;;
  (setq ibuffer-formats
        `((mark modified read-only
                " " (icon 2 2 :left :elide)
                ,(propertize " " 'display `(space :align-to 8))
                (name 18 18 :left :elide)
                " " (size 9 -1 :right)
                " " (mode 16 16 :left :elide) " " filename-and-process)
          (mark " " (name 16 -1) " " filename)))
  )

;; --------------------------------------------------------

;; general

;; file
(setq make-backup-files nil)
(setq auto-save-default nil)

;; paren
(electric-pair-mode 1)
(show-paren-mode 1)

;; tab
(setq-default tab-width 4)
(setq-default indent-tabs-mode -1)

;; no message
(setq inhibit-startup-message t)

;; number
(column-number-mode t)
(global-display-line-numbers-mode)

;; scroll
(setq scroll-conservatively 1)

;; cursor
(setq-default cursor-type 'bar)
(set-cursor-color "yellow")

;; color
(set-face-background 'default "black")
(set-face-foreground 'default "white")

;; keybind
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-c\C-h" 'help-command)

;; titlebar
(setq frame-title-format "%f")

;; bar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; yes-or-no to y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

;; beep
(setq ring-bell-function 'ignore)


;; display
(display-time-mode t)

(setq default-frame-alist
      '(
        (width . 180)
        (height . 60)
        ))

;; fullscreen
(global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)


;; Comment line
(global-set-key (kbd "s-/") 'comment-line)

;; alpha
(if window-system
    (progn
      (set-frame-parameter nil 'alpha 95)))

;; region
(transient-mark-mode 1)

;;current directory
(let ((ls (member 'mode-line-buffer-identification
                  mode-line-format)))
  (setcdr ls
          (cons '(:eval (concat " ("
                                (abbreviate-file-name default-directory)
                                ")"))
                (cdr ls))))

;; --------------------------------------------------------

;;; languages

;; Rust
(require 'rustic)
(setq-default rustic-format-trigger 'on-save)
(setq rustic-rustfmt-bin "~/.cargo/bin/rustfmt")
(add-to-list 'rustic-rustfmt-config-alist '("edition" . "2018"))
;;(setq rustic-lsp-server 'rls)
(setq lsp-rust-analyzer-server-command '("~/usr/local/bin/rust-analyzer"))


;; Python
(add-to-list 'exec-path (expand-file-name "~/.pyenv/shims/"))
(add-to-list 'exec-path (expand-file-name "~/.local/bin/"))
(leaf elpy
  :ensure t
  :init
  (elpy-enable))

;; --------------------------------------------------------

;;; mode

(leaf delsel
  :doc "delete selection if you insert"
  :tag "builtin"
  :global-minor-mode delete-selection-mode)

(leaf paren
  :doc "highlight matching paren"
  :tag "builtin"
  :custom ((show-paren-delay . 0.1))
  :global-minor-mode show-paren-mode)

;; ivy
(leaf ivy
  :doc "Incremental Vertical completYon"
  :req "emacs-24.5"
  :tag "matching" "emacs>=24.5"
  :url "https://github.com/abo-abo/swiper"
  :emacs>= 24.5
  :ensure t
  :leaf-defer nil
  :custom ((ivy-initial-inputs-alist . nil)
           (ivy-re-builders-alist . '((t . ivy--regex-fuzzy)
                                      (swiper . ivy--regex-plus)))
           (ivy-use-selectable-prompt . t))
  :global-minor-mode t
  :config
  (leaf swiper
    :doc "Isearch with an overview. Oh, man!"
    :req "emacs-24.5" "ivy-0.13.0"
    :tag "matching" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :bind (("C-s" . swiper)))

  (leaf counsel
    :doc "Various completion functions using Ivy"
    :req "emacs-24.5" "swiper-0.13.0"
    :tag "tools" "matching" "convenience" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :bind (("C-S-s" . counsel-imenu)
           ("C-x C-r" . counsel-recentf))
    :custom `((counsel-yank-pop-separator . "\n----------\n")
              (counsel-find-file-ignore-regexp . ,(rx-to-string '(or "./" "../") 'no-group)))
    :global-minor-mode t))

(leaf ivy-rich
  :doc "More friendly display transformer for ivy."
  :req "emacs-24.5" "ivy-0.8.0"
  :tag "ivy" "emacs>=24.5"
  :emacs>= 24.5
  :ensure t
  :after ivy
  :global-minor-mode t)

(leaf prescient
  :doc "Better sorting and filtering"
  :req "emacs-25.1"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :commands (prescient-persist-mode)
  :custom `((prescient-aggressive-file-save . t)
            (prescient-save-file . ,(locate-user-emacs-file "prescient")))
  :global-minor-mode prescient-persist-mode)

(leaf ivy-prescient
  :doc "prescient.el + Ivy"
  :req "emacs-25.1" "prescient-4.0" "ivy-0.11.0"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :after prescient ivy
  :custom ((ivy-prescient-retain-classic-highlighting . t))
  :global-minor-mode t)


;; flycheck
(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :tag "minor-mode" "tools" "languages" "convenience" "emacs>=24.3"
  :url "http://www.flycheck.org"
  :emacs>= 24.3
  :ensure t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :global-minor-mode global-flycheck-mode)

;; company-jedi
(require 'jedi-core)
(setq jedi:complete-on-dot t)
(setq jedi:use-shortcuts t)
(add-hook 'python-mode-hook 'jedi:setup)
(add-to-list 'company-backends 'company-jedi)

;; docker
(require 'docker)
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" .dockerfile-mode))
(require 'docker-compose-mode)
(require 'docker-tramp-compat)
(set-variable 'docker-tramp-use-names t)

;; web-mode
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'"      . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'"    . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'"      . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'"      . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'"    . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'"      . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'"      . web-mode))

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

(setq web-mode-enable-current-element-highlight t)

(set-face-attribute 'web-mode-doctype-face nil :foreground "Pink3")
(set-face-attribute 'web-mode-html-tag-face nil :foreground "Green")
(set-face-attribute 'web-mode-html-attr-value-face nil :foreground "Yellow")
(set-face-attribute 'web-mode-html-attr-name-face nil :foreground "#0FF")

(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-auto-closing t)

;; Company
(leaf company
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :ensure t
  :leaf-defer nil
  :bind ((company-active-map
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-s" . company-filter-candidates)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("C-n" . company-complete-selection))
         (company-search-map
          ("<tab>" . company-select-next)
          ("C-p" . company-select-previous)))
  :custom ((company-idle-delay . 0)
           (company-minimum-prefix-length . 3)
           (company-transformers . '(company-sort-by-occurrence)))
  :global-minor-mode global-company-mode)

(leaf company-c-headers
  :doc "Company mode backend for C/C++ header files"
  :req "emacs-24.1" "company-0.8"
  :tag "company" "development" "emacs>=24.1"
  :emacs>= 24.1
  :ensure t
  :after company
  :defvar company-backends
  :config
  (add-to-list 'company-backends 'company-c-headers))

;; yasnippet
(leaf yasnippet
  :ensure t
  :disabled t
  :custom
  :hook (after-init-hook . yas-global-mode)
  :bind ((yas-keymap
          ("C-<tab>" . nil)
          ("<tab>"   . nil))  ; for company
         (yas-minor-mode-map
          ("<tab>"   . nil)
          ("C-<tab>" . nil)
          ("C-c y e" . yas-expand)
          ("C-c y i" . yas-insert-snippet)
          ("C-c y n" . yas-new-snippet)
          ("C-c y v" . yas-visit-snippet-file)
          ("C-c y l" . yas-describe-tables)
          ("C-c y g" . yas-reload-all)))
  :init
  (leaf yasnippet-snippets :ensure t)
  (leaf yatemplate :ensure t)
  )

(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

(require 'yasnippet)
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(yas-global-mode 1)

;; --------------------------------------------------------

;; calender
(leaf calendar
  :custom
  (;; 祝日をカレンダーに表示
   (mark-holidays-in-calendar . t)
   ;; 月と曜日の表示調整
   (calendar-month-name-array . ["01" "02" "03" "04" "05" "06"
                                 "07" "08" "09" "10" "11" "12" ])
   (calendar-day-name-array   . ["日" "月" "火" "水" "木" "金" "土"])
   (calendar-day-header-array . ["日" "月" "火" "水" "木" "金" "土"])
   ;; 日曜開始
   (calendar-week-start-day   . 0))
  :config
  (with-eval-after-load "calendar"
    (leaf japanese-holidays
      :ensure t
      :custom
      ((japanese-holiday-weekend         . '(0 6))
       (japanese-holiday-weekend-marker  . '(holiday  ;; 日
                                             nil      ;; 月
                                             nil      ;; 火
                                             nil      ;; 水
                                             nil      ;; 木
                                             nil      ;; 金
                                             japanese-holiday-saturday)))
      :hook
      ((calendar-today-visible-hook   . japanese-holiday-mark-weekend)
       (calendar-today-invisible-hook . japanese-holiday-mark-weekend)
       (calendar-today-visible-hook   . calendar-mark-today))
      :config
      (setq calendar-holidays (append japanese-holidays))
      )
    )
  )


(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(ibuffer-vc docker-compose-mode dockerfile-mode docker company-jedi flycheck japanese-holidays rustic minimap leaf-keywords hydra elpy el-get blackout))))


;;; init.el ends here
