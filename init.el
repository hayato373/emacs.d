

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

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

;; 環境を日本語、UTF-8にする
(set-locale-environment nil)
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

(setq skk-japanese-message-and-error nil)
(setq skk-show-japanese-menu nil)


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

; flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; company-jedi
(require 'jedi-core)
(setq jedi:complete-on-dot t)
(setq jedi:use-shortcuts t)
(add-hook 'python-mode-hook 'jedi:setup)
(add-to-list 'company-backends 'company-jedi)





;; *.~ とかのバックアップファイルを作らない
(setq make-backup-files nil)

;; .#* とかのバックアップファイルを作らない
(setq auto-save-default nil)

;; 括弧を自動で補完する
(electric-pair-mode 1)

;; tab
(setq-default tab-width 4)
(setq-default indent-tabs-mode t)

;; デフォルトの起動時のメッセージを表示しない
(setq inhibit-startup-message t)

;; 列の番号
(column-number-mode t)

;; 行番号の表示
(global-display-line-numbers-mode)

;; 1行ごとの改ページ
(setq scroll-conservatively 1)

;; 対応する括弧を光らせる
(show-paren-mode 1)

;; cursor
(setq cursor-type 'bar)

;; color
(set-face-background 'default "black")
(set-face-foreground 'default "white")

;; keybind
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-c\C-h" 'help-command)

;; titlebar
(setq frame-title-format "%f")

;; メニューバーの非表示
(menu-bar-mode -1)

;; ツールバーの非表示
(tool-bar-mode -1)

;; 別のウィンドウ
(global-set-key "\C-t" 'other-window)

;;
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; indent
(electric-indent-mode -1)

;; Comment line
(global-set-key (kbd "s-/") 'comment-line)

;; alpha
(if window-system
    (progn
      (set-frame-parameter nil 'alpha 95)))

;; リージョンのハイライト
(transient-mark-mode 1)

;;current directory 表示
(let ((ls (member 'mode-line-buffer-identification
                  mode-line-format)))
  (setcdr ls
          (cons '(:eval (concat " ("
                                (abbreviate-file-name default-directory)
                                ")"))
                (cdr ls))))

;; 時刻をモードラインに表示
(display-time-mode t)

;; Company
(require 'company)
(global-company-mode) ; 全バッファで有効にする
(setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
(setq company-idle-delay 0) ; デフォルトは0.5
(setq company-minimum-prefix-length 3) ; デフォルトは4
(setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
(setq completion-ignore-case t)
(setq company-dabbrev-downcase nil)
(global-set-key (kbd "C-M-i") 'company-complete)
(define-key company-active-map [tab] 'company-select-next) ;; TABで候補を設定
(define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete) ;; 各種メジャーモードでも C-M-iで company-modeの補完を使う

;; yasnippetとの連携
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



;; yes-or-no to y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

;; beep
(setq ring-bell-function 'ignore)


;; ウィンドウサイズ
(setq default-frame-alist
      '(
        (width . 180)
        (height . 60)
        ))

;; fullscreen
(global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)

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
    (company-jedi flycheck japanese-holidays rustic minimap leaf-keywords hydra elpy el-get blackout))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 ;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
