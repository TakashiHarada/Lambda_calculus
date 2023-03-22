(setq debug-on-error t)

;; 起動画面表示時間の短縮／削除
(setq inhibit-startup-message t)

;; (line-number-mode t)
;; 行番号表示
(require 'linum)
(global-linum-mode)

;; 列番号表示
(setq column-number-mode t)

;; バックアップファイルの生成を停止
(setq make-backup-files nil)

;; 自動保存ファイルの生成を停止
(setq auto-save-default nil)

;; 文字コードに utf-8 を使用
(prefer-coding-system 'utf-8)

;; スクロールバーを非表示
(scroll-bar-mode -1)

;; ツールバーを非表示
(tool-bar-mode -1)

;; ファイルの種類に応じて文字を色付けしたり，フォントを変更
(global-font-lock-mode t)

;; ファイル（バッファ）末尾においてカーソルを下に移動しても
;; 新しい行（空行）を追加しない
(setq next-line-add-newlines nil)

;; Ctr-h に Back Space を割当
(global-set-key "\C-h" 'delete-backward-char)

;; Hunspell (Spell Checker)
(setq ispell-program-name "hunspell")
(setq ispell-local-dictionary "en_US")

(setq x-alt-keysym 'meta)


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (pinentry gnu-elpa-keyring-update ##))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'mozc)
(setq default-input-method "japanese-mozc")

(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-time-mode t)


;; Ocaml
;; https://github.com/ocaml/tuareg
(load "/home/scinfo/tharada/.emacs.d/tuareg/tuareg-site-file")

;; Scheme
(require 'xscheme)

;; Haskell
;; https://github.com/haskell/haskell-mode
(add-to-list 'load-path "~/.emacs.d/haskell-mode/")
(require 'haskell-mode-autoloads)
(add-to-list 'Info-default-directory-list "~/.emacs.d/haskell-mode/")

(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
