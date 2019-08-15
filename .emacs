;; comment commands
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.react.js\\'" . web-mode))

(setq web-mode-engines-alist
      '(("django" . "\\.html\\'")))
(add-to-list 'auto-mode-alist '("/data/users/jschumann/www-hg/html/intern/js/.*\\.js[x]?\\'" . web-mode))
(defun pk-web-mode-hook ()
  "Hooks for Web mode."
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "White")
  (set-face-attribute 'web-mode-html-tag-face nil :foreground "Yellow")
  )
(add-hook 'web-mode-hook  'pk-web-mode-hook)


;; enabling line numbers by defaut

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
;; Preset `nlinum-format' for minimum width.
(defun my-nlinum-mode-hook ()
  (when nlinum-mode
    (setq-local nlinum-format
                (concat "%" (number-to-string
                             ;; Guesstimate number of buffer lines.
                             (ceiling (log (max 1 (/ (buffer-size) 80)) 10)))
                        "d "))))
(add-hook 'nlinum-mode-hook #'my-nlinum-mode-hook)

(global-nlinum-mode)
;;merge mode hook



;; adding themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8de9f0d0e8d041ac7e7fc9d7db2aff119259eea297ccc247e81470851df32602" default)))
 '(package-selected-packages
   (quote
    (json-reformatter-jq json-reformat hack-mode mmm-mode multiple-cursors hack-time-mode company php-mode golden-ratio-scroll-screen nlinum go-mode yaml-mode web-mode python-django yasnippet))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-function-name-face ((t (:foreground "color-27"))))
 '(font-lock-string-face ((t (:foreground "color-141")))))

;; not full scroll c-v m-v
(require 'golden-ratio-scroll-screen)
(global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
(global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up)

;; org mode bindings
(add-hook 'org-load-hook
          (lambda ()
            (define-key org-mode-map "\M-n" 'org-metadown)
            (define-key org-mode-map "\M-p" 'org-metaup)))
;; org mode invis text
(setq org-catch-invisible-edits 'smart)
(setq org-list-demote-modify-bullet '( ("-" . "+") ("*" . "+")("+" . "-")))


;; company
(require 'company)
(global-set-key (kbd "M-TAB") 'company-complete)
(setq company-idle-delay  nil) ;; removing typeahead display
(add-hook 'after-init-hook 'global-company-mode) ;; default on


;;hack
(require 'hack-mode)
(setq hack-for-hiphop-root "~/www")
(load "/home/engshare/tools/hack-for-hiphop")
(add-to-list 'auto-mode-alist '("\\.php\\'" . hack-mode))

;; python+ sql = mmm
 (require 'mmm-mode)
 (set-face-background 'mmm-default-submode-face nil)

(mmm-add-classes
 '((python-sql
    :submode sql-mode
    :face mmm-code-submode-face
    :front "# SQL\\(\n\\|\t\\)*\\(\[ -_A-Z0-9\]+\\)\\(\[ =\]\\)\\(\"\"\"\\|'''\\)"
    :back "\\(\"\"\"\\|'''\\)\\( \\|\t\\|\n\\)*\\# /SQL")))

(mmm-add-mode-ext-class 'python-mode "*.py" 'python-sql)


;; hack debug (borked)
(load-library "/home/jschumann/fbsource/fbcode/emacs_config/emacs-packages/hphpd.el")

(defun hphpd-localhost ()
  "Start HipHop Debugger against localhost"
  (interactive)
  (hphpd "hphpd -h localhost"))

(defun hphpd-script ()
  "Start HipHop Debugger against script"
  (interactive)
  (hphpd "hphpd -f ~/www/scripts/insights/insights_generate_metrics.php"))


;; save sessions
(desktop-save-mode 1)
(put 'upcase-region 'disabled nil)
