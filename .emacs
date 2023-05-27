;; Run m-x package-install-selected-packages to install 

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
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
;; (require 'ein)
;; (require 'ein-notebook)
;; (require 'ein-subpackages)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))
(defvar my-packages
  '(web-mode golden-ratio-scroll-screen company mmm-mode nlinum clang-format prettier-js go-mode)

  "A list of packages to ensure are installed at launch.")
; install the missing packages
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.react.js\\'" . web-mode))

(setq web-mode-engines-alist
      '(("django" . "\\.html\\'")))
;;(add-to-list 'auto-mode-alist '("/data/users/jschumann/www-hg/html/intern/js/.*\\.js[x]?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts[x]?\\'" . web-mode))
(defun pk-web-mode-hook ()
  "Hooks for Web mode."
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "White")
  (set-face-attribute 'web-mode-html-tag-face nil :foreground "Yellow")
  )
(add-hook 'web-mode-hook  'pk-web-mode-hook)
;; tab spaces war
(setq-default tab-width 4)

;; terminal doesnt ring bell 
(setq visible-bell t)

;; enabling line numbers by defaut

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
 '(clang-format-style
   "{BasedOnStyle: llvm, ColumnLimit: 120, IndentWidth: 4, AlwaysBreakTemplateDeclarations: Yes}")
 '(cmake-ide-build-dir "/home/jschumann/src/star/")
 '(custom-safe-themes
   '("8de9f0d0e8d041ac7e7fc9d7db2aff119259eea297ccc247e81470851df32602" default))
 '(flycheck-checker-error-threshold 800)
 '(flycheck-clangcheck-build-path "~/src/star/")
 '(flycheck-eslint-args "-c ~/src/star/notebook/.eslintrc")
 '(flycheck-javascript-eslint-executable "~/src/star/notebook/node_modules/eslint/bin/eslint.js")
 '(flycheck-rst-sphinx-executable "/home/jschumann/.nix-profile/bin/sphinx-build")
 '(gc-cons-threshold 100000000)
 '(helm-ag-base-command
   "ag --no-color --nogroup -W 150 --ignore=output/ --ignore=*.orig --ignore=*.*#")
 '(helm-locate-command "locate %s -e -A --regex %s \"$(pwd)\"")
 '(indent-tabs-mode nil)
 '(irony-server-install-prefix "/run/current-system/sw")
 '(make-backup-files nil)
 '(mode-require-final-newline nil)
 '(package-selected-packages
   '(flycheck-rust helm-projectile projectile flycheck-irony company-irony irony no-littering xclip rust-mode sphinx-mode company-terraform terraform-mode ws-butler cmake-ide zzz-to-char tide rainbow-delimiters flycheck-rtags rtags flycheck-clangcheck flycheck lua-mode helm-ag nix-mode lsp-mode protobuf-mode gnu-elpa-keyring-update ein python-black blacken sml-mode clang-format json-reformatter-jq json-reformat mmm-mode multiple-cursors hack-time-mode company php-mode golden-ratio-scroll-screen nlinum go-mode yaml-mode web-mode python-django yasnippet)) 
 '(prettier-js-args '("--tab-width" "4" "--print-width" "100" "--semi" "false"))
 '(prettier-js-command "~/.npm-packages/bin/prettier")
 '(tide-completion-fuzzy t)
 '(tide-server-max-response-length 204800)
 '(tide-tscompiler-executable "~/src/star/notebook/node_modules/typescript/bin/tsc")
 '(tide-tsserver-executable "~/src/star/notebook/node_modules/typescript/bin/tsserver")
 '(tide-user-preferences
   '(:includeCompletionsForModuleExports t :includeCompletionsWithInsertText t :allowTextChangesInNewFiles t :generateReturnInDocTemplate t :noErrorTruncation t))
 '(vc-annotate-background-mode nil)
 '(warning-suppress-types '((comp) (comp) (comp) (auto-save))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:inherit diff-changed :extend t :background "#5faf00"))))
 '(ediff-current-diff-C ((t (:extend t :background "#5f0087"))))
 '(ediff-fine-diff-C ((t (:background "#8700af"))))
 '(font-lock-builtin-face ((t (:foreground "#5f87af"))))
 '(font-lock-comment-face ((t (:foreground "#8FA6B2"))))
 '(font-lock-function-name-face ((t (:foreground "#0087ff"))))
 '(font-lock-keyword-face ((t (:foreground "#AC69F2"))))
 '(font-lock-string-face ((t (:foreground "#9FB8F6"))))
 '(font-lock-type-face ((t (:foreground "#00af00"))))
 '(font-lock-variable-name-face ((t (:foreground "#d78700"))))
 '(helm-ff-file ((t (:inherit font-lock-builtin-face :extend t :foreground "brightwhite"))))
 '(highlight ((t (:background "#5f005f"))))
 '(linum ((t (:inherit default :foreground "#808080"))))
 '(log-view-message ((t (:extend t :background "brightblack"))))
 '(lsp-face-highlight-textual ((t (:inherit highlight :background "magenta"))))
 '(lsp-headerline-breadcrumb-path-face ((t (:inherit lsp-headerline-breadcrumb-symbols-face))))
 '(lsp-headerline-breadcrumb-symbols-face ((t (:foreground "black" :weight bold))))
 '(match ((t (:background "purple"))))
 '(minibuffer-prompt ((t (:foreground "#00d7ff"))))
 '(rst-level-1 ((t (:background "brightblack"))))
 '(rst-level-2 ((t (:background "brightblack"))))
 '(rst-level-3 ((t (:background "brightblack"))))
 '(vc-annotate-face-FFCCCC ((t (:background "#Ffcccc"))) t))


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
(setq company-idle-delay  nil) ;; removing typeahead display
(add-hook 'after-init-hook 'global-company-mode) ;; default on

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
;; python interpreter
(setq python-shell-interpreter "python3")


(defun hphpd-localhost ()
  "Start HipHop Debugger against localhost"
  (interactive)
  (hphpd "hphpd -h localhost"))

(defun hphpd-script ()
  "Start HipHop Debugger against script"
  (interactive)
  (hphpd "hphpd -f ~/www/scripts/insights/insights_generate_metrics.php"))

;; clang-format
(require 'clang-format)
(global-set-key (kbd "C-c u") 'clang-format-buffer)

;; prettier
(require 'prettier-js)
(defun enable-prettier-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
		  (funcall (cdr my-pair)))))

(add-hook 'web-mode-hook #'(lambda ()
                            (enable-prettier-mode
                             '("\\.jsx?\\'" . prettier-js-mode))))

;; projectile
(require 'projectile)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

(global-unset-key (kbd "C-x l"))
;; my keybindings
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-q") 'company-complete)
    (define-key map (kbd "C-j") nil)
    (define-key map (kbd "C-j w") 'avy-goto-word-1)
    (define-key map (kbd "C-j c") 'avy-goto-char-in-line)
    (define-key map (kbd "C-x C-f") 'helm-projectile)
    ;;(define-key map (kbd "C-x C-f") 'helm-locate)
    (define-key map (kbd "C-x c b") 'helm-bookmarks)
    (define-key map (kbd "C-x c k") 'helm-show-kill-ring)
    (define-key map (kbd "C-x c r") 'helm-resume)
    (define-key map (kbd "C-x r l") 'helm-bookmarks)
    (define-key map (kbd "C-x l r") 'lsp-rename)
    (define-key map (kbd "C-x ;") 'comment-line)
    (define-key map (kbd "C-x f") 'find-file)
    map)
  "my-keys-minor-mode keymap.")



(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")

(my-keys-minor-mode 1)

;; save sessions
(desktop-save-mode 1)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; backup in one place. flat, no tree structure
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;;(require 'no-littering)



; roslaunch highlighting
(add-to-list 'auto-mode-alist '("\\.launch$" . xml-mode))

;; lsp stuff
(require 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'nix-mode-hook 'lsp)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))


;; cmake
(require 'rtags)
(cmake-ide-setup)
(add-hook 'c-mode-hook 'rtags-start-process-unless-running)
(add-hook 'c++-mode-hook 'rtags-start-process-unless-running)

(setq read-process-output-max (* 1024 1024)) ;; 1mb


;; disable flymake
(setq lsp-diagnostics-provider :none)
;; enable flycheck ;;can swap to after-init-hook
(add-hook 'c++-mode-hook #'global-flycheck-mode)
;; flycheck c++ include
;; (add-hook 'c++-mode-hook
;;           (lambda () (setq flycheck-clang-include-path
;;                            (list (expand-file-name "~/src/star/")))))
(with-eval-after-load "flycheck"
    (setq flycheck-clang-warnings `(,@flycheck-clang-warnings
                                    "no-pragma-once-outside-header")))
;;flycheck clangcheck
;; (require 'flycheck-clangcheck)

;; (defun my-select-clangcheck-for-checker ()
;;   "Select clang-check for flycheck's checker."
;;   (flycheck-set-checker-executable 'c/c++-clangcheck
;;                                    "/home/jschumann/.nix-profile/bin/clang-check")
;;   (flycheck-select-checker 'c/c++-clangcheck))

;; (add-hook 'c-mode-hook #'my-select-clangcheck-for-checker)
;; (add-hook 'c++-mode-hook #'my-select-clangcheck-for-checker)
;; (setq debug-on-message "Error")
;; enable static analysis
;;(setq flycheck-clangcheck-analyze t)


;;flycheck rtags
;; ensure that we use only rtags checking
;; https://github.com/Andersbakken/rtags#optional-1

;;(require 'flycheck-rtags) 

;; doesnt highlight missing semicolon :/

;; Optional explicitly select the RTags Flycheck checker for c or c++ major mode.
;; Turn off Flycheck highlighting, use the RTags one.
;; Turn off automatic Flycheck syntax checking rtags does this manually.
;; (defun my-flycheck-rtags-setup ()
;;   "Configure flycheck-rtags for better experience."
;;   (flycheck-select-checker 'rtags)
;;   ;; (setq-local flycheck-check-syntax-automatically nil)
;;   ;; (setq-local flycheck-highlighting-mode nil)
;;   )
;; (add-hook 'c-mode-hook #'my-flycheck-rtags-setup)
;; (add-hook 'c++-mode-hook #'my-flycheck-rtags-setup)
;; (add-hook 'objc-mode-hook #'my-flycheck-rtags-setup
;;          )


;; (defun setup-flycheck-rtags ()
;;   (interactive)
;;   (flycheck-select-checker 'rtags)
;;   ;; RTags creates more accurate overlays.
;;   (setq-local flycheck-highlighting-mode nil)
;;   (setq-local flycheck-check-syntax-automatically nil))

;; ;; only run this if rtags is installed
;; (when (require 'rtags nil :noerror)
;;   ;; make sure you have company-mode installed
;;   (require 'company)
;;   (define-key c-mode-base-map (kbd "M-.")
;;     (function rtags-find-symbol-at-point))
;;   (define-key c-mode-base-map (kbd "M-,")
;;     (function rtags-find-references-at-point))
;;   ;; disable prelude's use of C-c r, as this is the rtags keyboard prefix
;; ;;  (define-key prelude-mode-map (kbd "C-c r") nil)
;;   ;; install standard rtags keybindings. Do M-. on the symbol below to
;;   ;; jump to definition and see the keybindings.
;;   (rtags-enable-standard-keybindings)
;;   ;; comment this out if you don't have or don't use helm
;;   (setq rtags-use-helm t)
;;   ;; company completion setup
;;   (setq rtags-autostart-diagnostics t)
;;   (rtags-diagnostics)
;;   (setq rtags-completions-enabled t)
;;   (push 'company-rtags company-backends)
;;   (global-company-mode)
;;   (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))
;;   ;; use rtags flycheck mode -- clang warnings shown inline
;;   (require 'flycheck-rtags)
;;   ;; c-mode-common-hook is also called by c++-mode
;;   (add-hook 'c-mode-common-hook #'setup-flycheck-rtags))

;;sed -e 's/build\/source/home\/jschumann\/src\/star/g'

;; cpp / c++ stuff
(setq-default c-basic-offset 4)
(add-hook 'c++-mode-hook 
(lambda ()  (define-key c++-mode-map (kbd "C-x c f") 'clang-format-region)))

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)



(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; helm
(with-eval-after-load 'helm
  (define-key helm-map (kbd "TAB")       #'nil)
  (define-key helm-map (kbd "C-TAB")       #'helm-select-action)
  (define-key helm-map (kbd "<backtab>") #'helm-previous-line))

(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode 1)
(set-face-attribute 'helm-selection nil 
                    :background "purple"
                    :foreground "black")
;; helm-ag
(setq helm-ag-insert-at-point 'symbol)
(global-set-key (kbd "M-/") 'helm-ag-project-root)



;; nix
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))


;; lock files
(setq create-lockfiles nil)


;; prevent window split (helm + grep)
(setq split-height-threshold nil
      split-width-threshold nil)

;; ediff
    ;; (add-hook 'ediff-startup-hook
    ;;           (lambda () 
    ;;             (progn
    ;;               (select-frame-by-name "Ediff")
    ;;               (set-frame-size(selected-frame) 40 40))))
;;     (defun my-ediff-ash ()
;;       "Function to be called after buffers and window setup for ediff."
;; 	  (other-window))
;; (add-hook 'ediff-after-setup-windows-hook 'my-ediff-ash 'append)


;; tide

;; tide things,
(require 'tide)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (ws-butler-mode +1)
  ;;(typescript-mode)
  ;;(prettier-js-mode +1) 
  (tide-hl-identifier-mode +1)
  (modify-syntax-entry ?# "_" typescript-mode-syntax-table)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))




;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;;(add-hook 'before-save-hook ')

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(eval-after-load "tide"
  '(define-key tide-mode-map (kbd "C-x c f") 'prettier-region))

(eval-after-load "tide"
  '(define-key tide-mode-map (kbd "C-x x s") 'tide-restart-server))


(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (typescript-mode))))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "ts" (file-name-extension buffer-file-name))
              (typescript-mode))))
(add-hook 'js2-mode-hook #'setup-tide-mode)
;; configure javascript-tide checker to run after your default javascript checker
(require 'flycheck)
(flycheck-add-next-checker 'javascript-tide 'javascript-eslint 'append)
;; configure jsx-tide checker to run after your default jsx checker
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

(defun my-print-region ()
  (message "%d %d" (region-beginning) (region-end))
  )

(defun prettier-region (posBegin posEnd)
  "Print number of words and chars in region."
  (interactive "r")
  (message "Formatting …")
  (let* (
         (old-prettier-args prettier-js-args)
         (prettier-js-args (append old-prettier-args (list "--range-start" (number-to-string posBegin) "--range-end" (number-to-string posEnd))))
         )
    (prettier-js)
    )
  )

;; zzz-mode
(global-set-key (kbd "M-z") #'zzz-to-char)

;; rust
(add-hook 'rust-mode-hook #'yas-minor-mode)
(add-hook 'rust-mode-hook #'lsp)
(add-hook 'rust-mode-hook #'flycheck-mode)
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
(add-hook 'rust-mode-hook
	  (lambda () (setq indent-tabs-mode nil)))
(add-hook 'rust-mode-hook 
(lambda ()  (define-key rust-mode-map (kbd "M-?") 'lsp-find-references)))
(setq rust-format-on-save t)
