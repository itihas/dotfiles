;; -*- eval: (git-auto-commit-mode 1) -*-


(require 'package) ;; You might already have this line
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(setq TeX-auto-save t)
(setq TeX-parse-self t)

(add-hook 'LaTeX-mode-hook 
          (lambda()
             (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
             (setq TeX-command-default "XeLaTeX")
             (setq TeX-save-query nil)
             (setq TeX-show-compilation t)))

(require 'cl)
;; multiple cursor mode is hilarious and awesome
(require 'multiple-cursors)
;; general
(setq browse-url-browser-function 'browse-url-firefox)
(global-subword-mode)


;; encryption
(epa-file-enable)
;; network (proxy)

;; (setq url-proxy-services
;;    '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;;      ("http" . "proxy.iiit.ac.in:8080")
;;      ("https" . "proxy.iiit.ac.in:8080")))

;; (setq url-proxy-services
;;       '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;; 	("http" . "")
;; 	("https" . "")))

;; Open .v files with Proof General's Coq mode
(load "~/.emacs.d/lisp/PG/generic/proof-site")

;; geiser is usually for scheme atm, find a nicer way to do this

(setq geiser-default-implementation 'scheme)


;; haskell

(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(setq haskell-process-log t)



;; leaving Custom
;; (at least for everything but theme because Custom themes rock)

(setq gac-ask-for-summary-p t)
(setq magit-diff-use-overlays nil)
(setq message-default-charset (quote iso-8859-1))
(setq mouse-avoidance-banish-position
      (quote
       ((frame-or-window . frame)
	(side . right)
	(side-pos . 0)
	(top-or-bottom . top)
	(top-or-bottom-pos . 0))))
(setq mouse-avoidance-mode (quote banish))
(setq org-agenda-files '("~/notebook/"))

(setq org-archive-location "./archives/%s_archive::")
(setq org-default-notes-file "~/notebook/capture.org")

(setq org-refile-targets '((nil :maxlevel . 9)
                                (org-agenda-files :maxlevel . 9)))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)                  ; Show full paths for refiling

(setq org-special-ctrl-a/e t)

;; org cdlatex mode

(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

;; imenu/speedbar

(setq org-imenu-depth 10)
(setq imenu-auto-rescan t)
(setq speedbar-generic-list-group-expand-button-type 'expandtag)

;; semantic mode
(semantic-mode t)

;; helm

;; (helm-mode 1)
;; (require 'helm-config)
;; (setq helm-buffers-fuzzy-matching t
;;       helm-recentf-fuzzy-match    t)
;; (setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
;; (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
;; (helm-autoresize-mode t)



;; completion

;; ivy-mode
;; (ivy-mode t)
;; (setq magit-completing-read-function 'ivy-completing-read)

;; ido
(ido-mode t)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)



;; citations: bibtex, reftex
;; (setq org-ref-default-bibliography '("~/notebook/bibliography.bib"))

(setq bibtex-completion-bibliography '("~/notebook/bibliography.bib"))
(setq reftex-default-bibliography '("~/notebook/bibliography.bib"))

(setq bibtex-completion-pdf-field "File")

;; (eval-after-load 'org-plus-contrib (require 'ox-bibtex))

(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
        "bibtex %b"
        "pdflatex -interaction nonstopmode -output-directory %o %f"
        "pdflatex -interaction nonstopmode -output-directory %o %f"))
;; appearances

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(column-number-mode)

(smartparens-global-mode)
(set-fontset-font
 t 'symbol
 (font-spec :family "Symbola") nil 'prepend)


;; ibuffer

(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("dired" (mode . dired-mode))
	       ("orgmode" (or
			   (mode . org-mode)
			   (mode . org-agenda-mode)))
	       ("emacs" (or
			 (name . "^\\*scratch\\*$")
			 (name . "^\\*Messages\\*$")
			 (name . "^\\*Compile-Log\\*$")
			 (name . "^\\*Backtrace\\*$")
			 (name . "^\\*Packages\\*$")))
	       ("elisp" (mode . emacs-lisp-mode))))))

;; keylogger for emacs. find out what I ought to bind things to.
(keyfreq-mode t)
(keyfreq-autosave-mode t)



;; orgmode

;; basics
(require 'org-protocol)
(setq org-directory "~/notebook")
(setq org-enforce-todo-dependencies t) ;; block parent todos from being marked done until children are done.
(setq org-agenda-todo-list-sublevels nil) ;don't list child todos in the agenda view

;; org todo
(setq org-todo-keywords
      (quote ((sequence "todo(t!)" "next(n!)" "working(w!)" "|" "done(d@!)")
	      (sequence  "wait(w@/!)"  "|"  "paused(p@/!)" "cancelled(c@/!)" )
	      (sequence "try(r!)" "trying(i!)" "|" "tried(e@/!)" "nah(h@/!)")
	      )))
(setq org-log-done 'note)
(setq org-log-into-drawer t)
(setq org-log-reschedule 'note)

;; org-drill
(setq org-drill-add-random-noise-to-intervals-p t)
(setq org-drill-scope 'agenda)
;; local keymap
(defun my/org-mode-bindings ()
  (local-set-key (kbd "C-c C-q") 'counsel-org-tag) ;org-tag completion
)

;; appearances

(setq org-support-shift-select t)
(setq org-tags-column 0)
(require 'org-bullets)			;pretty bullets in orgmode
(setq org-bullets-bullet-list '("○" "॰" "•" "‣" "⁃"))
(setq org-startup-indented t)
(setq org-indent-indentation-per-level 1)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-mode-hook (lambda () (visual-line-mode 1)))
(eval-after-load 'org
  '(setf org-highlight-latex-and-related '(latex)))


;; org tagging
(setq org-complete-tags-always-offer-all-agenda-tags t)

;; org capture
(setq org-default-notes-file (concat org-directory "/capture.org"))
(setq org-capture-templates
      '(("i" "Inbox" entry
	 (file "~/notebook/capture.org")
	 "* %? \n:PROPERTIES:\n :CREATED: %U\n :END:\n\n")
	("P" "Paste" entry
	 (file "~/notebook/capture.org")
	 "* %a \n:PROPERTIES:\n :CREATED: %U\n :END:\n\n#+BEGIN_EXAMPLE\n%c\n#+END_EXAMPLE\n%?\n\n")
	("L" "Link Protocol" entry
	 (file "~/notebook/bookmarks.org")
	 "* %a \n:PROPERTIES:\n :CREATED: %U\n :END:\n%?\n\n")
	("p" "Protocol" entry
	 (file "~/notebook/bookmarks.org")
	 "* %a \n:PROPERTIES:\n :CREATED: %U\n :END:\n\n#+BEGIN_EXAMPLE\n%i\n#+END_EXAMPLE\n%?\n\n")
	("b" "Bookmark" entry
	 (file "~/notebook/bookmarks.org")
	 "* %a \n:PROPERTIES:\n :CREATED: %U\n :END:\n \n #+BEGIN_EXPORT html \n%i\n #+END_EXPORT\n %?\n\n")
	("q" "Quote" entry
	 (file "~/notebook/quotes.org")
	 "*  \n:PROPERTIES:\n :CREATED: %U\n :END:\n#+BEGIN_QUOTE %i\n%?\n #+END_QUOTE\n \nEntered on %U\n\n")
        ("j" "Journal" entry
	 (file+datetree "~/notebook/journal.gpg")
	 "*  \n :PROPERTIES: \n :CREATED: %U\n :END:\n\n %?\n\n")
	))

(setq org-file-apps
      '((auto-mode . emacs)
	("\\.mm\\'" . default)
	("\\.x?html?\\'" . default)
	("pdf" . "evince %s")))


;; org export

(setq org-html-checkbox-type 'unicode)


;; org-capture behaviours for floating window scenarios.
(defadvice org-capture
    (after make-full-window-frame activate)
  "Advise capture to be the only window when used as a popup"
  (if (equal "emacs-capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "emacs-capture" (frame-parameter nil 'name))
      (progn
	(delete-frame))))


;; org publishing - publish notebook for easy reading

(require 'ox-publish)
(setq org-publish-project-alist
      '(
	("notebook-notes"
	 :base-directory "~/notebook/"
	 :base-extension "org"
	 :publishing-directory "~/public_html/"
	 :recursive t
	 :publishing-function org-html-publish-to-html
	 :headline-levels 4             ; Just the default for this project.
	 :html-extension "html"
	 :auto-sitemap t
	 )
	("notebook-static"
	 :base-directory "~/notebook/"
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
	 :publishing-directory "~/public_html/"
	 :recursive t
	 :publishing-function org-publish-attachment
	 )
	("notebook" :components "notebook"  "notebook-static") 
      ))








;; openwith - defaults applications for some file types
(require 'openwith)
(openwith-mode t)
(setq openwith-associations '(("\\.pdf\\'" "evince" (file))))


;; default behaviours
(setq blink-cursor-mode nil)
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(set-scroll-bar-mode nil)
(setq show-paren-style 'mixed)
(setq backup-directory-alist `(("." . "~/.saves")))


;; keybindings

;; ;; helm bindings

;; (global-set-key (kbd "M-X") 'execute-extended-command) ;for when I want plain old M-x

;; (global-set-key (kbd "<menu>") 'helm-command-prefix) ;helm interface entry
;; (global-set-key (kbd "M-x") 'helm-M-x)	;sexy helm M-x
;; (global-set-key (kbd "M-y") 'helm-show-kill-ring) ;usable kill ring
;; (global-set-key (kbd "C-b") 'helm-mini)		  ;buffer switching, recent files
;; (global-set-key (kbd "C-f") 'helm-find-files) ; open file

;; easy keys for window management. Key based on ErgoEmacs keybinding
(global-set-key (kbd "C-1") 'delete-other-windows) ; expand current pane
(global-set-key (kbd "C-2") 'split-window-below) ; split pane top/bottom
(global-set-key (kbd "C-3") 'split-window-horizontally) ; split pane top/bottom


(global-set-key (kbd "C-d") 'delete-window) ; close current pane
(global-set-key (kbd "C-z") 'other-window) ; cursor to other pane
(global-set-key (kbd "C-q") 'kill-buffer) ; close current buffer

(global-set-key (kbd "C-=") 'er/expand-region) ; expand selection by semantic unit

;; note the slightly dangerous use of the super key here
(global-set-key (kbd "<s-left>") 'previous-buffer) ; go to previous buffer
(global-set-key (kbd "<s-right>") 'next-buffer) ; go to next buffer


(global-set-key (kbd "C-x C-b") 'ibuffer) ; open ibuffer instead of emacs buffer list

(global-set-key (kbd "C-S") 'ido-occur) ; open ido-occur instead of find-forward



;; orgmode shortcuts
(global-set-key (kbd "<f5> t") 'org-todo-list) ; open org todo list
(global-set-key (kbd "<f5> a") 'org-agenda-list) ; open org agenda
(global-set-key (kbd "<f5> s") 'org-store-link) ; store an org link
(global-set-key (kbd "<f5> c")  'org-capture) ; org capture
(global-set-key (kbd "<f5> w")  'org-copy-subtree) ; org copy subtree at point

(global-set-key (kbd "<f9>")  (lambda () (interactive) (org-timer-set-timer "00:00:30"))) ; start review timer. meant to be used in capture context, primarily.

;; appearance things
(global-set-key (kbd "M-o") 'olivetti-mode) ; toggle olivetti mode
(global-set-key (kbd "<f6>") 'cycle-themes) ; switch between themes; see cycle-themes definition below
(global-set-key (kbd "<f7>") 'toggle-serif) ; switch between themes; see cycle-themes definition below

;; counts
(global-set-key (kbd "<f8>") 'count-words) ; count words

;; multiple cursor mode
(global-set-key (kbd "C-S-SPC") 'mc/mark-pop) ;add cursor at point and move to previous mark in kill ring



;; aliases
(defalias 'os 'olivetti-set-width)

;; miscellany from interesting places

;; below from http://tonyballantyne.com/tech/transposition/ - potentially awesome aliases
(defalias 'ts 'transpose-sentences)
(defalias 'tp 'transpose-paragraphs)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 95 :width normal :foundry "GOOG" :family "Noto Mono"))))
 '(org-tag ((((class color) (min-colors 89)) (:weight bold)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("8ed752276957903a270c797c4ab52931199806ccd9f0c3bb77f6f4b9e71b9272" "2a739405edf418b8581dcd176aaf695d319f99e3488224a3c495cb0f9fd814e3" "dbc36265f8078e5b36e907b7753cd5b538f702f9f1be398d17ff9c5b442fe8cf" "31992d4488dba5b28ddb0c16914bf5726dc41588c2b1c1a2fd16516ea92c1d8e" "a49760e39bd7d7876c94ee4bf483760e064002830a63e24c2842a536c6a52756" "67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" "f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" "a0dc0c1805398db495ecda1994c744ad1a91a9455f2a17b59b716f72d3585dde" "9d91458c4ad7c74cf946bd97ad085c0f6a40c370ac0a1cbeb2e3879f15b40553" "32ffeb13f3c152300d14757b431967e63da005f54712dad6a2f8b8b33fb94bac" "c7a9a68bd07e38620a5508fef62ec079d274475c8f92d75ed0c33c45fbe306bc" "4e753673a37c71b07e3026be75dc6af3efbac5ce335f3707b7d6a110ecb636a3" "7dbb593ad0fb90230b196ffbd6a503c3e9086925cc68f212e625a017b8c824a7" "0e219d63550634bc5b0c214aced55eb9528640377daf486e13fb18a32bf39856" "9492d427a99f6e99c66d31a804e38a6ff995dec7c5940e5dd37f242d39fd41f0" "62408b3adcd05f887b6357e5bd9221652984a389e9b015f87bbc596aba62ba48" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "9f3dd1d7b46e99b94bb53506c44b651c811b3552100898842bdd22ce63ab8b55" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(magit-diff-use-overlays nil t)
 '(org-agenda-files
   (quote
    ("~/notebook/rulechanging/jul2017submission.org" "/home/sahiti/notebook/modal.org" "/home/sahiti/notebook/bookmarks.org" "/home/sahiti/notebook/bucket.org" "/home/sahiti/notebook/capture.org" "/home/sahiti/notebook/confluence2016.org" "/home/sahiti/notebook/debug.org" "/home/sahiti/notebook/emacs-goals.org" "/home/sahiti/notebook/ev-nextquarter-4-17.org" "/home/sahiti/notebook/export-0.org" "/home/sahiti/notebook/export-1.org" "/home/sahiti/notebook/health.org" "/home/sahiti/notebook/predictions.org" "/home/sahiti/notebook/progressreport_oct_25_2016.org" "/home/sahiti/notebook/quotes.org" "/home/sahiti/notebook/reading.org" "/home/sahiti/notebook/research_interests.org" "/home/sahiti/notebook/sitemap.org" "/home/sahiti/notebook/system.org" "/home/sahiti/notebook/thesis.org")))
 '(org-babel-load-languages (quote ((emacs-lisp . t) (haskell . t))))
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 1.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
		 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(package-selected-packages
   (quote
    (cdlatex flyspell-lazy flyspell-popup pydoc ox-reveal multiple-cursors org-plus-contrib latex-preview-pane smartparens latex-math-preview zotxt syntax-subword shakespeare-mode ido-at-point icicles auctex elm-mode ecb imenu-anywhere intero auto-indent-mode outshine outorg swoop ido-occur flex-isearch flx-ido expand-region company-coq monokai-theme jedi scion typing zenburn-theme windata web-mode tuareg tree-mode tabbar solarized-theme smex request-deferred relative-line-numbers rainbow-mode rainbow-delimiters rainbow-blocks racket-mode quack python-mode parsebib paredit-menu org-wc org-pomodoro org-if org-bullets openwith olivetti nyan-mode nlinum mingus magit linum-relative keyfreq ido-vertical-mode ibuffer-vc ht haskell-mode gnuplot-mode gnuplot git-timemachine git-auto-commit-mode geiser eink-theme direx dired-subtree dash-functional counsel biblio anaconda-mode)))
 '(safe-local-variable-values
   (quote
    ((haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4)
     (eval git-auto-commit-mode nil))))
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-very-old-color nil))

;; theme cycling

(defun cycle-themes (φn)
  "Cycle themes among a preset list.

If `universal-argument' is called first, cycle n steps. Default is 1 step.

Adapted from

URL `http://ergoemacs.org/emacs/elisp_toggle_command.html'
Version 2015-12-17"
  (interactive "p")
  ;; uses a property “state”. Value is a integer.
  (let* (
         (ξvalues (list 'solarized-dark 'solarized-light 'eink 'zenburn 'monokai))
         (ξindex-before
          (if (get 'cycle-themes 'state)
	      (get 'cycle-themes 'state)
            0))
         (ξindex-after (% (+ ξindex-before (length ξvalues) φn) (length ξvalues)))
         (ξprev-value (nth  ξindex-before ξvalues))
         (ξnext-value (nth  ξindex-after ξvalues)))

    (put 'cycle-themes 'state ξindex-after)

    (disable-theme ξprev-value)
    (load-theme ξnext-value)
    (message "background color changed to %s" ξnext-value)))

(cycle-themes 0)



;; switch faces between fixed-width and variable


(defvar serif-preserve-default-list nil
  "A list holding the faces that preserve the default family and
  height when TOGGLE-SERIF is used.")
(setq serif-preserve-default-list
      '(;; LaTeX markup
        font-latex-math-face
        font-latex-sedate-face
        font-latex-warning-face
        ;; org markup
        org-latex-and-related
        org-meta-line
        org-verbatim
        org-block-begin-line
        ;; syntax highlighting using font-lock
        font-lock-builtin-face
        font-lock-comment-delimiter-face
        font-lock-comment-face
        font-lock-constant-face
        font-lock-doc-face
        font-lock-function-name-face
        font-lock-keyword-face
        font-lock-negation-char-face
        font-lock-preprocessor-face
        font-lock-regexp-grouping-backslash
        font-lock-regexp-grouping-construct
        font-lock-string-face
        font-lock-type-face
        font-lock-variable-name-face
        font-lock-warning-face))

(defun toggle-serif ()
  "Change the default face of the current buffer to use a serif family."
  (interactive)
  (when (display-graphic-p)  ;; this is only for graphical emacs
    ;; the serif font familiy and height, save the default attributes
    (let ((serif-fam "Droid Serif")
          (serif-height 110)
          (default-fam (face-attribute 'default :family))
          (default-height (face-attribute 'default :height)))
      (if (not (bound-and-true-p default-cookie))
          (progn (make-local-variable 'default-cookie)
                 (make-local-variable 'preserve-default-cookies-list)
                 (setq preserve-default-cookies-list nil)
                 ;; remap default face to serif
                 (setq default-cookie
                       (face-remap-add-relative
                        'default :family serif-fam :height serif-height))
                 ;; keep previously defined monospace fonts the same
                 (dolist (face serif-preserve-default-list)
                   (add-to-list 'preserve-default-cookies-list
                                (face-remap-add-relative
                                 face :family default-fam :height default-height)))
                 (message "Turned on serif writing font."))
        ;; undo changes
        (progn (face-remap-remove-relative default-cookie)
               (dolist (cookie preserve-default-cookies-list)
                 (face-remap-remove-relative cookie))
               (setq default-cookie nil)
               (setq preserve-default-cookies-list nil)
               (message "Restored default fonts."))))))
;; at the end because apparently org-plus-contrib is only included by Custom.
;; (require 'ox-bibtex)

