;; -*- eval: (git-auto-commit-mode 1) -*-


(require 'package) ;; You might already have this line
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line


(require 'cl)

;; general

;; network (proxy)

(setq url-proxy-services
   '(("no_proxy" . "^\\(localhost\\|10.*\\)")
     ("http" . "proxy.iiit.ac.in:8080")
     ("https" . "proxy.iiit.ac.in:8080")))



;; leaving Custom
;; (at least for everything but theme because Custom themes rock)

(setq gac-ask-for-summary-p nil)
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
(ido-everywhere 1)
(ido-vertical-mode 1)
(ido-ubiquitous-mode 1)
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

;; appearances

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(column-number-mode)
		      

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
(require 'org-drill)
(setq org-directory "~/notebook")
(setq org-enforce-todo-dependencies t) ;; block parent todos from being marked done until children are done.
(setq org-agenda-todo-list-sublevels nil) ;don't list child todos in the agenda view

;; org todo
(setq org-todo-keywords
      (quote ((sequence "TODO(t!)" "NEXT(n!)" "OVERDUE(o@!)" "|" "DONE(d@!)")
	      (sequence  "WAIT(w@/!)"  "|"  "PAUSED(p@/!)" "CANCELLED(c@/!)" )
	      (sequence "IDEA(i!)" "MAYBE(y!)" "WORKING(k!)" "|" "USED(u/@!)")
	      )))
(setq org-log-done 'note)
(setq org-log-into-drawer t)
(setq org-log-reschedule 'note)

;; org-drill
(setq org-drill-add-random-noise-to-intervals-p t)

;; local keymap
(defun my/org-mode-bindings ()
  (local-set-key (kbd "C-c C-q") 'counsel-org-tag) ;org-tag completion
)

;; appearances

(setq org-support-shift-select t)
(setq org-tags-column 0)
(setq org-hide-emphasis-markers t)	;hide font styles markup
(setq org-pretty-entities t)		;pretty-print symbols, super/subscript, etc.
(require 'org-bullets)			;pretty bullets in orgmode
(setq org-bullets-bullet-list '("○" "॰" "•" "჻"))
(setq org-startup-indented t)
(setq org-indent-indentation-per-level 1)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(add-hook 'org-mode-hook (lambda () (visual-line-mode 1)))

;; org tagging
(setq org-complete-tags-always-offer-all-agenda-tags t)

;; org capture
(setq org-default-notes-file (concat org-directory "/capture.org"))
(setq org-capture-templates
      '(("i" "Inbox" entry (file "~/notebook/capture.org")
	 "* %? \n:PROPERTIES:\n :CREATED: %U\n :END:\n %i\n")
	("b" "Bookmark" entry (file "~/notebook/bookmarks.org")
	 "* %c \n:PROPERTIES:\n :CREATED: %U\n :END:\n \n #+BEGIN_QUOTE \n%i\n #+END_QUOTE\n %? ")
	("q" "Quote" entry (file "~/notebook/quotes.org")
	 "*  \n:PROPERTIES:\n :CREATED: %U\n :END:\n#+BEGIN_QUOTE %i\n%?\n #+END_QUOTE\n \nEntered on %U\n")
        ("j" "Journal" entry (file+datetree "~/notebook/journal.org")
	 "*  \n :PROPERTIES: \n :CREATED: %U\n :END:\n\n %?\n")
	("p" "Prediction" entry (file "~/notebook/predictions.org")
	 "* %^{Prediction} \n :PROPERTIES:\n :CREATED: %U\n :END:\n | %U | %^{ODDS} | \n DEADLINE:%^t \n ")
	("v" "Vocabulary" entry
               (file+headline (concat org-directory "/vocab.org")
                              "Vocabulary")
               "* %^{The word} :drill:\n** Meaning \n%^{Definition}\n** POS\n ^{POS}\n** Pronunciation\n^{Pronunciation}\n** Example\n^{Example}\n** Language\n^{Language}\n** Word Origin\n^{Origin}\n")))

(setq org-file-apps
      '((auto-mode . emacs)
	("\\.mm\\'" . default)
	("\\.x?html?\\'" . default)
	("pdf" . "llpp %s")))


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
      (delete-frame)))


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
(setq openwith-associations '(("\\.pdf\\'" "llpp" (file))))

;; jedi setup.
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)                 ; optional

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

;; note the slightly dangerous use of the super key here
(global-set-key (kbd "<s-left>") 'previous-buffer) ; go to previous buffer
(global-set-key (kbd "<s-right>") 'next-buffer) ; go to next buffer
(global-set-key (kbd "C-x C-b") 'ibuffer) ; open ibuffer instead of emacs buffer list


;; orgmode shortcuts
(global-set-key (kbd "<f5> t") 'org-todo-list) ; open org todo list
(global-set-key (kbd "<f5> a") 'org-agenda-list) ; open org agenda
(global-set-key (kbd "<f5> s") 'org-store-link) ; store an org link
(global-set-key (kbd "<f5> c")  'org-capture) ; org capture

;; appearance things
(global-set-key (kbd "M-o") 'olivetti-mode) ; toggle olivetti mode
(global-set-key (kbd "<f6>") 'cycle-themes) ; switch between themes; see cycle-themes definition below

;; counts
(global-set-key (kbd "<f8>") 'count-words) ; count words





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
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "1ASC" :family "Liberation Mono"))))
 '(org-tag ((t (:weight bold :height 0.8)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("0e219d63550634bc5b0c214aced55eb9528640377daf486e13fb18a32bf39856" "9492d427a99f6e99c66d31a804e38a6ff995dec7c5940e5dd37f242d39fd41f0" "62408b3adcd05f887b6357e5bd9221652984a389e9b015f87bbc596aba62ba48" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "9f3dd1d7b46e99b94bb53506c44b651c811b3552100898842bdd22ce63ab8b55" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(fci-rule-color "#eee8d5")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(magit-diff-use-overlays nil t)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (scion typing zenburn-theme windata web-mode tuareg tree-mode tabbar solarized-theme smex request-deferred relative-line-numbers rainbow-mode rainbow-delimiters rainbow-blocks racket-mode quack python-mode parsebib paredit-menu ox-reveal org-wc org-pomodoro org-pdfview org-mobile-sync org-jekyll org-if org-drill-table org-bullets openwith olivetti nyan-mode nlinum mingus magit linum-relative keyfreq ido-vertical-mode ido-ubiquitous ibuffer-vc ht haskell-mode gnuplot-mode gnuplot git-timemachine git-auto-commit-mode geiser eink-theme direx dired-subtree dash-functional counsel biblio anaconda-mode)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))



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
         (ξvalues (list 'solarized-light 'solarized-dark 'eink 'zenburn 'leuven))
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

