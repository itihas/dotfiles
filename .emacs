;; -*- eval: (git-auto-commit-mode 1) -*-

(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;; typewriter-noises!!!
(byte-compile-file "/home/sahiti/.emacs.d/writer-typewriter.el")
(load "/home/sahiti/.emacs.d/writer-typewriter.elc")


;; general

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



;; orgmode

;; basics
(require 'org-protocol)
(setq org-directory "~/notebook")
(setq org-enforce-todo-dependencies t) ;; block parent todos from being marked done until children are done.
(setq org-agenda-todo-list-sublevels nil) ;don't list child todos in the agenda view
;; org todo
(setq org-todo-keywords
      (quote ((sequence "TODO(t!)" "|" "DONE(d@!)")
	      (sequence  "WAIT(w@/!)"  "|"  "PAUSED(p@/!)" "CANCELLED(c@/!)" )
	      (sequence "IDEA(i!)" "MAYBE(y!)" "WORKING(k!)" "|" "USED(u/@!)")
	      )))
(setq org-log-done 'note)
(setq org-log-into-drawer t)
(setq org-log-reschedule 'note)

;; appearances

(setq org-tags-column 0)
(setq org-hide-emphasis-markers t)	;hide font styles markup
(setq org-pretty-entities t)		;pretty-print symbols, super/subscript, etc.
(require 'org-bullets)			;pretty bullets in orgmode
(setq org-bullets-bullet-list '("○" "॰" "•" "჻"))
(setq org-startup-indented t)
(setq org-indent-indentation-per-level 1)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; org tagging
(setq org-complete-tags-always-offer-all-agenda-tags t)

;; org capture
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-capture-templates
      '(("i" "Inbox" entry (file "~/notebook/capture.org")
	 "* %? \n:PROPERTIES:\n :CREATED: %U\n :END:\n  %i\n")
	("b" "Bookmark" entry (file "~/notebook/capture.org")
	 "* %c %^G \n:PROPERTIES:\n :CREATED: %U\n :END:\n\n %? \n #+BEGIN_QUOTE \n%i\n #+END_QUOTE\n")
	("q" "Quote" entry (file "~/notebook/quotes.org")
	 "*  \n:PROPERTIES:\n :CREATED: %U\n :END:\n#+BEGIN_QUOTE %i\n%?\n #+END_QUOTE\n \nEntered on %U\n")
        ("j" "Journal" entry (file+datetree "~/notebook/journal.org")
	 "*%^{Title}  \n :PROPERTIES: \n :CREATED: %U\n :END:\n\n %?\n")
	("p" "Prediction" entry (file "~/notebook/predictions.org")
	 "* %^{Prediction} \n :PROPERTIES:\n :CREATED: %U\n :END:\n %^{ODDS}p \n DEADLINE:%^t \n ")))

(setq org-file-apps
      '((auto-mode . emacs)
	("\\.mm\\'" . default)
	("\\.x?html?\\'" . default)
	("pdf" . "llpp %s")))










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

;; easy keys for window management. Key based on ErgoEmacs keybinding
(global-set-key (kbd "C-1") 'delete-other-windows) ; expand current pane
(global-set-key (kbd "C-2") 'split-window-below) ; split pane top/bottom
(global-set-key (kbd "C-3") 'split-window-horizontally) ; split pane top/bottom
(global-set-key (kbd "C-.") 'enlarge-window) ; split pane top/bottom
(global-set-key (kbd "C-;") 'shrink-window) ; split pane top/bottom
(global-set-key (kbd "C->") 'enlarge-window-horizontally) ; split pane top/bottom
(global-set-key (kbd "C-:") 'shrink-window-horizontally) ; split pane top/bottom

(global-set-key (kbd "C-f") 'find-file) ; open file
(global-set-key (kbd "<f12>") 'save-buffer) ; save

(global-set-key (kbd "C-d") 'delete-window) ; close current pane
(global-set-key (kbd "C-z") 'other-window) ; cursor to other pane
(global-set-key (kbd "C-q") 'kill-buffer) ; close current buffer


(global-set-key (kbd "<C-left>") 'previous-buffer) ; go to previous buffer
(global-set-key (kbd "<C-right>") 'next-buffer) ; go to next buffer
(global-set-key (kbd "C-b") 'ibuffer) ; open ibuffer instead of emacs buffer list


;; orgmode shortcuts
(global-set-key (kbd "<f5> t") 'org-todo-list) ; open org todo list
(global-set-key (kbd "<f5> a") 'org-agenda-list) ; open org agenda
(global-set-key (kbd "<f5> s") 'org-store-link) ; store an org link
(global-set-key (kbd "<f5> c")  'org-capture) ; org capture
(global-set-key (kbd "<f5> x")  'org-toggle-pretty-entities) ; pretty printing in org
(global-set-key (kbd "<f5> p")  'org-pomodoro) ; timeboxing yay
(global-set-key (kbd "<f5> x")  'org-toggle-pretty-entities) ; pretty printing in org

;; appearance things
(global-set-key (kbd "M-o") 'olivetti-mode) ; toggle olivetti mode
(global-set-key (kbd "M-v") 'variable-pitch-mode) ; toggle monospace
(global-set-key (kbd "<f7>") 'cycle-themes) ; switch between themes; see cycle-themes definition below

;; counts
(global-set-key (kbd "<f8>") 'count-words) ; count words




;; aliases
(defalias 'os 'olivetti-set-width)

;; miscellany from interesting places

;; below from http://tonyballantyne.com/tech/transposition/ - potentially awesome aliases
(defalias 'ts 'transpose-sentences)
(defalias 'tp 'transpose-paragraphs)

;; Custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(fci-rule-color "#073642")
 '(gac-ask-for-summary-p nil)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(magit-diff-use-overlays nil)
 '(message-default-charset (quote iso-8859-1))
 '(mouse-avoidance-banish-position
   (quote
    ((frame-or-window . frame)
     (side . right)
     (side-pos . 0)
     (top-or-bottom . top)
     (top-or-bottom-pos . 0))))
 '(mouse-avoidance-mode (quote banish) nil (avoid))
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-agenda-files
   (quote
    ("~/notebook/Sem10/classes_sem10.org" "/home/sahiti/notebook/capture.org" "/home/sahiti/notebook/journal.org" "/home/sahiti/notebook/reading.org" "/home/sahiti/notebook/bucket.org" "/home/sahiti/notebook/quotes.org" "/home/sahiti/notebook/health.org")))
 '(org-archive-location "./archives/%s_archive::")
 '(org-default-notes-file "~/notebook/notes.org")
 '(org-refile-targets (quote ((org-agenda-files :maxlevel . 4))))
 '(org-refile-use-outline-path (quote file))
 '(org-special-ctrl-a/e t)
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#ff7f00")
     (60 . "#ffbf00")
     (80 . "#b58900")
     (100 . "#ffff00")
     (120 . "#ffff00")
     (140 . "#ffff00")
     (160 . "#ffff00")
     (180 . "#859900")
     (200 . "#aaff55")
     (220 . "#7fff7f")
     (240 . "#55ffaa")
     (260 . "#2affd4")
     (280 . "#2aa198")
     (300 . "#00ffff")
     (320 . "#00ffff")
     (340 . "#00ffff")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))

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
         (ξvalues (list 'solarized-dark 'solarized-light 'eink 'zenburn 'leuven))
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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#fffff8" :foreground "#111111" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "1ASC" :family "Liberation Mono")))))
