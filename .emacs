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
(semantic-mode 1)

;; helm
(helm-mode 1)
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
(global-set-key (kbd "<f6>") 'helm-google-suggest)


;; bibtex
(setq bibtex-completion-bibliography '("~/notebook/misc.bib"))
(setq reftex-default-bibliography '("~/notebook/misc.bib"))

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


;; org export

(setq org-html-checkbox-type 'unicode)







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

;; helm bindings
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-b") 'helm-multi-files)
(global-set-key (kbd "C-f") 'helm-find-files) ; open file

;; easy keys for window management. Key based on ErgoEmacs keybinding
(global-set-key (kbd "C-1") 'delete-other-windows) ; expand current pane
(global-set-key (kbd "C-2") 'split-window-below) ; split pane top/bottom
(global-set-key (kbd "C-3") 'split-window-horizontally) ; split pane top/bottom


(global-set-key (kbd "<f12>") 'save-buffer) ; save

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




(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "1ASC" :family "Liberation Mono"))))
 '(org-tag ((t (:weight bold :height 0.8)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-safe-themes
   (quote
    ("40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "9f3dd1d7b46e99b94bb53506c44b651c811b3552100898842bdd22ce63ab8b55" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))))



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

