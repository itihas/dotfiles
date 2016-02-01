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

;; orgmode

(setq org-directory "~/notebook")
(setq org-mobile-inbox-for-pull "~/notebook/mobile_capture.org") ;; new mobile notes go here
(setq org-mobile-directory "~/Dropbox/MobileOrg");; mobile org
(setq org-enforce-todo-dependencies t) ;; block parent todos from being marked done until children are done.
(setq org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "HIATUS(h)" "|" "DONE(d)")
			  (sequence "|" "CANCELED(c)"))) ;; todo keywords
(setq org-log-done 'note)
;;(setq org-tags-alist
;; org capture
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-c c" 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/notebook/capture.org" "Tasks")
	 "* TODO %?\n  %i\n  %a")
	("m" "Misc" entry (file+headline "~/notebook/capture.org" "Misc")
	 "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/notebook/journal.org")
	 "* %?\nEntered on %U\n  %i\n  %a")))
(setq org-refile-targets
      '((org-agenda-files . (:level . 1))))




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
;; (setq initial-buffer-choice "~/notebook")
(setq backup-directory-alist `(("." . "~/.saves")))

;; solarized theme toggle
(setq islight nil)

(defun toggle-theme-solarized
    ()
    (interactive)
  (if islight
      (progn
	(load-theme 'solarized-dark)
	(setq islight nil))
    (progn
      (load-theme 'solarized-light)
      (setq islight t))))

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

(global-set-key (kbd "C-d") 'delete-window) ; close current pane
(global-set-key (kbd "C-z") 'other-window) ; cursor to other pane
(global-set-key (kbd "C-q") 'kill-buffer) ; close current buffer


(global-set-key (kbd "<C-left>") 'previous-buffer) ; go to previous buffer
(global-set-key (kbd "<C-right>") 'next-buffer) ; go to next buffer
(global-set-key (kbd "C-b") 'ibuffer) ; open ibuffer instead of emacs buffer list


;; see toggle-theme-solarized above. this is to make it easy to switch between "day" and "night" modes.
(global-set-key (kbd "<f7>") 'toggle-theme-solarized) ; split pane top/bottom

;; orgmode shortcuts
(global-set-key (kbd "<f5> t") 'org-todo-list) ; open org todo list
(global-set-key (kbd "<f5> a") 'org-agenda-list) ; open org agenda
(global-set-key (kbd "<f5> s") 'org-store-link) ; store an org link

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(org-agenda-files
   (quote
    ("~/notebook/Sem10/classes_sem10.org" "/home/sahiti/notebook/capture.org" "/home/sahiti/notebook/journal.org" "/home/sahiti/notebook/org.org" "/home/sahiti/notebook/project.org" "/home/sahiti/notebook/reading.org" "/home/sahiti/notebook/someday.org" "/home/sahiti/notebook/todo.org" "/home/sahiti/notebook/whiteboard-2016-jan.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
