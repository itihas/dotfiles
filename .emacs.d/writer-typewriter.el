;; EMACS KEYCLICKS AND TYPEWRITER SIMULATION.

;; See configuration section below. Please read all the comments.

;; WHAT IS IT

;; Part of the never-released writer-mode package. Maybe some day
;; writer-mode will be ready for the world, but not yet.

;; There are three things here.

;; 1) writer-typewriter

;; Enter typewriter simulation. Exit with C-g.

;; Sort of an adjunct to a darkroom mode, maybe. Or maybe a lightroom
;; in which there are stark black letters on a white background. Best
;; in full-fullscreen, such as you get on a console, although perhaps
;; the "sheet of paper" will be too wide on some screens.

;; Some people say they are more productive as writers when using an
;; old-fashioned typewriter. They say that the inability to backspace,
;; use arrow keys, or otherwise make corrections or changes while
;; writing is actually an advantage. They say this is because you're
;; more careful about what you write and how you write it, knowing
;; that you can't fix it (at least not immediately).

;; I don't know if I agree, and your own mileage will surely vary. But
;; it's sort of fun to try it out. Well, sort of fun.  OK, maybe not
;; so much. But there is at least one argument that says you will
;; learn to be a more accurate typist. A lot of mistakes get made
;; simply because we know we can fix them at once.

;; Typing sounds, if you have a set of sound files, are automatically
;; turned on in this mode. This means printing keys, reaching the margin,
;; and carriage return all play a sound. See below, under 'installation'
;; to see what you need for sound files.

;; 2) writer-make-noise

;; Make sounds on printing keys, at the right column margin, and
;; on carriage return. Exit with writer-keep-quiet. This is independent
;; of writer-typewriter. Don't use the two together!

;; This is at least minimally useful, allowing the addition of typing
;; sounds just about anywhere in EMACS. It's something I wanted to
;; have for a while, heaven knows why. As noted above, it gets
;; tiresome.

;; The method of doing this third part turned out to be rather
;; different from doing keyclicks during typewriter simulation.  I
;; believe it works pretty reliably on Emacs 23.

;; INSTALLATION

;; Download your favorite typewriter sounds from the
;; net. www.sounddogs.com is one possible place. (I don't have rights
;; to redistribute sounds; sorry.)  Put them somewhere in an
;; accessible directory. Then configure the stuff in the Configuration
;; Section. It's best to byte-compile writer-typewriter.el and load
;; writer-typewriter.elc in your .emacs or other startup file.

;; NOTES

;; This isn't fully complete in that Alt-TAB is still going to switch
;; processes in an X-windows environment. This _can_ be suppressed but
;; I didn't take things that far.

;; Note also that the keyclick implementation is not suitable for
;; visually impaired situations where you want to hear a click every
;; time a key is touched. For that you might look at something like
;; the pyclick package.

;; CREDITS

;; Some ideas in here were taken from other findings on the net.  All
;; credit to previous authors, notably Peter Vasil and Joao Tavora.
;; Thanks to Pascal Bourgignon for suggestions and critiques.

;; DISCLAIMER AND CONTACT

;; My testing environment is Emacs 23.3, Linux Mint 13. Not tested
;; on any other versions, distros, or OS. Things could be done a
;; little differently with Emacs 24, but I'm not on it right now. 
;; No, I'm not going to do a Windows or Mac version.

;; 2013-08-20 0.05 -Eliminated writer-noisy-typewriter. Sounds
;;                 are now always on in typewriter mode. 
;;                 -Force writer-make-noise off when using writer-
;;                 typewriter.
;;                 -In typewriter sim mode, eliminate keyclick on
;;                 spacebar. Thanks to Joe Corneli.
;; 2013-07-30 0.04 Added recentering on page after newline.
;; 2013-07-29 0.03 -Turn off wrapping in typewriter sim (not
;;                 completely correct yet).
;;                 -Get rid of CPU eating loop by handling C-g
;;                 explicitly.
;;                 -Consolidate duplicated code.
;;                 -Use unwind-protect to trap C-g and reset fill,
;;                 etc. 
;; 2013-07-27 0.02 Allow for 'org-return'.
;; 2013-07-27 0.01 Initial release. Bob Newell, Honolulu, Hawai`i.

;; Contact: typewriter@bobnewell.net. This is not a promise of support
;; or anything else.

;; May be used in any legal manner, however ownership of code that is
;; original to me is not transferred.  No warranty offered, no support
;; provided, no liabilities assumed. Use at your own risk.

;;;;;;;;;;;; START CONFIGURATION SECTION

;; Specify sound files here (full path): key click, margin bell,
;; and carriage return sounds.

(defvar writer-keyclick-noise "/home/sahiti/Selectric/AnyKey.wav")
(defvar writer-marginbell-noise "/home/sahiti/Selectric/TabKey.wav")
(defvar writer-linefeed-noise "/home/sahiti/Selectric/Enter.wav")

;; Specify column at which the margin bell rings.

(defvar writer-margin-column 72)

;; Specify a command that can play your sound files from the 
;; command line, preferably something that runs fast.

(defvar writer-play-command "aplay")

;;;;;;;;;;; END CONFIGURATION SECTION

;; I did what I could to minimize sound lag. It is a problem and a
;; most annoying one. I still haven't fully solved it. There can be
;; some lag on the first few keyclicks, while 'aplay' (or whatever) is
;; being loaded. There can be lag later if you don't type for a while
;; and 'aplay' and/or the sound files get moved out of memory or
;; cache. The way to fix this would be to have a running sound daemon
;; (remember the 'sinatra' package?) but I didn't implment that here,
;; at least not yet (and maybe never). Note that lag is also quite
;; dependent on system loading. If the system is not busy,
;; particularly doing i/o, lag will be much less and often not
;; noticeable. It of course helps to keep the sound files small.

(defvar writer-input-charno)
(defvar writer-wrapmode-p)
(defvar writer-makenoise-p)
(defvar writer-save-wrapmode)
(defvar writer-save-autofillmode)
(defvar writer-condition-name)
(defvar writer-save-makenoise)

;; writer-typewriter simulates a typewriter with no noises.

(defun writer-typewriter()
"Act like a typewriter for input"
 (interactive)
 (setq writer-save-makenoise writer-makenoise-p)
;; Turn this off to avoid double sounds.
 (setq writer-makenoise-p nil)
 (writer-typewriter-sim)
)

(defun writer-typewriter-sim ()
"Simulate a typewriter for input"

;; Save the word wrap mode then turn it off. This is a typewriter,
;; there is no wrapping! Ditto with fill mode, which is a little 
;; harder. I am not sure, though, if this works right in org mode,
;; though it appears to.

 (setq writer-save-wrapmode word-wrap)
 (setq word-wrap nil)

;; The following returns nil if auto-fill gets turned off,
;; meaning it was on. It returns a value if auto-fill gets
;; turned on, meaning it was off.

 (setq writer-save-autofillmode (auto-fill-mode))
 (if writer-save-autofillmode 
     (turn-off-auto-fill)
 )

;; Set up in center of screen (if there is enough text so far).

(recenter-top-bottom)

;; Do until C-g is pressed or something messes up.

 (unwind-protect
  (progn

;; It's been noted that '(while t' is a little strange. Yes, it surely
;; is.

    (while t

;; The prompt " " below is completely necessary, otherwise all input
;; gets echoed in the minibuffer whenever the user pauses in typing.
;; It is inefficient but I haven't solved the problem as yet in a
;; better manner. Suggestions most welcome. The answer seems buried
;; in read_char in keyboard.c in EMACS source, but I'm not sure.
  
     (setq writer-input-charno (read-char-exclusive " "))

;; Printing characters are just inserted.

     (if (and (>= writer-input-charno 32)
	      (<= writer-input-charno 126)
         )
	 (progn
	   (insert (char-to-string writer-input-charno))
	   (redisplay t)

;; Play the keyclick sound maybe, but not for spacebar.

	   (if (> writer-input-charno 32)
	     (start-process "writer-noise" nil writer-play-command writer-keyclick-noise)
           )

;; See if we are at the margin.  If so play the bell sound maybe.

	   (if (= (current-column) writer-margin-column)
	         (start-process "writer-noise" nil writer-play-command writer-marginbell-noise)
           )
         )

;; The enter key translates to a newline.

         (if (= writer-input-charno 13)
             (progn
	       (newline)

;; Recenter to be just like a typewriter.

	       (recenter-top-bottom)
	       (redisplay t)

;; Play the carriage return sound maybe.

   	       (start-process "writer-noise" nil writer-play-command writer-linefeed-noise)
             )

         )
      ) ;; end character checking

    ) ;; end 'while t'
  ) ;; end of unwind-protect normal body progn

;; The 'unwind' section, entered on quit or error, or normal
;; termination, the latter of which is not possible given the
;; '(while t' construct above.

  (progn
;; Restore settings.
   (setq word-wrap writer-save-wrapmode)
   (setq writer-makenoise-p writer-save-makenoise)
   (if (not writer-save-autofillmode)
       (turn-on-auto-fill)
   )
  ) ;; end of unwind block

 ) ;; end unwind-protect
)

(defun writer-make-noise ()
"Turn on keyclicks (be careful what you wish for)"
 (interactive)
 (add-hook 'post-command-hook 'writer-make-click-maybe)
)

(defun writer-keep-quiet ()
"Turn off keyclicks, hurrah"
 (interactive)
 (remove-hook 'post-command-hook 'writer-make-click-maybe)
)    

(defun writer-make-click-maybe ()
   (if this-command
	 (if (or 
;; Maybe these would be faster in a list?
	      (equal 'self-insert-command this-command)
	      (equal 'org-self-insert-command this-command))
	   (progn
  	      (start-process "writer-noise" nil writer-play-command writer-keyclick-noise)
              (if (= (current-column) writer-margin-column)
 	          (start-process "writer-noise" nil writer-play-command writer-marginbell-noise)
              )
           )
	   (if (or 
		  (equal 'newline this-command)
		  (equal 'org-return this-command))
  	       (start-process "writer-noise" nil writer-play-command writer-linefeed-noise)
	   )
         )
  )
)

