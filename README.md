# dotfiles
My Emacs and GUI-related dotfiles. I run Arch Linux with Xmonad, xmobar, and stalonetray. XTerm is default terminal.

Everything is Solarized-themed.

Xmonad modifier is changed to Super/Windows.

Xmobar is configured to be tiny and unobtrusive. 

I've set window borders to zero, because outlines are ugly; this has the unwanted effect of removing focused-window highlighting. Unocused windows are set to be a little transparent instead. With a clean and themed background this seems to work okay.
(Compton is necessary anyway now, t o prevent screen tearing. :sigh:)

Stalonetray is my concession to Dropbox functionality; I need it for almost nothing else. It sits in the bottom right corner behind everything, as tiny as I can make it without straining to see the dot in the icon. Since it seems like Dropbox is likely not the only program that will ever give me this problem, I suppose I'll live with it.

My .emacs, as far as possible, is documented in the comments, and is very young. I'm currently migrating back to Custom, so variable definitions are split between the Custom section and not. This is ugly, please don't use it as is.
