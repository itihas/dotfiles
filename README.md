# dotfiles
My Emacs and GUI-related dotfiles. I run Arch Linux with XMonad, XMobar, and stalonetray, and use XTerm as default terminal.

Everything is Solarized-themed.

XMobar is configured to be tiny and unobtrusive. 

Xmonad has no focused-wondow highlighting, because outlines are ugly and transparency/shadow effects are expensive (compositor) and hurt the clarity of unfocused windows. I'm relying on cursor location, the title in xmobar, and the mouse at the moment; this is awkward and I'm searching for nicer solutions that don't cost me an hour of battery life.

Stalonetray is my concession to Dropbox functionality; I need it for almost nothing else. It sits in the bottom right corner behind everything, as tiny as I can make it without straining to see the dot in the icon.

.emacs, as far as possible, is self-documenting. Theme-wise, I will mention I've created and bound a command to toggle the light and dark solarized themes, which is nice for adjusting to surrounding light conditions.
