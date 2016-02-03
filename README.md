# dotfiles
My Emacs and GUI-related dotfiles. I run Arch Linux with Xmonad, xmobar, and stalonetray, and use XTerm as default terminal.

Everything is Solarized-themed.

Xmobar is configured to be tiny and unobtrusive. 

I've set window borders to zero, because outlines are ugly; this has the unwanted effect of removing focused-window highlighting. Transparency/shadow effects to achieve the same are expensive (compositor) and hurt the clarity of unfocused windows. I'm relying on cursor and pointer location and the title in xmobar at the moment; this is awkward and I'm searching for nicer solutions that don't cost me an hour of battery life.

Stalonetray is my concession to Dropbox functionality; I need it for almost nothing else. It sits in the bottom right corner behind everything, as tiny as I can make it without straining to see the dot in the icon. Since it seems like Dropbox is likely not the only program that will ever give me this problem, I suppose I'll live with it.

My .emacs, as far as possible, is documented in the comments, and is very young - I recently migrated away from custom, and am adding things only as I need them. Theme-wise, I will mention I've created and bound a command to toggle the light and dark solarized themes, which is nice for adjusting to surrounding light conditions.
