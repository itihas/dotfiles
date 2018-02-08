#!/bin/sh
# -*- eval: (git-auto-commit-mode 1); -*-

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

merge in defaults and keymaps

if [ -f $sysresources ]; then


    xrdb -merge $sysresources

fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then



    xrdb -merge "$userresources"

    
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# for dunst

systemctl --user import-environment DISPLAY

# # start some nice programs

# if [ -d /etc/X11/xinit/xinitrc.d ] ; then
#  for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
#   [ -x "$f" ] && . "$f"
#  done
#  unset f
# fi



# # compositing
# # compton -CGb

# # systray
# stalonetray &

# automount disks
udiskie &

# colour temperature adjuster
redshift-gtk &

# Dropbox
dropbox &

# Set background image with feh
feh --bg-fill /home/sahiti/background.png &

exec i3
