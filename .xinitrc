setxkbmap -layout 'us,ru(winkeys)' -option 'grp:ctrl_shift_toggle'
# xloadimage -onroot -fit ~/Pictures/Wall2.jpg

trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand false --widthtype pixel --width 80 --height 16 --transparent true --tint 0x666666 &
exec xmonad
