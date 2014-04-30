% Night mode for Xorg
% Ricardo Catalinas Jiménez
% 29 April 2014


  I just wanted to share a simple script I use when I'm working late in
my computer. Apparently there is evidence that [blue light affects
sleep](https://justgetflux.com/research.html) and I've been testing it
for some time and I can say that I feel the difference. After reducing
my exposure to blue light before going to bed I feel more tired and more
predisposed to sleep.

  I don't like installing additional applications for this kind of
purpose, thus I'm using this embarrassedly simple script to reduce the
blue component and the brightness of my Xorg display:

	#!/bin/sh

	OUTPUT=DVI-I-1

	case $1 in
		off)
			xrandr --output $OUTPUT --gamma 1:1:1 --brightness 1.0
			;;
		*)
			xrandr --output $OUTPUT --gamma 1:1:0.5 --brightness 0.7
			;;
	esac
