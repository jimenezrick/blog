% Night mode for Xorg
% Ricardo Catalinas Jiménez
% 30 April 2014


  I just wanted to share a simple script I use when I'm working late in
my computer. Apparently there is evidence that [blue light affects
sleep](https://justgetflux.com/research.html) and I've been testing it
for some time and I can say that I feel the difference. After reducing
my exposure to blue light before going to bed I feel more tired and more
predisposed to sleep.

  I don't like installing additional applications for this kind of
purpose, thus I'm using this embarrassedly simple script to reduce the
blue component and the brightness of all my Xorg displays:

	#!/bin/sh

	BLUE=0.5
	BRIGHTNESS=0.7
	OUTPUTS=$(xrandr | sed -n 's/^\([^ ]*\).*\<connected\>.*/\1/p')

	adjust_output() {
		blue=$1
		brightness=$2
		shift 2

		for out in $*
		do
			xrandr --output $out --gamma 1:1:$blue --brightness $brightness
		done
	}

	case $1 in
		off)
			adjust_output 1 1.0 $OUTPUTS
			;;
		*)
			adjust_output $BLUE $BRIGHTNESS $OUTPUTS
			;;
	esac
