#!/bin/zsh

find . -not \( -name ".?*" -prune \) -type f -print0 | while read -d $'\0' file
do 
	file=${file#\.\/}
	case "$file" in
		README*)
			continue
			;;
		*(~|\.swp))
			continue
			;;
		(source|config)*)
			ofile=$HOME/$file
			;;
		*)
			ofile=$HOME/.$file
			;;
	esac
	echo "Installing: $ofile"
	case "$1" in
		--dryrun)
			continue
			;;
		*)
			ginstall -p -D -m 440 $file $ofile
			;;
	esac
done

case "$1" in
	--dryrun)
		exit
		;;
	*)
		chmod -R 700 ~/.vim/bundle
		;;
esac
