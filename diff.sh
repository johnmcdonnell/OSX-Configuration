#!/bin/zsh

# Find will avoid issues with filenames containing spaces.
find . -not \( -name ".?*" -prune \) -type f -print0 | while read -d $'\0' file
do
	#file=`cut -c 3- $file`
	file=${file#\.\/}
	case "$file" in
		README)
			continue
			;;
		*(.png|.jpg|.db|.sh|~|.swp))
			continue
			;;
		(source|config)*)
			ofile="$HOME/$file"
			;;
		*)
			ofile="$HOME/.$file"
			;;
	esac
	echo "Diffing $file with $ofile"
	diff "$file" "$ofile"
done

