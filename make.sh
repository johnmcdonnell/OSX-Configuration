

for file in `git ls-files`;
do 
	case "$file" in
		README)
			continue
			;;
	esac
	
	if [[ `expr $file : "source"` -ne 0 ]];
	then
		echo "Installing " ~/$file
		ginstall -p -D -m 440 $file ~/$file
	elif [[ `expr $file : "config"` -ne 0 ]]; 
	then
		echo "Installing " ~/$file
		ginstall -p -D -m 440 $file ~/$file
	else
		echo "Installing "~/.$file
		ginstall -p -D -m 440 $file ~/.$file
	fi
done

