

for file in `git ls-files`;
do 
	case "$file" in
		README)
			continue
			;;
	esac
	echo "Diffing " $file
	
	if [[ `expr $file : "source"` -ne 0 ]];
	then
		diff $file ~/$file
	elif [[ `expr $file : "config"` -ne 0 ]]; 
	then
		diff $file ~/$file
	else
		diff $file ~/.$file
	fi
done

