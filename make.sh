

for file in `git ls-files`;
do 
	if [[ `expr substring $file 1 6` -eq "source" ]]; then
		echo $file
		# ginstall -p -D -m 440 $file ~/.$file
		#ginstall -p -D -m 440 $file
	fi
done
