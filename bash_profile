if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# bash_completion
if [ -f /opt/local/etc/bash_completion ]; then
	. /opt/local/etc/bash_completion 
fi

MKL_NUM_THREADS=1
export MKL_NUM_THREADS
