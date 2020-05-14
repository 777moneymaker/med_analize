#!/bin/bash

if [[ $EUID -ne 0 ]];
then
    exec sudo /bin/bash "$0" "$@"
fi

DIR="${HOME}/med_analize"
FILE="/usr/local/bin/med_analize"

if [ -d "$DIR" ]; then
	printf 'Uninstalling...\n'
  	rm -r ${DIR}
else
	printf "ERROR: Directory containing program doesn't exist\n"
	exit 1
fi

if [ -L "$FILE" ]; then
	printf 'Removing symbolic links...\n'
  	rm "$FILE"
else
	printf "ERROR: Symbolic link not found!\n"
	exit 1
fi

printf "Uninstalled successfully!\n"

