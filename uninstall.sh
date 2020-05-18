#!/usr/bin/env bash

if [[ $EUID -ne 0 ]];
then
    exec sudo /bin/bash "$0" "$@"
fi

DIR="${HOME}/med_analize"
FILE="/usr/local/bin/med_analize"

if [ -d "$DIR" ]; then
	echo 'Removing directory conteining files.'
  	rm -r ${DIR}
else
	echo "ERROR: Directory containing program doesn't exist."
	exit 1
fi

if [ -L "$FILE" ]; then
	echo 'Links removed.'
  	rm "$FILE"
else
	echo "ERROR: Symbolic link not found."
	exit 1
fi

echo "Program uninstalled."

