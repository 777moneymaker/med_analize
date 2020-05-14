#!/bin/bash


DIR="${HOME}/med_analize"

if [ -d "$DIR" ]; then
	printf 'Uninstalling...\n'
  	rm -r ${DIR}
else
	printf "ERROR: Directory containing program doesn't exist\n"
	exit 1
fi

printf "Uninstalled successfully!\n"

