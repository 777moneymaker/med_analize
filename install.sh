#!/usr/bin/env bash

if [[ $EUID -ne 0 ]];
then
    exec sudo /bin/bash "$0" "$@"
fi

printf 'Copying script to $HOME/med_analyze/\n'
mkdir $HOME/med_analize
cp ./med_analize.R $HOME/med_analize/
ln -s $HOME/med_analize/med_analize.R /usr/local/bin/med_analize
chmod +x /usr/local/bin/med_analize

printf "Linking ...\n"
printf "Done!\n"
printf "Ready to use -> med_analyze --args\n"
printf "To remove just use uninstall.sh script\n"