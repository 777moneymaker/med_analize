#!/usr/bin/env bash

if [[ $EUID -ne 0 ]];
then
    exec sudo /bin/bash "$0" "$@"
fi

echo "Copying script to $HOME/med_analyze"
mkdir $HOME/med_analize
cp ./Program/med_analize.R $HOME/med_analize/
cp ./Program/utilities.R $HOME/med_analize/
ln -s $HOME/med_analize/med_analize.R /usr/local/bin/med_analize
chmod +x /usr/local/bin/med_analize

echo "med_analyze is ready to use \"med_analize --args\""
echo "To remove -> use uninstall.sh script"