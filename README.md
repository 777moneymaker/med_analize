# Med-Analize

Simple statistical analyzer for medical data.

# Installation
WARNING! Make sure, that you are executing install and uninstall script directly in the med_analize folder.

You need to have R language and Rscript installed.
To do it, just use ```sudo apt install r-base```

After this you have to install dependencies.
Execute install_dependencies.R
```
sudo chmod +x install_dependencies.R
# then
./install_dependencies.R
```

After this you can use app by executing script or you can install it via install.sh script.
```
./install.sh
# then use as standalone
med_analize --args
```

To uninstall it -> execute uninstall.sh
```
./uninstall.sh
```

# Example installation
```
user@pc:~/Desktop/med_analize/Program$ sudo ./install_dependencies.R && cd ..
user@pc:~/Desktop/med_analize$ ./install.sh
Installed

... Usage...

user@pc:~/Desktop/med_analize$ ./uninstall.sh
Uninstalled
```

# Usage
WARNING: To use med_analize via Rscript or directly from shell, you have to change 9th line of med_analize.R
```
source('~/med_analize/utilities.R') -> change to -> source('path/to/med_analize/Programs/utilities.R')
```

* For additional info
```
 med_analize --help
```

* Using Rscript
```
Rscript script.R --args
```

* Executing from shell
```
# first
sudo chmod +x script.R
# then
./script.R --args
```

* Using as standalone (recommended-> executing install.sh is required)
```
med_analize --args
```

# Author
* Miłosz Chodkowski @ PUT Poznań [777moneymaker](https://github.com/777moneymaker)
