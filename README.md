# Med-Analize

Simple statistical analyzer for medical data.

# Installation
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

# Usage
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

* Using as standalone (executing install.sh is required)
```
med_analize --args
```

# Author
* Miłosz Chodkowski @ PUT Poznań [777moneymaker](https://github.com/777moneymaker)
