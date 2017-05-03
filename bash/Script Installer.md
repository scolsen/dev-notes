Small bash script that can be used to “install” a target script into the user’s PATH. If the script works correctly, the user should be able to run your script as a bash command.

First we’ll set two important variables, a flag to determine whether the script is already installed, and the source directory of our script.

```sh
#!/bin/bash 
INSTALLED=0
SRC_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)" #this line grabs the location the install script is saved at. 
```

Next, we’ll determine if the script is already installed. If not, we’ll add it to the user’s bin so that we can run it as we would any other bash command. It’s a good practice to install scripts in `​~/bin`​, so that’s where we’ll try to install our script. If the `​~/bin`​ directory does not exist or is not present on the PATH, we’ll mind our manners and ask the user if she would like to create that directory or add it to the path—we shouldn’t simply create it or add it to the PATH without asking—that would be impolite. The user may have good reasons this directory is not on the PATH or does not exist.

```sh
check_install () {
  CURRBIN="$(ls ~/bin)" #store the curent contents of ~/bin
  if [[ $CURRBIN != *"script-to-install"* ]]; then
    if [ ! -d ~/bin ]; then
      echo "Target install directory ~/bin does not exist. Create the install directory? (y/n)"
      read answer 
      if [ answer == 'y' ] || [ answer == 'yes' ] || [ answer == 'Yes' ] || [ answer == 'YES' ]; then 
        mkdir ~/bin
        else 
          exit
      fi
    fi
```

Once we have our install location settled, we merely have to copy our script to the target location, then add our install location to the path.

```sh
    cp -R $SRC_DIR ~/bin
    if [[ $PATH != *"~/bin"* ]]; then 
      echo "~/bin is not currently on your PATH. Adding ~/bin to your path will ensure you can run this script as a command from any location. Add ~/bin to your PATH? (y/n)"
      read answer 
      if [ answer == 'y' ] || [ answer == 'yes' ] || [ answer == 'Yes' ] || [ answer == 'YES' ]; then 
        export PATH=$PATH:~/bin 
      else 
        echo "Okay. ~/bin was not added to the PATH."
        exit
      fi
    fi
```

After that, we’re more or less ready to role. We should let the user know, and set INSTALLED to 1 in case any of our above conditions aren’t met, in which case the script is already installed and we have nothing to do.

```sh
    echo "script-to-install installed. Run script-to-install to use."
  else 
    INSTALLED=1
  fi
}
```

Now for the main portion of our script. We’ll check if the script is installed, if not we’ll install it, otherwise, we’ll go ahead and run whatever process the script contains, such as arugment processing.

```sh
check_install
if [ $INSTALLED == 1]; then
  process_args "$@"
fi
```

Here’s the full installer script:

```sh
#!/bin/bash 
INSTALLED=0
SRC_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)" 
check_install () {
  CURRBIN="$(ls ~/bin)" 
  if [[ $CURRBIN != *"script-to-install"* ]]; then
    if [ ! -d ~/bin ]; then
      echo "Target install directory ~/bin does not exist. Create the install directory? (y/n)"
      read answer 
      if [ answer == 'y' ] || [ answer == 'yes' ] || [ answer == 'Yes' ] || [ answer == 'YES' ]; then 
        mkdir ~/bin
        else 
          exit
      fi
    fi
    cp -R $SRC_DIR ~/bin
    if [[ $PATH != *"~/bin"* ]]; then 
      echo "~/bin is not currently on your PATH. Adding ~/bin to your path will ensure you can run this script as a command from any location. Add ~/bin to your PATH? (y/n)"
      read answer 
      if [ answer == 'y' ] || [ answer == 'yes' ] || [ answer == 'Yes' ] || [ answer == 'YES' ]; then 
        export PATH=$PATH:~/bin 
      else 
        echo "Okay. ~/bin was not added to the PATH."
        exit
      fi
    fi
    echo "script-to-install installed. Run script-to-install to use."
  else 
    INSTALLED=1
  fi
}
check_install
if [ $INSTALLED == 1]; then
  process_args "$@"
fi
```



