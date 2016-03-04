# emacs-config

Emacs 24.5 Configuration (using Cask)

# Description

Configure emacs for:
 * Ansible
 * Cask
 * Ditaa
 * GIT
 * IRC
 * LISP
 * Markdown
 * Org mode
 * PlantUML
 * Python
 * ...

# Usage

## Install Cask

    git clone https://github.com/cask/cask.git ~/.cask
    # Add to ~/.bashrc or ~/.bash_aliases
    export PATH=$PATH:~/.cask/bin

## Configure Emacs

    git clone https://github.com/blallau/.emacs.d ~/.emacs.d/
    cd ~/.emacs.d/; cask install

## Launch Emacs

    emacs

# Language Configuration

## Python

    sudo apt-get install python-flake8 python-virtualenv
    # On Emacs M-X jedi:install-server

# Proxy

 * Launch Cask with a proxy (cask can't handle proxy user:password)

# License

Emacs-config is licensed under the [MIT License](http://www.opensource.org/licenses/mit-license.php).
