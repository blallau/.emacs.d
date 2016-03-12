# emacs-config

Emacs 24.5 Configuration (using Cask)

# Description

Configure emacs for:
 * Ansible
 * Cask
 * Dropbox
 * GIT
 * IRC
 * LISP
 * Markdown
 * Org mode
   * Ditaa
   * PlantUML
   * export Reveal.js
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

## Reveal.js

    wget https://github.com/hakimel/reveal.js/archive/3.2.0.tar.gz
    tar zxvf 3.2.0.tar.gz
    mv reveal.js-3.2.0 reveal.js

# Proxy

 * Launch Cask with a proxy (cask can't handle proxy user:password)

# Todo

 * Fix Tramp
 * Use use-package

# License

Emacs-config is licensed under the [MIT License](http://www.opensource.org/licenses/mit-license.php).
