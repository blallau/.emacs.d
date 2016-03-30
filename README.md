# emacs-config

Emacs 24.5 Configuration (using Cask)

# Description

Configure emacs for:
 * Ansible
 * Cask
 * Dropbox
 * GIT
 * GoLang
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

## GoLang

    sudo tar -C /usr/local -xzf go<version>.linux-amd64.tar.gz
    export PATH=$PATH:/usr/local/go/bin

    export GOPATH=$HOME/work/go
    export PATH=$PATH:$GOPATH/bin

    # install gocode, godef, go oracle
    go get -u github.com/nsf/gocode
    go get -u github.com/rogpeppe/godef
    go get -u golang.org/x/tools/cmd/oracle

# Proxy

 * Launch Cask with a proxy (cask can't handle proxy user:password)

# Todo

 * Fix Tramp
 * Use use-package

# License

Emacs-config is licensed under the [MIT License](http://www.opensource.org/licenses/mit-license.php).
