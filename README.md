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
 * JSON
 * LISP
 * Markdown
 * Org mode
   * Ditaa
   * PlantUML
   * export Reveal.js
 * Puppet
 * Python
 * Ruby
 * SaltStack
 * The Silver Searcher (Ag)
 * Terraform
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

# Tools configuration

## RG (ripgrep)

    curl -LO https://github.com/BurntSushi/ripgrep/releases/download/11.0.2/ripgrep_11.0.2_amd64.deb
    sudo dpkg -i ripgrep_11.0.2_amd64.deb

## Ag

    sudo apt-get install silversearcher-ag

## PlantUML

    http://sourceforge.net/projects/plantuml/files/plantuml.jar

# Language Configuration

## Python

    sudo apt-get install python-jedi python-flake8 python-virtualenv
    # On Emacs M-X jedi:install-server

## Reveal.js

    wget https://github.com/hakimel/reveal.js/archive/<version>.tar.gz
    tar zxvf <version>.tar.gz
    mv reveal.js-<version> ~/work/pres/reveal.js

## GoLang

    sudo tar -C /usr/local -xzf go<version>.linux-amd64.tar.gz
    export PATH=$PATH:/usr/local/go/bin

    export GOPATH=$HOME/work/go
    export PATH=$PATH:$GOPATH/bin

    # install gocode, godef, go oracle
    (go-projectile-install-tools)

# Proxy

 * Launch Cask with a proxy (cask can't handle proxy user:password)

# Todo

 * Use use-package (in progress)

# License

Emacs-config is licensed under the [MIT License](http://www.opensource.org/licenses/mit-license.php).

# Inspired by

https://github.com/rakanalh/dotemacs
