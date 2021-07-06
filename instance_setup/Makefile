.PHONY: update-packages bash

all: update-packages config vim

update-packages:
	sudo apt update

config:
	yes | cp .bashrc ~/.bashrc
	yes | cp inputrc ~/.inputrc

vim:
	curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    		https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	cp .vimrc ~/.vimrc
	vim +'PlugInstall --sync' +qa

jupyter:
	sudo apt install jupyter
	yes | sudo apt-get install python3-pip
	pip install jupyternotify
	yes | jupyter nbextensions_configurator enable --user
# jupyer vim bindings
# jupyer vimtext
