.PHONY: update-packages bash

all: check_make bash vim

check_make:
	@command -v make >/dev/null || { echo "need to install GNU make" && exit 1; }

bash: 
	@echo "Setting up bash"
	@# Add line to ~/.bashrc that sources personal bashrc if it doesn't exit
	@[ ! -f ~/.bashrc ] || ! grep -qs yasser ~/.bashrc 
	@echo '\n# Yasser'\''s Config\n[[ -e ~/.yasser_rc/.bashrc && \$yasser_config_env ]] && source ~/.yasser_rc/.bashrc' >> ~/.bashrc

vim:
	@echo "Installing vim plugin manager & plugins"
	@curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
		https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	@vim -u ~/.yasser_rc/.vimrc +'PlugInstall --sync' +qa

jupyter:
	@sudo apt install jupyter
	@yes | sudo apt-get install python3-pip
	@pip install jupyternotify
	@yes | jupyter nbextensions_configurator enable --user
