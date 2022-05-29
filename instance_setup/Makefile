.PHONY: update-packages bash

SHELL=/bin/bash
config_path=~/.yasser_rc

all: check_make bash vim

check_make:
	@command -v make >/dev/null || { echo "need to install GNU make" && exit 1; }

move_config_to_server:
	@# Expand files starting with a dot
	[[ -n "$(host)" ]] || { echo 'need a host name' && exit 1; }
	@ssh -t "$(host)" "mkdir -p $(config_path)"
	shopt -s dotglob; scp  -qr "$(HOME)"/dotfiles/instance_setup/* "$(host):$(config_path)"
	scp  -qr "$(HOME)"/dotfiles/readline/.config/readline "$(host):$(config_path)"
	@# Setup bash
	echo -e "\nNotes: Jupyter notebook can be setup via Makefile"
	@ssh -t "$(host)" "cd $(config_path) && make --keep-going"
	echo -e "\nNotes: Jupyter notebook can be setup via Makefile"

bash: 
	@echo "Setting up bash"
	@[[ ! -f ~/.bashrc ]] || ! grep -qs yasser ~/.bashrc || { echo "Bash already configured" && exit 1; }
	@echo -e '\n# Yasser'\''s Config\n[[ -e ~/.yasser_rc/.bashrc && $$yasser_config_env ]] && source ~/.yasser_rc/.bashrc' >> ~/.bashrc

vim:
	@curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
		https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	@vim -u ~/.yasser_rc/.vimrc +'PlugInstall --sync' +qa

jupyter:
	@sudo apt install jupyter
	@yes | sudo apt-get install python3-pip
	@pip install jupyternotify
	@yes | jupyter nbextensions_configurator enable --user
