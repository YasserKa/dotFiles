.PHONY: update-packages bash

SHELL=/bin/bash
config_path=~/.yasser_rc

all: bash vim

move_config_to_server:
	@# Expand files starting with a dot
	@[[ -n "$(host)" ]] || { echo 'need a host name' && exit 1; }
	@command -v make >/dev/null || { echo "GNU make is needed at '$(host)' server" && exit 1; }
	@ssh -t "$(host)" "mkdir -p $(config_path)"
	@shopt -s dotglob; scp  -qr "$(DOTFILES_DIR)"/instance_setup/* "$(host):$(config_path)"
	@scp  -qr "$(DOTFILES_DIR)"/readline/.config/readline "$(host):$(config_path)"
	@ssh -t "$(host)" "cd $(config_path) && make --keep-going"
	@echo -e "Notes: Jupyter notebook can be setup via Makefile"

vim:
	@echo "Setting up vim"
	@sudo apt install vim
	curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    	https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	@vim +PlugInstall +qall

bash: 
	@echo "Setting up bash"
	@[[ ! -f ~/.bashrc ]] || ! grep -qs yasser ~/.bashrc && \
		echo -e '\n# Yasser'\''s Config\n[[ -e ~/.yasser_rc/.bashrc && $$yasser_config_env ]] && source ~/.yasser_rc/.bashrc' >> ~/.bashrc || echo "Bash already configured"

clipboard: 
	@echo "Setting up clipboard sharing"
	@yes | sudo apt install xclip
	@echo -e "Need following /etc/ssh/sshd_config settings


jupyter:
	@echo "Setting up jupyter"
	@sudo apt install jupyter
	@yes | sudo apt-get install python3-pip
	@pip install jupyternotify
	@yes | jupyter nbextensions_configurator enable --user
