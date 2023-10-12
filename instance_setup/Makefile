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

bash: 
	@echo "Setting up bash"
	@[[ ! -f ~/.bashrc ]] || ! grep -qs yasser ~/.bashrc && \
		echo -e '\n# Yasser'\''s Config\n[[ -e ~/.yasser_rc/.bashrc && $$yasser_config_env ]] && source ~/.yasser_rc/.bashrc' >> ~/.bashrc || echo "Bash already configured"

clipboard: 
	@echo "Setting up clipboard sharing"
	@yes | sudo apt install xclip
	@echo -e "Need following /etc/ssh/sshd_config settings
X11Forwarding yes
X11DisplayOffset 10
X11UseLocalhost yes

Use ssh -CY
	"

vim:
	@echo "Setting up vim"
	@curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
		https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim > /dev/null 2>&1 
	@vim -u ~/.yasser_rc/.vimrc +'PlugInstall --sync' +qa > /dev/null 2>&1 

jupyter:
	@echo "Setting up jupyter"
	@sudo apt install jupyter
	@yes | sudo apt-get install python3-pip
	@pip install jupyternotify
	@yes | jupyter nbextensions_configurator enable --user
