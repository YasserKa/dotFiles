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
	@# Allow vetical and block cursor bars
	@command ssh -t $(host) "sed -i '/set show-mode-in-prompt/s/^#//; /set vi-ins/s/^#//; /set vi-cmd/s/^#//' $(config_path)/readline/inputrc"
	@ssh -t "$(host)" "cd $(config_path) && make --keep-going"
	@echo -e "Notes: Jupyter notebook can be setup via Makefile"

vim_root:
	@su - root -c 'curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
	@sudo cp ~/.yasser_rc/.vimrc /root/
	@su - root -c 'vim +PlugInstall +qall'

vim:
	@echo "Setting up vim"
	@sudo apt install vim
	curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    	https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	@vim +PlugInstall +qall

bash: 
	@echo "Setting up bash"
	@sudo apt install tmux
	@[[ ! -f ~/.bashrc ]] || ! grep -qs yasser ~/.bashrc && \
		echo -e '\n# Yasser'\''s Config\n[[ -e ~/.yasser_rc/.bashrc && $$yasser_config_env ]] && source ~/.yasser_rc/.bashrc' >> ~/.bashrc || echo "Bash already configured"
