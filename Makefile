XDG_CONFIG_HOME = $(HOME)/.config
XDG_DATA_HOME=$(HOME)/.local/share

# i3ipc: autokey for emacs script
# MyMuPDF: getting highlighted text in pdf
# pyperclip: used to yank to clipboard by zathura's handle_document
# adblock tldextract sci-hub: qutebrowser 
PYPI_PACKAGES = i3ipc PyMuPDF tmuxp pyperclip jupyter_contrib_nbextensions poetry notebook neovim-remote selenium flake8 black isort autoimport adblock tldextract sci-hub ipython pip

.PHONY: install
install: post-install-packages upgrade-pypi-packages setup-knowledge-base setup-neovim setup-jupyter-notebook setup-qutebrowser setup-bash firewalld setup-ambient-music show-final-instructions-message

.PHONY: stow-etc
stow-etc:
	sudo stow etc --target=/

.PHONY: install-aur-helper
install-aur-helper: stow-etc
	rm -rf /tmp/paru
	git clone https://aur.archlinux.org/paru.git  /tmp/paru
	cd /tmp/paru && makepkg --install --syncdeps --noconfirm
	rm -rf /tmp/baph

.PHONY: pre-install-packages
pre-install-packages:
	@echo "Creating directories"
	@mkdir -p $(HOME)/Pictures
	@mkdir -p $(XDG_DATA_HOME)/applications
	@mkdir -p $(XDG_CONFIG_HOME)/systemd/user
	@mkdir -p $(XDG_CONFIG_HOME)/cmus
	@mkdir -p $(XDG_CONFIG_HOME)/jupyter
	@mkdir -p $(XDG_CONFIG_HOME)/emacs
	@mkdir -p $(XDG_CONFIG_HOME)/nvim
	@mkdir -p $(XDG_CONFIG_HOME)/qutebrowser
	rm $(HOME)/.bashrc $(HOME)/.bash_profile
	sudo pacman --sync --refresh --sysupgrade --noconfirm stow

.PHONY: stow-packages
stow-packages:
	stow alacritty autorandr autokey bash bat cmus cron dunst emacs fasd flake8 fzf feh git gnupg gtk i3 icons isync jupyter khard latex lnav lsd mailcap mpv msmtp neomutt networkmanager_dmenu newsboat notmuch npm nvim okular picom polybar qutebrowser ranger readline rofi scripts shell_common ssh sxhkd systemd tmux tuir vimpagerrc wallpapers X11 xdg-open xmodmap zathura

.PHONY: post-install-packages
post-install-packages: stow-packages
	# accurate date
	sudo timedatectl set-ntp true
	emacs --batch --load=$(XDG_CONFIG_HOME)/emacs/init.el
	nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync' -c 'MasonToolsInstall'
	# Display link
	systemctl start displaylink
	systemctl enable displaylink
	# Cmus
	systemctl start cmus --user
	systemctl enable cmus --user
	systemctl enable notify-me.timer --user
	systemctl start notify-me.timer --user
	systemctl enable udiskie.service --user
	systemctl start udiskie.service --user
	systemctl enable dunst.service --user
	systemctl start dunst --user
	systemctl enable sxhkd.service --user
	systemctl start sxhkd.service --user
	systemctl enable msmtp-runqueue.timer --user
	systemctl start msmtp-runqueue.timer --user
	systemctl enable mbsync.timer --user
	systemctl start mbsync.timer --user
	sudo systemctl enable nftables.service
	sudo systemctl start nftables.service
	sudo systemctl enable acpid.service
	sudo systemctl start acpid.service
	sudo systemctl enable firewalld.service
	sudo systemctl start firewalld.service
	@# Weekly, discard unused blocks in filesystem
	sudo systemctl enable fstrim.timer
	systemctl --user daemon-reload
	systemctl daemon-reload

.PHONY: setup-bash
setup-bash:
	@# Sync pkgfile database for command-no-found-handler function to work
	 sudo pkgfile -u

.PHONY: setup-tmux
setup-tmux:
	@# Setup Tmux plugin manager
	 git clone https://github.com/tmux-plugins/tpm $(XDG_CONFIG_HOME)/tmux/plugins/tpm

.PHONY: setup-neovim
setup-neovim:
	# Get plugings
	git clone https://github.com/AstroNvim/AstroNvim ~/.config/nvim
	nvim +PackerSync
	nvim -c 'TSInstall python bash vim latex lua'

.PHONY: setup-neovim-old
setup-neovim-old:
	# Get plugings
	curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
		https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	nvim -c 'TSInstall python bash vim latex lua'
	# install coc extensions
	nvim -c 'CocInstall -sync coc-snippets coc-db coc-explorer coc-json  coc-pyright coc-emmet coc-html coc-vimtex|qall'

.PHONY: upgrade-pypi-packages
upgrade-pypi-packages:
	@echo "Upgrading PYPI packages"
	@pip install --upgrade --user  $(PYPI_PACKAGES)


.PHONY: setup-knowledge-base
setup-knowledge-base:
	cd $(HOME)
	git pull https://github.com/YasserKa/notes

.PHONY: setup-jupyter-notebook
setup-jupyter-notebook:
	jupyter nbextensions_configurator enable --user
	# You may need the following to create the directoy
	mkdir -p $(jupyter --data-dir)/nbextensions
	# Now clone the repository
	cd "$(jupyter --data-dir)/nbextensions"
	git clone https://github.com/lambdalisue/jupyter-vim-binding vim_binding
	chmod -R go-w vim_binding
	jupyter nbextension enable vim_binding/vim_binding

.PHONY: setup-qutebrowser
setup-qutebrowser:
	cd "$(XDG_CONFIG_HOME)/qutebrowser/qutescript" && pip install -e . --user
	python $(XDG_CONFIG_HOME)/qutebrowser/userscripts/yank_all.py --install --bin=yank_all.py
	# Download dictionary
	/usr/share/qutebrowser/scripts/dictcli.py install en-US
	paru -S chromium-widevine # viewing DRM content (Spotify)
	paru -S qtwebkit-plugins-git # For SpellChecking
	# Update adblock list
	qutebrowser :adblock-update
	pkill qutebrowser

.PHONY: setup-ambient-music
setup-ambient-music:
	yt-dlp -x -o "$(HOME)/Music/ambient_music.%(ext)s" https://www.youtube.com/watch?v=6uVUv8gZHBE

.PHONY: show-final-instructions-message
show-final-instructions-message:
	@echo "Actions that needs to be done manually:"
	@echo "- Open okular to import keybinding scheme"

.PHONY: make-clean-pkglist
make-clean-pkglist:
	@cat pkglist | grep -o "^[^#]*" | sort | sed '1d' | tr -d "[:blank:]" >| pkglist_clean.tmp

## compare-packages: compare the current installed packages with the list
.PHONY: compare-packages
compare-packages: make-clean-pkglist
	@# Make expects a 0, otherwise it fails.
	@# diff returns 1 if a difference is found
	@diff -y --suppress-common-lines --color pkglist_clean.tmp <(pacman -Qqe | grep -vE paru | sort); [ $$? -eq 1 ]
	@rm -f *tmp

.PHONY: install-packages
install-packages: make-clean-pkglist install-aur-helper
	@sudo paru --sync --refresh --sysupgrade --noconfirm --needed - < pkglist_clean.tmp
	@rm -f *tmp

.PHONY: help
help : Makefile
	@sed -n 's/^##//p' $<
