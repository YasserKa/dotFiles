XDG_CONFIG_HOME = $(HOME)/.config
XDG_DATA_HOME=$(HOME)/.local/share

# i3ipc: autokey for emacs script
# MyMuPDF: getting highlighted text in pdf
# pyperclip: used to yank to clipboard by zathura's handle_document
# adblock tldextract sci-hub: qutebrowser 
PYPI_PACKAGES = i3ipc PyMuPDF tmuxp pyperclip jupyter_contrib_nbextensions poetry notebook neovim-remote selenium flake8 black isort autoimport adblock tldextract sci-hub ipython pip

.PHONY: install
install: pre-install-packages update-sudoers install-packages post-install-packages update-sudoers show-final-instructions-message 

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
	@rm -rf $(HOME)/.bashrc $(HOME)/.bash_profile
	@# https://wiki.archlinux.org/title/GRUB/Tips_and_tricks#Hide_GRUB_unless_the_Shift_key_is_held_down
	@sudo echo 'GRUB_FORCE_HIDDEN_MENU="true"' >| /boot/grub/grub.cfg
	@sudo grub-mkconfig -o /boot/grub/grub.cfg
	@# Install stow and neovim
	sudo pacman --sync --refresh --sysupgrade --noconfirm stow neovim

.PHONY: update-sudoers
update-sudoers:
	# Don't timeout sudo privilege through the installation period
	# Defaults        env_reset,timestamp_timeout=0
	sudo EDITOR=neovim visudo

.PHONY: install-packages
install-packages: creat-clean-pkglist install-aur-helper
	@sudo paru --sync --refresh --sysupgrade --noconfirm --skipreview --needed - < pkglist_clean.tmp
	@rm -f *tmp

# create a file containing list of packages for package manager
.PHONY: clean-pkglist
create-clean-pkglist:
	@cat pkglist | grep -o "^[^#]*" | sort | sed '1d' | tr -d "[:blank:]" >| pkglist_clean.tmp

.PHONY: stow-etc
stow-etc:
	sudo rm /etc/pacman.conf
	sudo stow etc --target=/

.PHONY: install-aur-helper
install-aur-helper: stow-etc
	rm -rf /tmp/paru
	git clone https://aur.archlinux.org/paru.git  /tmp/paru
	cd /tmp/paru && makepkg --install --syncdeps --noconfir	cd /tmp/paru && makepkg --install --syncdeps --noconfirm
	rm -rf /tmp/paru

.PHONY: post-install-packages
post-install-packages: stow-packages setup-systemd-services setup-jupyter-notebook setup-qutebrowser setup-ambient-music 
	# Setup neovim
	git clone --depth 1 https://github.com/AstroNvim/AstroNvim ~/.config/nvim
	nvim  --headless -c 'autocmd User LazyDone quitall'
	# Setup Emacs
	emacs --batch --load=$(XDG_CONFIG_HOME)/emacs/init.el
	@# Accurate date
	sudo timedatectl set-ntp true
	@# Setup autokey (don't show tray icon)
	sed -i '/.*"showTrayIcon": false,/c\"showTrayIcon": true,' "$(HOME)/.config/autokey/autokey.json"
	@# Sync pkgfile database for command-no-found-handler function to work
	sudo pkgfile -u
	@echo "Upgrading PYPI packages"
	@pip install --upgrade --user  $(PYPI_PACKAGES)
	@# Setup Tmux plugin manager
	git clone https://github.com/tmux-plugins/tpm $(XDG_CONFIG_HOME)/tmux/plugins/tpm
	@#Setup notes
	git clone https://github.com/YasserKa/notes $(HOME)/notes
	@# Setup ambient music
	yt-dlp -x -o "$(HOME)/Music/ambient_music.%(ext)s" https://www.youtube.com/watch?v=6uVUv8gZHBE


.PHONY: setup-systemd-services
setup-systemd-services:
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

.PHONY: stow-packages
stow-packages:
	stow abook alacritty autorandr autokey bash bat cmus cron dunst emacs fasd flake8 fzf feh git gnupg gtk i3 icons ipython isync jupyter khard kitty latex lnav lsd mailcap mime_types mpv msmtp neomutt networkmanager_dmenu newsboat notmuch npm nvim okular picom polybar qutebrowser ranger readline rofi scripts ssh sxhkd systemd tmux tuir vimpagerrc wallpapers X11 xmodmap zathura

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
	git submodule update --init --recursive
	cd "$(XDG_CONFIG_HOME)/qutebrowser/" && pip install -e . --user
	python $(XDG_CONFIG_HOME)/qutebrowser/userscripts/yank_all --install --bin=yank_all
	# Download dictionary
	/usr/share/qutebrowser/scripts/dictcli.py install en-US
	paru -S chromium-widevine # viewing DRM content (Spotify)
	paru -S qtwebkit-plugins-git # For SpellChecking
	# Update adblock list
	qutebrowser :adblock-update
	pkill qutebrowser

.PHONY: show-final-instructions-message
show-final-instructions-message:
	@echo "Actions that needs to be done manually:"
	@echo "- Open okular to import keybinding scheme"

## compare-packages: compare the current installed packages with the list
.PHONY: compare-packages
compare-packages: create-clean-pkglist
	@# Make expects a 0, otherwise it fails.
	@# diff returns 1 if a difference is found
	@diff -y --suppress-common-lines --color pkglist_clean.tmp <(pacman -Qqe | grep -vE paru | sort); [ $$? -eq 1 ]
	@rm -f *tmp

.PHONY: help
help : Makefile
	@sed -n 's/^##//p' $<
