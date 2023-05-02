XDG_CONFIG_HOME = $(HOME)/.config
XDG_DATA_HOME=$(HOME)/.local/share

# i3ipc: autokey for emacs script
# MyMuPDF: getting highlighted text in pdf
# pyperclip: used to yank to clipboard by zathura's handle_document
# adblock tldextract sci-hub: qutebrowser 
# jupyter-ascending require notebook library
PYPI_PACKAGES_PIP = i3ipc PyMuPDF pyperclip neovim-remote selenium adblock tldextract pip pipx jupyter-ascending notebook
# jupyter notebook related packages are injected in the jupyter rule
PYPI_PACKAGES_PIPX = tmuxp poetry pdm notebook ruff black isort autoimport sci-hub ipython

.PHONY: install
install: pre-install-packages update-sudoers install-packages post-install-packages update-sudoers

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
	@sudo sh -c "echo 'GRUB_FORCE_HIDDEN_MENU="true"' >> /boot/grub/grub.cfg"
	@sudo curl https://gist.githubusercontent.com/anonymous/8eb2019db2e278ba99be/raw/257f15100fd46aeeb8e33a7629b209d0a14b9975/gistfile1.sh --output /etc/grub.d/31_hold_shift 
	@sudo chmod +x /etc/grub.d/31_hold_shift
	@sudo grub-mkconfig -o /boot/grub/grub.cfg
	@# Install stow and neovim
	sudo pacman --sync --refresh --sysupgrade --noconfirm stow neovim

.PHONY: update-sudoers
update-sudoers:
	# Don't timeout sudo privilege through the installation period
	# Defaults        env_reset,timestamp_timeout=-1
	sudo EDITOR=nvim visudo

.PHONY: install-packages
install-packages: create-clean-pkglist install-aur-helper setup-tuir
	@sudo pacman --sync --refresh --upgrade
	@paru --sync --refresh --sysupgrade --noconfirm --skipreview --needed - < pkglist_clean.tmp
	@# Get nvim preconfiguration before stowing
	@git clone --depth 1 https://github.com/AstroNvim/AstroNvim $(XDG_CONFIG_HOME)/nvim
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
	git clone --depth 1 https://aur.archlinux.org/paru.git  /tmp/paru
	# Get multilib and archlinuxfr databases
	sudo pacman --sync --refresh
	cd /tmp/paru && makepkg --install --syncdeps --noconfir	cd /tmp/paru && makepkg --install --syncdeps --noconfirm
	rm -rf /tmp/paru

.PHONY: setup-tuir
setup-tuir:
	@git clone --depth 1 https://gitlab.com/YasserKa/tuir /tmp/tuir
	@cd /tmp/tuir && pip install . && cd .. && rm /tmp/tuir -rf

.PHONY: post-install-packages
post-install-packages: stow-packages install-pypi-packages setup-systemd-services setup-jupyter-notebook setup-qutebrowser
	@# Setup neovim
	nvim  --headless -c 'autocmd User LazyDone quitall'
	@# Accurate date
	sudo timedatectl set-ntp true
	@# Sync pkgfile database for command-no-found-handler function to work
	sudo pkgfile -u
	@# Setup Tmux plugin manager
	git clone --depth 1 https://github.com/tmux-plugins/tpm $(XDG_CONFIG_HOME)/tmux/plugins/tpm
	@#Setup notes
	git clone https://github.com/YasserKa/notes $(HOME)/notes
	@# Setup ambient music
	yt-dlp -x -o "$(HOME)/Music/ambient_music.%(ext)s" https://www.youtube.com/watch?v=6uVUv8gZHBE

.PHONY: stow-packages
stow-packages:
	stow abook alacritty autorandr autokey bash bat cmus cron dunst emacs fasd flake8 fzf feh git gnupg gtk hunspell i3 icons ipython isync jupyter khard kitty latex lnav lsd mailcap mime_types mpv msmtp neomutt networkmanager_dmenu newsboat notmuch npm nvim okular picom dprint polybar qutebrowser ranger readline rofi scripts ssh sxhkd systemd tmux tuir vimpagerrc wallpapers X11 xmodmap zathura

.PHONY:install-pypi-packages
install-pypi-packages: 
	@echo "Installing PYPI packages"
	@pip install --user  $(PYPI_PACKAGES_PIP)
	@for PACKAGE in $(PYPI_PACKAGES_PIPX); do pipx install "$$PACKAGE"

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

# Setup vim_binding jupyter extension
EXT_DIR=$(HOME)/.local/share/jupyter/nbextension/vim_binding
export PATH := $(PATH):$(HOME)/.local/bin
.PHONY: setup-jupyter-notebook
setup-jupyter-notebook:
	@echo "Setting up jupyter notebook"
	@echo "Setting up jupyter ascending"
	@# jupyter-ascending need notebook library
	@pip install notebook jupyter-ascending --user
	@pipx install notebook
	@pipx inject notebook jupytext 
	@pipx inject notebook jupyter_contrib_nbextensions
	@jupyter nbextension install jupytext --py --user
	@jupyter nbextension enable jupytext --py --user
	@jupyter nbextension install jupyter_ascending --py --user
	@jupyter nbextension enable jupyter_ascending --py --user
	@jupyter serverextension enable jupyter_ascending --py --user
	@echo "Setting up vim_binding extension"
	@git clone --depth 1 https://github.com/lambdalisue/jupyter-vim-binding $(EXT_DIR)
	@chmod -R go-w $(EXT_DIR)
	@jupyter nbextension enable vim_binding/vim_binding

.PHONY: setup-qutebrowser
setup-qutebrowser:
	git submodule update --init --recursive
	cd $(XDG_CONFIG_HOME)/qutebrowser/qutescript && pip install -e . --user
	python $(XDG_CONFIG_HOME)/qutebrowser/userscripts/yank_all --install --bin=yank_all
	# Download dictionary
	/usr/share/qutebrowser/scripts/dictcli.py install en-US
	paru --sync --noconfirm --skipreview  chromium-widevine # viewing DRM content (Spotify)
	paru --sync --noconfirm --skipreview qtwebkit-plugins-git # For SpellChecking
	# Update adblock list
	qutebrowser :adblock-update
	pkill qutebrowser

## compare-packages: compare the current installed packages with the list
.PHONY: compare-packages
compare-packages: create-clean-pkglist
	@# Make expects a 0, otherwise it fails.
	@# diff returns 1 if a difference is found
	@diff -y --suppress-common-lines --color pkglist_clean.tmp <(pacman -Qqe | grep -vE paru | sort) || echo ""
	@rm -f *tmp

.PHONY: help
help : Makefile
	@sed -n 's/^##//p' $<
