XDG_CONFIG_HOME = $(HOME)/.config
XDG_DATA_HOME=$(HOME)/.local/share

# sci-hub for qutebrowser
PYPI_PACKAGES_PIPX = tmuxp pdm sci-hub ipython

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
	@mkdir -p $(XDG_CONFIG_HOME)/lnav/configs
	@mkdir -p $(XDG_DATA_HOME)/qutebrowser
	@mkdir -p $(XDG_DATA_HOME)/okular
	@mkdir -p $(XDG_DATA_HOME)/qutebrowser/webengine
	@mkdir -p $(XDG_CONFIG_HOME)/autokey
	@mkdir -p $(XDG_CONFIG_HOME)/zsh
	@mkdir -p $(HOME)/.ssh
	@chmod 700 $(HOME)/.ssh
	@mkdir -p $(XDG_DATA_HOME)/icons/hicolor
	@mkdir -p $(XDG_DATA_HOME)/.gnugp
	@chmod 700 $(XDG_DATA_HOME)/.gnugp
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
install-packages: create-clean-pkglist install-aur-helper
	@sudo pacman --sync --refresh --sysupgrade
	@paru --sync --refresh --sysupgrade --noconfirm --skipreview --needed - < pkglist_clean.tmp
	# Need to be installed afterwrds because displaylink depend son evdi which
	# conflicts with evdi-git
	@paru --sync evdi-git
	@# Get nvim preconfiguration before stowing
	@git clone --depth 1 https://github.com/AstroNvim/AstroNvim $(XDG_CONFIG_HOME)/nvim
	@rm -f *tmp
	# Install submodules
	@git clone  https://github.com/YasserKa/dotfiles-private ~/.dotfiles-private
	@git sbmodule update --init --recursive

# create a file containing list of packages for package manager
.PHONY: clean-pkglist
create-clean-pkglist:
	@# pacman doesn\'t show the following installed packages
	@cat pkglist | grep -o "^[^#]*" | grep -vE "(binutils|fakeroot|gcc|gnupg|libtool|m4|make|msgpack-c|patch|pkgconf|sudo|texinfo|tree-sitter|which)" | sort | sed '1d' | tr -d "[:blank:]" >| pkglist_clean.tmp

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
post-install-packages: stow-packages install-pypi-packages setup-systemd-services setup-qutebrowser
	@# Setup neovim
	nvim  --headless -c 'autocmd User LazyDone quitall'
	@# Setup Zsh plugin manager & shell
	@zsh <(curl -s https://raw.githubusercontent.com/zap-zsh/zap/master/install.zsh) --branch release-v1
	@sudo usermod --shell /bin/zsh yasser
	@# Accurate date
	sudo timedatectl set-ntp true
	@# Sync pkgfile database for command-no-found-handler function to work
	sudo pkgfile -u
	@# Setup Tmux plugin manager
	git clone --depth 1 https://github.com/tmux-plugins/tpm $(XDG_CONFIG_HOME)/tmux/plugins/tpm
	@#Setup notes
	git clone https://github.com/YasserKa/notes $(HOME)/notes
	git -C $(HOME)/notes config --bool branch.master.sync true
	git -C $(HOME)/notes config --bool branch.master.syncNewFiles true
	git -C $(HOME)/notes config --local commit.gpgsign false
	@# Setup ambient music
	yt-dlp -x -o "$(HOME)/Music/ambient_music.%(ext)s" https://www.youtube.com/watch?v=6uVUv8gZHBE

.PHONY: stow-packages
stow-packages:
	X11 abook alacritty autokey autorandr bash bat cmus cron dprint dunst emacs fasd feh flake8 fzf geoclue git gnupg gtk hunspell i3 icons ipython isync jupyter khard kitty latex lnav lsd mailcap mime_types mpv msmtp neomutt networkmanager_dmenu newsboat notmuch npm nvim okular paru picom polybar qutebrowser ranger readline rofi scripts shikane ssh stow sway sxhkd systemd tmux tuir urlscan vimpagerrc wallpapers waybar wezterm xmodmap yt-dlp zathura zsh
	stow --target=$HOME --dir="$HOME/.dotfiles-private" qutebrowser bash zsh hunspell khard snippets
	git -C $(HOME)/.dotfiles-private config --bool branch.main.sync true
	git -C $(HOME)/.dotfiles-private config --bool branch.main.syncNewFiles true
	git -C $(HOME)/.dotfiles-private config --local commit.gpgsign false
	sudo stow root --target=/root/

.PHONY:install-pypi-packages
install-pypi-packages: 
	@echo "Installing Python appliations"
	@for PACKAGE in $(PYPI_PACKAGES_PIPX); do pipx install "$$PACKAGE"; done

.PHONY: setup-systemd-services
setup-systemd-services:
	systemctl enable --now cmus --user
	systemctl enable --now notify-me.timer --user
	systemctl enable --now udiskie.service --user
	systemctl enable --now dunst.service --user
	systemctl enable --now sxhkd.service --user
	systemctl enable --now geoclue-agent.service --user
	systemctl enable --now msmtp-runqueue.timer --user
	systemctl enable --now mbsync.timer --user
	systemctl enable --now snapd.socket
	sudo systemctl enable --now displaylink
	sudo systemctl enable --now cronie
	sudo systemctl enable --now nftables.service
	sudo systemctl enable --now acpid.service
	sudo systemctl enable --now firewalld.service
	sudo systemctl enable --now bluetooth.service
	sudo systemctl enable --now networkmanager.service
	sudo systemctl enable --now systemd-resolved.service
	@# Weekly, discard unused blocks in filesystem
	sudo systemctl enable --now fstrim.timer
	systemctl --user daemon-reload
	systemctl daemon-reload

.PHONY: setup-qutebrowser
setup-qutebrowser:
	cd $(XDG_CONFIG_HOME)/qutebrowser/qutescript && pip install -e . --user
	python $(XDG_CONFIG_HOME)/qutebrowser/userscripts/yank_all --install --bin=yank_all
	# Download dictionary
	/usr/share/qutebrowser/scripts/dictcli.py install en-US
	paru --sync --noconfirm --skipreview  chromium-widevine # viewing DRM content (Spotify)
	# Update adblock list
	qutebrowser :adblock-update
	pkill qutebrowser

## compare-packages: compare the current installed packages with the list
.PHONY: compare-packages
compare-packages: create-clean-pkglist
	@# Make expects a 0, otherwise it fails.
	@# diff returns 1 if a difference is found
	@diff -y --suppress-common-lines --color pkglist_clean.tmp <(pacman -Qqe | grep -vE "(paru|evdi-git)" | sort) || echo ""
	@rm -f *tmp

.PHONY: help
help : Makefile
	@sed -n 's/^##//p' $<
