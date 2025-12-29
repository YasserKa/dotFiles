XDG_CONFIG_HOME = $(HOME)/.config
XDG_DATA_HOME=$(HOME)/.local/share

# sci-hub for qutebrowser
PYPI_PACKAGES = tmuxp pdm sci-hub ipython pix2tex mutt-ics jupytext

.PHONY: install
install: update-sudoers pre-install-packages install-packages post-install-packages update-sudoers

.PHONY: pre-install-packages
pre-install-packages:
	@echo "Creating directories"
	@mkdir -p $(HOME)/bin
	@mkdir -p $(HOME)/.cache # For fasd
	@mkdir -p $(HOME)/Pictures
	@mkdir -p $(XDG_DATA_HOME)/applications
	@mkdir -p $(XDG_CONFIG_HOME)/systemd/user
	@mkdir -p $(XDG_CONFIG_HOME)/autorandr
	@mkdir -p $(XDG_CONFIG_HOME)/cmus
	@mkdir -p $(XDG_CONFIG_HOME)/rofi
	@mkdir -p $(XDG_CONFIG_HOME)/lnav
	@mkdir -p $(XDG_CONFIG_HOME)/sunshine
	@mkdir -p $(XDG_CONFIG_HOME)/copyq
	@mkdir -p $(XDG_CONFIG_HOME)/jupyter
	@mkdir -p $(XDG_CONFIG_HOME)/emacs
	@mkdir -p $(XDG_CONFIG_HOME)/i3
	@mkdir -p $(XDG_CONFIG_HOME)/projects
	@mkdir -p $(XDG_CONFIG_HOME)/tmux/plugins
	@mkdir -p $(XDG_CONFIG_HOME)/qutebrowser
	@mkdir -p $(XDG_CONFIG_HOME)/lnav/configs
	@mkdir -p $(XDG_CONFIG_HOME)/qutebrowser/userscripts
	@mkdir -p $(XDG_DATA_HOME)/qutebrowser
	@mkdir -p $(XDG_DATA_HOME)/qutebrowser/webengine
	@mkdir -p $(XDG_CONFIG_HOME)/Zotero
	@# Used by pai function
	@mkdir -p $(HOME)/.local/state/fzf
	@mkdir -p $(XDG_DATA_HOME)/okular
	@mkdir -p $(XDG_CONFIG_HOME)/autokey/data
	@mkdir -p $(XDG_CONFIG_HOME)/zsh
	@mkdir -p $(XDG_DATA_HOME)/icons/hicolor
	@rm -rf $(HOME)/.bashrc $(HOME)/.bash_profile
	sudo pacman --sync --refresh --sysupgrade --noconfirm stow

.PHONY: update-sudoers
update-sudoers:
	# Don't timeout sudo privilege through the installation period
	# Defaults        env_reset,timestamp_timeout=-1
	sudo pacman --sync --refresh --sysupgrade --noconfirm neovim
	sudo EDITOR=nvim visudo

.PHONY: install-packages
install-packages: create-clean-pkglist install-aur-helper
	@sudo pacman --sync --refresh --sysupgrade
	@paru --sync --refresh --noconfirm --sysupgrade --skipreview --needed - < pkglist_clean.tmp
	@yes | paru -S --skipreview evdi
	@# Get nvim preconfiguration before stowing
	@rm -f *tmp
	@# remove go packages installed by AUR packages
	@sudo rm $(HOME)/go -r
	# Install submodules
	@git submodule update --init --recursive

# create a file containing list of packages for package manager
.PHONY: clean-pkglist
create-clean-pkglist:
	@# pacman doesn\'t show the following installed packages
	@# Added xremap bcz there are one for each x11 and wayland
	@cat pkglist | grep -o "^[^#]*" | grep -vE "(binutils|fakeroot|gcc|gnupg|libtool|m4|make|msgpack-c|patch|pkgconf|sudo|texinfo|tree-sitter|which|xremap-x11-bin)" | sort | sed '1d' | tr -d "[:blank:]" >| pkglist_clean.tmp

.PHONY: stow-root
stow-root:
	@# Needs to be installed before stowing its config, else it will make an error
	@sudo pacman -S --noconfirm greetd
	sudo rm -r /etc/{pacman.conf,greetd}
	sudo mkdir /root/.config/nvim
	sudo stow root --target=/

.PHONY: install-aur-helper
install-aur-helper: stow-root
	rm -rf /tmp/paru
	git clone --depth 1 https://aur.archlinux.org/paru.git  /tmp/paru
	# Get multilib and archlinuxfr databases
	sudo pacman --sync --refresh
	cd /tmp/paru && makepkg --install --syncdeps --noconfir	cd /tmp/paru && makepkg --install --syncdeps --noconfirm
	rm -rf /tmp/paru

.PHONY: setup-tuir
setup-tuir:
	@git clone --depth 1 https://gitlab.com/YasserKa/tuir /tmp/tuir
	@# mailcap module (used by tuir) is removed at python 3.13
	@uv python install 3.12
	cd /tmp/tuir/ && uv tool install .
	cd .. && rm /tmp/tuir -rf

.PHONY: post-install-packages
post-install-packages: stow-packages install-pypi-packages setup-systemd-services setup-qutebrowser setup-tuir
	@# Needed for latex lsp in nvim
	@npm install -g tree-sitter-cli
	@# Setup editors
	nvim  --headless -c 'quitall'
	@# Setup Zsh plugin manager & shell
	@zsh <(curl -s https://raw.githubusercontent.com/zap-zsh/zap/master/install.zsh) --branch release-v1
	@sudo usermod --shell /bin/zsh $(USER)
	@# Accurate date
	@sudo timedatectl set-ntp true
	@# Sync pkgfile database for command-no-found-handler function to work
	@sudo pkgfile -u
	@# Setup Tmux plugin manager
	@git clone --depth 1 https://github.com/tmux-plugins/tpm $(XDG_CONFIG_HOME)/tmux/plugins/tpm
	@# Frecency
	@cargo install fre
	@# Install utility Enable mouse wrap between outpus with different resolutions
	@# The AUR package for it is outdated
	@git clone --depth 1 https://github.com/Airblader/xedgewarp  /tmp/xedgewarp
	@cd /tmp/xedgewarp
	@make
	@sudo make install

.PHONY: stow-packages
stow-packages:
	# Install neovim starter kit before stowing
	@stow X11 autokey autorandr bash bat cmus copyq dprint dunst emacs feh flake8 fzf geoclue git gnupg gtk i3 icons ignore ipython isync jupyter khard kitty latex lnav lsd mailcap mime_types mpv msmtp navi neomutt networkmanager_dmenu newsboat nextcloud notmuch npm nvim okular paru picom polybar python qutebrowser ranger readline rofi scripts shikane shellcheck sunshine sway sxhkd swhkd systemd terminal-colors thunderbird tmux tuir urlscan vimpagerrc wallpapers waybar xmodmap xremap yt-dlp zathura zsh

.PHONY:install-pypi-packages
install-pypi-packages: 
	@echo "Installing Python appliations"
	@python -m pip install hypothepy --break-system-packages # Getting hypothesis links
	@for PACKAGE in $(PYPI_PACKAGES); do uv tool install "$$PACKAGE"; done

.PHONY: setup-systemd-services
setup-systemd-services:
	systemctl enable cmus --user
	systemctl enable wireplumber.service --user
	systemctl enable udiskie.service --user
	systemctl enable dunst.service --user
	systemctl enable --now ydotool.service --user
	systemctl enable geoclue-agent.service --user
	systemctl enable msmtp-runqueue.timer --user
	systemctl enable mbsync.timer --user
	systemctl enable syncthing.service --user
	sudo systemctl enable pkgfile-update.timer
	sudo systemctl enable greetd.service
	sudo systemctl enable displaylink
	sudo systemctl enable auto-cpufreq.service
	sudo systemctl enable --now tailscale-bypass.service
	# Disables to not override backups while setting up the environment
	# Enable afterwards
	# sudo systemctl enable cronie
	sudo systemctl enable nftables.service
	sudo systemctl enable firewalld.service
	sudo systemctl enable bluetooth.service
	@# Weekly, discard unused blocks in filesystem
	sudo systemctl enable fstrim.timer

.PHONY: setup-qutebrowser
setup-qutebrowser:
	# Download dictionary
	/usr/share/qutebrowser/scripts/dictcli.py install en-US
	paru --sync --noconfirm --skipreview  chromium-widevine # viewing DRM content (Spotify)

## compare-packages: compare the current installed packages with the list
.PHONY: compare-packages
compare-packages: create-clean-pkglist
	@# Make expects a 0, otherwise it fails.
	@# diff returns 1 if a difference is found
	@#NOTE: To mark pkg as explicity installed: sudo pacman -D --asexplicit <pkg>
	@diff -y --suppress-common-lines --color pkglist_clean.tmp <(pacman -Qqe | grep -vE "(paru|evdi-git|evdi|evdi-compat-git|cmake|xremap-wlroots-bin|intel-media-driver|libva-utils|mesa-utils|nvidia|vulkan-intel|amd-ucode|vulkan-radeon)" | sort) || echo ""
	@rm -f *tmp

.PHONY: help
help : Makefile
	@sed -n 's/^##//p' $<
