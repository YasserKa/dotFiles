XDG_CONFIG_HOME = $(HOME)/.config
XDG_DATA_HOME=$(HOME)/.local/share

# sci-hub for qutebrowser
PYPI_PACKAGES = tmuxp pdm sci-hub ipython pix2tex mutt-ics

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
	@mkdir -p $(XDG_CONFIG_HOME)/lnav
	@mkdir -p $(XDG_CONFIG_HOME)/copyq
	@mkdir -p $(XDG_CONFIG_HOME)/jupyter
	@mkdir -p $(XDG_CONFIG_HOME)/emacs
	@mkdir -p $(XDG_CONFIG_HOME)/tmux/plugins
	@mkdir -p $(XDG_CONFIG_HOME)/nvim
	@mkdir -p $(XDG_CONFIG_HOME)/qutebrowser
	@mkdir -p $(XDG_CONFIG_HOME)/lnav/configs
	@mkdir -p $(XDG_DATA_HOME)/qutebrowser
	@# Used by pai function
	@mkdir -p $(HOME)/.local/state/fzf
	@mkdir -p $(XDG_DATA_HOME)/okular
	@mkdir -p $(XDG_DATA_HOME)/qutebrowser/webengine
	@mkdir -p $(XDG_CONFIG_HOME)/autokey/data
	@mkdir -p $(XDG_CONFIG_HOME)/zsh
	@mkdir -p $(XDG_DATA_HOME)/icons/hicolor
	@rm -rf $(HOME)/.bashrc $(HOME)/.bash_profile
	@# https://wiki.archlinux.org/title/GRUB/Tips_and_tricks#Hide_GRUB_unless_the_Shift_key_is_held_down
	@sudo sh -c "echo 'GRUB_FORCE_HIDDEN_MENU="true"' >> /boot/grub/grub.cfg"
	@sudo curl https://gist.githubusercontent.com/anonymous/8eb2019db2e278ba99be/raw/257f15100fd46aeeb8e33a7629b209d0a14b9975/gistfile1.sh --output /etc/grub.d/31_hold_shift 
	@sudo chmod +x /etc/grub.d/31_hold_shift
	@sudo grub-mkconfig -o /boot/grub/grub.cfg
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
	@yes | paru -S --skipreview evdi-compat-git
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
	@cat pkglist | grep -o "^[^#]*" | grep -vE "(binutils|fakeroot|gcc|gnupg|libtool|m4|make|msgpack-c|patch|pkgconf|sudo|texinfo|tree-sitter|which|wlroots)" | sort | sed '1d' | tr -d "[:blank:]" >| pkglist_clean.tmp

.PHONY: stow-etc
stow-etc:
	@# Needs to be installed before stowing its config, else it will make an error
	@sudo pacman -S --noconfirm greetd
	sudo rm -r /etc/{pacman.conf,greetd}
	sudo stow etc --target=/
	@# pkgfile-update needs specific file permission to work
	yes | sudo cp /etc/etc/pacman.conf /etc

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
  @uv python install 3.12 # mailcap module (used by tuir) is removed at python 3.13
  @cd /tmp/tuir/ && uv tool install .
	@cd .. && rm /tmp/tuir -rf

.PHONY: post-install-packages
post-install-packages: stow-packages install-pypi-packages setup-systemd-services setup-qutebrowser setup-tuir
	@# Setup editors
	nvim  --headless -c 'quitall'
	emacs --kill
	@# Setup Zsh plugin manager & shell
	@zsh <(curl -s https://raw.githubusercontent.com/zap-zsh/zap/master/install.zsh) --branch release-v1
	@sudo usermod --shell /bin/zsh $(USER)
	@# Accurate date
	@sudo timedatectl set-ntp true
	@# Sync pkgfile database for command-no-found-handler function to work
	@sudo pkgfile -u
	@# Setup Tmux plugin manager
	@git clone --depth 1 https://github.com/tmux-plugins/tpm $(XDG_CONFIG_HOME)/tmux/plugins/tpm

.PHONY: stow-packages
stow-packages:
	# Install neovim starter kit before stowing
	@git clone --depth 1 https://github.com/AstroNvim/AstroNvim $(XDG_CONFIG_HOME)/nvim
	@stow X11 autokey autorandr bash bat cmus copyq cron dprint dunst emacs feh flake8 fzf geoclue git gnupg gtk i3 icons ignore ipython isync jupyter khard kitty latex lnav lsd mailcap mime_types mpv msmtp navi neomutt networkmanager_dmenu newsboat notmuch npm nvim okular paru picom polybar python qutebrowser ranger readline rofi scripts shikane ssh sway sxhkd systemd tmux tuir urlscan vimpagerrc wallpapers waybar xmodmap yt-dlp zathura zsh
	@sudo stow root --target=/root/

.PHONY:install-pypi-packages
install-pypi-packages: 
	@echo "Installing Python appliations"
	@python -m pip install hypothepy --break-system-packages # Getting hypothesis links
	@for PACKAGE in $(PYPI_PACKAGES); do uv tool install "$$PACKAGE"; done

.PHONY: setup-systemd-services
setup-systemd-services:
	systemctl enable cmus --user
	systemctl enable notify-me.timer --user
	systemctl enable wireplumber.service --user
	systemctl enable udiskie.service --user
	systemctl enable dunst.service --user
	systemctl enable sxhkd.service --user
	systemctl enable geoclue-agent.service --user
	systemctl enable msmtp-runqueue.timer --user
	systemctl enable mbsync.timer --user
	sudo systemctl enable pkgfile-update.timer
	sudo systemctl enable greetd.service
	sudo systemctl enable displaylink
	sudo systemctl enable auto-cpufreq.service
	# Disables to not override backups while setting up the environment
	# Enable afterwards
	# sudo systemctl enable cronie
	sudo systemctl enable nftables.service
	sudo systemctl enable firewalld.service
	sudo systemctl enable bluetooth.service
	sudo systemctl enable systemd-resolved.service
	@# Weekly, discard unused blocks in filesystem
	sudo systemctl enable fstrim.timer

.PHONY: setup-qutebrowser
setup-qutebrowser:
	cd $(XDG_CONFIG_HOME)/qutebrowser/qutescript && pip install -e . --user --break-system-packages
	python $(XDG_CONFIG_HOME)/qutebrowser/userscripts/yank_all --install --bin=yank_all
	# Download dictionary
	/usr/share/qutebrowser/scripts/dictcli.py install en-US
	paru --sync --noconfirm --skipreview  chromium-widevine # viewing DRM content (Spotify)

## compare-packages: compare the current installed packages with the list
.PHONY: compare-packages
compare-packages: create-clean-pkglist
	@# Make expects a 0, otherwise it fails.
	@# diff returns 1 if a difference is found
	@diff -y --suppress-common-lines --color pkglist_clean.tmp <(pacman -Qqe | grep -vE "(paru|evdi-git|evdi|evdi-compat-git|wlroots-debug|cmake)" | sort) || echo ""
	@rm -f *tmp

.PHONY: help
help : Makefile
	@sed -n 's/^##//p' $<
