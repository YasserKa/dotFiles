ignored_packages = "dbeaver|postgresql"
USER = yasser

.PHONY: pre-install
pre-install: install-aur-helper install-etc

.PHONY: install
install: install-official-packages install-aur-packages stow-packages

.PHONY: stow-etc
stow-etc:
	sudo stow etc --target=/

.PHONY: install-aur-helper
install-aur-helper:
	rm -rf /tmp/baph
	git clone https://github.com/PandaFoss/baph /tmp/baph
	cd /tmp/baph && sudo -u root make install
	rm -rf /tmp/baph

.PHONY: install-official-packages
install-official-packages:
	pacman -Syu --needed --noconfirm \
		... \

.PHONY: install-aur-packages
install-aur-packages:
	baph --install --noconfirm --noview \

.PHONY: stow-packages
stow-packages:
	stow abook feh isync jupyter alacritty autorandr bash bat cmus cron-jobs dunst emacs fasd git gnupg gtk i3 icons latex lnav lsd mailcap networkmanager_dmenu notmuch npm shell_common X11 mpv msmtp mutt newsboat nvim pam picom polybar projects qutebrowser ranger readline rofi scripts ssh sxhkd systemd termite tmux tuir urlview vimpagerrc wallpapers xdbus xdg-open xmodmap zathura

setup-systemd:
	# Time synchro
	systemctl enable chrony.service
	systemctl start chrony.service
	# Display link
	systemctl start displaylink
	systemctl enable displaylink
	# Cmus
	systemctl start cmus --user
	systemctl enable cmus --user

.PHONY: setup-vim
setup-vim:
	nvim +'PlugInstall --sync' +qa

.PHONY: get_installed_packages
get_installed_packages:
	pacman -Qent | grep -vE  $(ignored_packages) > pkglist.tmp # Official
	pacman -Qm | grep -vE $(ignored_packages) > pkglist-aur.tmp # AUR

.PHONY: setup_qutebrowser
setup_qutebrowser:
	# cd "$XDG_CONFIG_HOME/qutebrowser"
	git clone https://github.com/hiway/python-qutescript.git qutescript
	cd qutescript
	pip install -e .
	# Download dictionary
	/usr/share/qutebrowser/scripts/dictcli.py install en-US
	sudo pacman -S youtube-dl aspell # for a userscript
	pip install adblock tldextract # ad block
	baph -i chromium-widevine \ # viewing DRM content (Spotify)
	qtwebkit-plugins-git # For SpellChecking

## compare_packages: compare the current installed packages with the list
.PHONY: compare_packages
compare_packages: get_installed_packages
	diff -y --suppress-common-lines --color pkglist-aur pkglist-aur.tmp || exit 0
	diff -y --suppress-common-lines --color pkglist pkglist.tmp         || exit 0
	rm -f *.tmp

## update_packages: update official and AUR package lists available on the sysytem
.PHONY: update_packages
update_packages: get_installed_packages
	mv pkglist.tmp pkglist
	mv pkglist-aur.tmp pkglist-aur

.PHONY: help
help : Makefile
	@sed -n 's/^##//p' $<
