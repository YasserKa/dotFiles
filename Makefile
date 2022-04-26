USER = yasser
XDG_CONFIG_HOME = $(HOME)/.config
XDG_DATA_HOME=$(HOME)/.local/share

# TODO: Use external list
OFFICIAL-PACKAGES = khard alacritty cmus feh isync autorandr bat dunst emacs fasd git adapta-gtk-theme gnupg i3-gaps lnav mailcap notmuch npm mpv msmtp neomutt pam picom qutebrowser ranger rofi rofi-calc sxhkd tmux tuir unclutter zathura xorg-xkill xorg-xinit unzip zip wget udiskie neovim-qt

AUR-PACKAGES = networkmanager_dmenu neovim-nightly-bin urlview polybar vimpager-git nerd-fonts-complete lsd-git

INSTALL-OFFICIAL-PACKAGES = $(patsubst %, install-official-%, $(OFFICIAL-PACKAGES))
INSTALL-AUR-PACKAGES = $(patsubst %, install-aur-%, $(AUR-PACKAGES))

.PHONY: install
install: stow-etc install-aur-helper $(INSTALL-OFFICIAL-PACKAGES) $(INSTALL-AUR-PACKAGES) stow-extras
install-pip-packages:
	pip install --user pynvim jedi
	# used by zathura synctex for nvim
	pip install neovim-remote
.PHONY: stow-etc
stow-etc:
	sudo stow etc --target=/

  # Get plugings
  curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  nvim -c 'PlugInstall|qall'
  # install coc extensions
  nvim -c 'CocInstall -sync coc-snippets coc-db coc-explorer coc-json  coc-pyright coc-emmet coc-html coc-vimtex|q'
  pacman -S tree-sitter

.PHONY: install-aur-helper
install-aur-helper:
	rm -rf /tmp/paru
	git clone https://aur.archlinux.org/paru.git  /tmp/paru
	cd /tmp/paru && makepkg --install --syncdeps --noconfirm
	rm -rf /tmp/baph

pre-install-packages:
	mkdir -p $(HOME)/Pictures
	mkdir -p $(XDG_DATA_HOME)/applications
	mkdir -p $(XDG_CONFIG_HOME)/systemd/user
	mkdir -p $(XDG_CONFIG_HOME)/cmus
	mkdir -p $(XDG_CONFIG_HOME)/jupyter
	mkdir -p $(XDG_CONFIG_HOME)/emacs
	mkdir -p $(XDG_CONFIG_HOME)/nvim
	mkdir -p $(XDG_CONFIG_HOME)/qutebrowser
	sudo pacman --sync --refresh --sysupgrade --noconfirm stow

install-aur-%: pre-install-packages
	paru --install --noconfirm $*

install-official-%:
	sudo pacman --sync --refresh --sysupgrade --noconfirm $*

.PHONY: stow-extras
stow-extras:
	stow bash cron-jobs git gnupg i3 icons latex lsd msmtp nvim projects readline scripts shell_common ssh systemd wallpapers X11 xdbus xdg-open xmodmap



post-install:
	stow
	emacs --batch --load=$(XDG_CONFIG_HOME)/emacs/init.el
	nvim +'PlugInstall --sync' +qa
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
	systemctl enable tmux.service --user
	systemctl start tmux.service --user
	sudo pacman -S nftables
	sudo systemctl enable nftables.service
	sudo systemctl start nftables.service
	# Enable actions like F keys
	sudo systemctl enable acpid.service
	sudo systemctl start acpid.service
	systemctl --user daemon-reload
	systemctl daemon-reload

.PHONY: setup_qutebrowser
setup_qutebrowser:
	cd "$(XDG_CONFIG_HOME)/qutebrowser" && pip install -e . --user
	# Download dictionary
	/usr/share/qutebrowser/scripts/dictcli.py install en-US
	# TODO: include in the list
	sudo pacman -S youtube-dl aspell # for a userscript
	pip install adblock tldextract # ad block
	paru -S chromium-widevine # viewing DRM content (Spotify)
	paru -S qtwebkit-plugins-git # For SpellChecking

.PHONY: get_installed_packages
get_installed_packages:
	pacman -Qent | grep -vE  $(IGNORED_PACKAGES) > pkglist.tmp # Official
	pacman -Qm | grep -vE $(IGNORED_PACKAGES) > pkglist-aur.tmp # AUR

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
