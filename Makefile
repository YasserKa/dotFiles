## compare_packages: compare the current installed packages with the list
compare_packages: pkglist pkglist-aur
	pacman -Qent > pkglist.tmp # Official
	pacman -Qm > pkglist-aur.tmp # AUR
	diff -y --suppress-common-lines --color pkglist-aur pkglist-aur.tmp || exit 0
	diff -y --suppress-common-lines --color pkglist pkglist.tmp         || exit 0
	rm -f *.tmp

## update_packages: update official and AUR package lists available on the sysytem
update_packages:
	pacman -Qent > pkglist # Official
	pacman -Qm > pkglist-aur # AUR

.PHONY : help
help : Makefile
	@sed -n 's/^##//p' $<
