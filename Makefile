ignored_packages := "dbeaver|postgresql"

get_installed_packages:
	pacman -Qent | grep -vE  $(ignored_packages) > pkglist.tmp # Official
	pacman -Qm | grep -vE $(ignored_packages) > pkglist-aur.tmp # AUR

## compare_packages: compare the current installed packages with the list
compare_packages: get_installed_packages
	diff -y --suppress-common-lines --color pkglist-aur pkglist-aur.tmp || exit 0
	diff -y --suppress-common-lines --color pkglist pkglist.tmp         || exit 0
	rm -f *.tmp

## update_packages: update official and AUR package lists available on the sysytem
update_packages: get_installed_packages
	mv pkglist.tmp pkglist
	mv pkglist-aur.tmp pkglist-aur

.PHONY : help
help : Makefile
	@sed -n 's/^##//p' $<
