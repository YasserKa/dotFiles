## Setup

- Install Arch Linux using [archinstall](https://wiki.archlinux.org/title/Archinstall)
- Restart the system
- Pull this repository and adjust `pkglist` and `Makefile` to your needs (e.g. most likely, you
  don't need `displaylink` and `evdi-git` packages)
- Execute `make install`
- Open okular to import keybinding scheme"
- Open vim and Emacs to setup and install packages

Note: nvim will open `sudoers.d` at the beginning, insert `Defaults        env_reset,timestamp_timeout=-1` 
to get rid of typing the password during the process. Remove the line afterwards.

## License

You can see the license of some of the files at the start of the file itself,
otherwise (if it not mentioned), it's MIT.
