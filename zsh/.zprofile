# Common Environment variables and starts X11
[[ -f $HOME/.profile ]] && . "$HOME/.profile"

# Needed, else the login shell won't source it
[[ -f $ZDOTDIR/.zshrc ]] && . "$ZDOTDIR/.zshrc"
