# Ony run if interactive
[[ $- != *i* ]] && return

# NOTE: Load plugins in this order
# zsh-autosuggestions zsh-syntax-highlighting zsh-vim-mode
export ZAP_PATH="${XDG_DATA_HOME:-$HOME/.local/share}/zap"
[ -f "$ZAP_PATH/zap.zsh" ] && source "$ZAP_PATH/zap.zsh"

# Options {{{
unsetopt CLOBBER # Don't overwrite when redireting using shell
setopt AUTO_CD   # Auto cd without using cd
setopt CORRECT # Correct spelling of commands
setopt INTERACTIVECOMMENTS # # On interactive line for comment
setopt BASH_REMATCH
# Stop highlighting pasted text
zle_highlight+=(paste:none)

# History {{{
export HISTFILE=$XDG_CONFIG_HOME/zsh/history
export SAVEHIST=$HISTSIZE

setopt HIST_IGNORE_ALL_DUPS  # do not put duplicated command into history list
setopt HIST_SAVE_NO_DUPS  # do not save duplicated command
setopt HIST_REDUCE_BLANKS  # remove unnecessary blanks
setopt HIST_IGNORE_SPACE
setopt INC_APPEND_HISTORY_TIME  # append command to history file immediately after execution
setopt EXTENDED_HISTORY  # record command start time
# }}}

# Prompt
NEWLINE=$'\n'

plug "woefe/git-prompt.zsh"
source "$ZAP_PATH/plugins/git-prompt.zsh/git-prompt.zsh"
 
PROMPT='%B%F{cyan}%~%b $(gitprompt) ${NEWLINE}%F{#008C00}❯ %b%f'
# }}}
# Keybindings {{{
# Vi keybindings
bindkey -v

autoload -Uz run-help
(( ${+aliases[run-help]} )) && unalias run-help
alias help=run-help

# Edit command in $EDITOR
autoload -U edit-command-line
zle -N edit-command-line
bindkey '^[v' edit-command-line

# Expands history
bindkey " " magic-space

## Readline
# showkey -a
# Shift-TAB cycles completions backward
bindkey -M viins '\e[Z' reverse-menu-complete
# C-S-i
bindkey '\e[105;6u' reverse-menu-complete

bindkey -M vicmd ':' vi-repeat-find
bindkey -M viins '^w' vi-backward-kill-word
bindkey -M viins '^a' beginning-of-line
bindkey -M viins '^e' end-of-line
bindkey -M viins '^h' vi-backward-delete-char
bindkey -M viins '^b' vi-backward-char
bindkey -M viins '^f' vi-forward-char
bindkey -M viins '^d' delete-char

# With shift modifier
# bindkey -M viins '\e[98;6u' vi-backward-word
# bindkey -M viins '\e[102;6u' vi-forward-word
# bindkey -M viins '\e[100;6u' delete-word
bindkey -M viins '[^b' vi-backward-word
bindkey -M viins '[^f' vi-forward-word
bindkey -M viins '[^d' delete-word
# }}}
# Autocompletion {{{

# Files with colors
# https://github.com/trapd00r/LS_COLORS
source /usr/share/LS_COLORS/dircolors.sh

zstyle ':completion:*' menu select
autoload -Uz compinit
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
compinit

plug "zsh-users/zsh-autosuggestions"
bindkey '^ ' autosuggest-accept


# Add autocompletion for newly added packages
# https://wiki.archlinux.org/title/zsh#Persistent_rehash
zstyle ':completion:*' rehash true

plug "zdharma-continuum/fast-syntax-highlighting"
# }}}
# Vim {{{

# 10ms for key sequences delay
KEYTIMEOUT=1

MODE_INDICATOR=""
plug "softmoth/zsh-vim-mode"
plug "hlissner/zsh-autopair"
# Make zsh use system clipboard
plug "kutsan/zsh-system-clipboard"
# Remove the normal mode indication
MODE_CURSOR_VICMD="blinking block"
MODE_CURSOR_VIINS="blinking bar"

# NOTE: needs to be after vim, so the visual mode in vim doesn't disturb the
# plugin
# History backward with suggestions
plug "zsh-users/zsh-history-substring-search"
# }}}
# Setup FZF {{{
if command -v fzf > /dev/null; then
  [[ -f /usr/share/fzf/completion.zsh ]] && . /usr/share/fzf/completion.zsh
  [[ -f /usr/share/fzf/key-bindings.zsh ]] && . /usr/share/fzf/key-bindings.zsh
fi

# Use C-e and C-j to edit and execute command
fzf-history-widget() {
  local selected num
  setopt localoptions noglobsubst noposixbuiltins pipefail no_aliases 2> /dev/null
  selected=( $(fc -rl 1 | awk '{ cmd=$0; sub(/^[ \t]*[0-9]+\**[ \t]+/, "", cmd); if (!seen[cmd]++) print $0 }' |
    FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} ${FZF_DEFAULT_OPTS-} -n2..,.. --scheme=history --bind=ctrl-r:toggle-sort,ctrl-z:ignore ${FZF_CTRL_R_OPTS-}  --expect=ctrl-e --query=${(qqq)LBUFFER} +m" $(__fzfcmd)) )
  local ret=$?
  if [ -n "$selected" ]; then
     local accept=0
    if [[ $selected[1] = ctrl-e ]]; then
      accept=1
      shift selected
    fi
    num=$selected[1]
    if [ -n "$num" ]; then
      zle vi-fetch-history -n $num
 		[[ $accept = 0 ]] && zle accept-line
    fi
  fi
  zle reset-prompt
  return $ret
}
zle     -N            fzf-history-widget
bindkey -M vicmd '^R' fzf-history-widget
bindkey -M viins '^R' fzf-history-widget

plug "Aloxaf/fzf-tab"
# disable sort when completing `git checkout`
zstyle ':completion:*:git-checkout:*' sort false
# set descriptions format to enable group support
zstyle ':completion:*:descriptions' format '[%d]'
# set list-colors to enable filename colorizing
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
# preview directory's content with exa when completing cd
zstyle ':fzf-tab:complete:*' fzf-preview 'lsd -1 --color=always $realpath'
# switch group using `,` and `.`
zstyle ':fzf-tab:*' switch-group ',' '.'


# }}}
# Autojumping {{{
fasd_cache="$HOME/.fasd-init-zsh"
if [[ "$commands[fasd]" -nt "$fasd_cache" || ! -s "$fasd_cache" ]]; then
  fasd --init posix-alias zsh-hook zsh-ccomp zsh-ccomp-install \
    zsh-wcomp zsh-wcomp-install >| "$fasd_cache"
fi
source "$fasd_cache"
unset fasd_cache
# }}}
# Update terminal emulator title {{{
# https://wiki.archlinux.org/title/zsh#xterm_title
autoload -Uz add-zsh-hook

function xterm_title_precmd () {
	print -Pn -- '\e]2;%n@%m %~\a'
	[[ "$TERM" == 'screen'* ]] && print -Pn -- '\e_\005{g}%n\005{-}@\005{m}%m\005{-} \005{B}%~\005{-}\e\\'
}

function xterm_title_preexec () {
	print -Pn -- '\e]2;%n@%m %~ %# ' && print -n -- "${(q)1}\a"
	[[ "$TERM" == 'screen'* ]] && { print -Pn -- '\e_\005{g}%n\005{-}@\005{m}%m\005{-} \005{B}%~\005{-} %# ' && print -n -- "${(q)1}\e\\"; }
}

if [[ "$TERM" == (Eterm*|alacritty*|aterm*|foot*|gnome*|konsole*|kterm*|putty*|rxvt*|screen*|wezterm*|tmux*|xterm*) ]]; then
	add-zsh-hook -Uz precmd xterm_title_precmd
	add-zsh-hook -Uz preexec xterm_title_preexec
fi
# }}}
# Others {{{
#
source ~/.bashrc.d/functions.bash
source ~/.bashrc.d/aliases.bash

source /usr/share/doc/pkgfile/command-not-found.zsh
# Make sure that pinentry uses the correct TTY
export GPG_TTY=$(tty)
# }}}
# vim:foldmethod=marker
