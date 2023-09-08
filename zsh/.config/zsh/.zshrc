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
# setopt BASH_REMATCH # This option makes vim plugin not work for c and d operators

# Stop highlighting pasted text
zle_highlight+=(paste:none)

# History {{{
export HISTFILE=$XDG_CONFIG_HOME/zsh/history
export SAVEHIST=$HISTSIZE
export HISTORY_IGNORE="(&|[ ]*|exit|ls(*| )|cd|cd ..|bg|fg|history|pls|clear|*/pypoetry/virtualenvs/*)"

# Don't show ignored history commands the when navigating history
# Doesn't work on the last executed command
zshaddhistory() {
  emulate -L zsh
  ## uncomment if HISTORY_IGNORE
  ## should use EXTENDED_GLOB syntax
  # setopt extendedglob
  # $1 adds a line break to the command
  [[ "${1//[$'\n']}" != ${~HISTORY_IGNORE} ]]
}

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
# Autocompletion {{{

# Commands with colors
# https://github.com/garabik/grc
[[ -s "/etc/grc.zsh" ]] && source /etc/grc.zsh

# Files with colors
# https://github.com/trapd00r/LS_COLORS
source /usr/share/LS_COLORS/dircolors.sh

# Directories to search for autoloadings
fpath+=($ZDOTDIR/zsh-completions)
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

# }}}
# Vim {{{

# 10ms for key sequences delay
KEYTIMEOUT=10

# Disable vim prompt indication for normal mode
MODE_INDICATOR=""
plug "softmoth/zsh-vim-mode"
source "$ZAP_PATH/plugins/zsh-vim-mode/zsh-vim-mode.plugin.zsh"
MODE_CURSOR_VICMD="blinking block"
MODE_CURSOR_VISUAL="$MODE_CURSOR_VICMD steady"
MODE_CURSOR_VIINS="blinking bar"

# NOTE: has to be after zsh-vim-mode so vi" highlighting work
plug "zdharma-continuum/fast-syntax-highlighting"

plug "hlissner/zsh-autopair"
# Make zsh use system clipboard
plug "kutsan/zsh-system-clipboard"
source "$ZAP_PATH/plugins/zsh-system-clipboard/zsh-system-clipboard.plugin.zsh"
# Change clipboard method to xsel, since xclip is bugged
# https://github.com/kutsan/zsh-system-clipboard/issues/46
ZSH_SYSTEM_CLIPBOARD_METHOD="xsp"

# NOTE: needs to be after vim, so the visual mode in vim doesn't disturb the
# plugin
# History backward with suggestions
plug "zsh-users/zsh-history-substring-search"

HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_NOT_FOUND="fg=red,standout"
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND="fg=green,standout"

# The plugin will auto execute this zvm_after_init function
bindkey -M viins '^p' history-substring-search-up
bindkey -M viins '^n' history-substring-search-down
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

source_zsh_config() { exec zsh }
zle -N source_zsh_config
# NOTE: doesn't work for \e, ,since it goes to normal mode after pressing \e,
bindkey -M vicmd -r ' '
bindkey -M vicmd  ' ss' source_zsh_config


[[ $NEED_SOURCE_VENV ]] && source ./.venv/bin/activate

# }}}
#
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
# bindings can showkey -a
# Shift-TAB cycles completions backward
bindkey -M viins '\e[Z' reverse-menu-complete
# C-S-i
bindkey '\e[105;6u' reverse-menu-complete

# NOTE: vi-backward-kill-word stops working after going to insert mode
# create a word using these seperators
WORDCHARS='~!#$%^&*(){}[]<>?.+;-'

vi-backward-kill-word () {
    local WORDCHARS=""
    zle backward-kill-word
    zle -f kill
}

zle -N vi-backward-kill-word

bindkey -M vicmd ':' vi-repeat-find
bindkey -M viins '^w' vi-backward-kill-word
bindkey -M viins '^a' beginning-of-line
bindkey -M viins '^e' end-of-line
bindkey -M viins '^h' backward-delete-char
bindkey -M viins '^b' vi-backward-char
bindkey -M viins '^f' vi-forward-char
bindkey -M viins '^d' delete-char

# With Ctrl-Shift modifier
bindkey -M viins '^[b' vi-backward-word
bindkey -M viins '^[f' vi-forward-word
vi-backawrd-kill-word () {
    local WORDCHARS=""
    zle kill-word
    zle -f kill
}

zle -N vi-backawrd-kill-word
bindkey -M viins '^[d' vi-backawrd-kill-word
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
zstyle ':fzf-tab:complete:cd:*' fzf-preview '[[ -d "$realpath" ]] && lsd -1 --color=always $realpath || bat --color=always --style=numbers --theme gruvbox-dark $realpath'
zstyle ':fzf-tab:complete:lsd:*' fzf-preview '[[ -d "$realpath" ]] && lsd -1 --color=always $realpath || bat --color=always --style=numbers --theme gruvbox-dark $realpath'
zstyle ':fzf-tab:complete:_rg:*' fzf-preview '[[ -d "$realpath" ]] && lsd -1 --color=always $realpath || bat --color=always --style=numbers --theme gruvbox-dark $realpath'
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
# Yay install and uninstall {{{
# Helper function to integrate paru and fzf
yzf() {
  pos=$1
  shift
  sed "s/ /\t/g" |
    fzf --ansi --nth=$pos --multi --history="${FZF_HISTDIR:-$XDG_STATE_HOME/fzf}/history-yzf$pos" \
    --preview-window=60%,border-left \
    --bind="alt-enter:execute(xdg-open 'https://aur.archlinux.org/packages?K={$pos}&SB=p&SO=d&PP=100' 2>/dev/null)" \
		--bind="ctrl-o:execute(xdg-open \$(pacman -Si {$pos} | grep URL | awk '{print \$NF}') 2>/dev/null)" \
		--bind="alt-o:execute(xdg-open 'https://archlinux.org/packages/{$pos}' 2>/dev/null)" \
		--header 'C-o: Open repo, A-o: Open package in official, A-enter: Search in AUR' \
    "$@" | cut -f$pos | xargs
  }

# Dev note: print -s adds a shell history entry

# List installable packages into fzf and install selection
yas() {
  cache_dir="/tmp/yas-$USER"
  test "$1" = "-y" && rm -rf "$cache_dir" && shift
  mkdir -p "$cache_dir"
  preview_cache="$cache_dir/preview_{2}"
  list_cache="$cache_dir/list"
  { test "$(cat "$list_cache$@" | wc -l)" -lt 50000 && rm "$list_cache$@"; } 2>/dev/null

  	pkg=$( (cat "$list_cache$@" 2>/dev/null || { pacman --color=always -Sl "$@"; paru --color=always -Sl aur "$@" } | sed 's/ [^ ]*unknown-version[^ ]*//' | tee "$list_cache$@") |
      yzf 2 --tiebreak=index --preview="cat $preview_cache 2>/dev/null | grep -v 'Querying' | grep . || paru --color always -Si {2} | tee $preview_cache")
          if test -n "$pkg"
          then echo "Installing $pkg..."
            cmd="paru -S $pkg"
            print -s "$cmd"
            eval "$cmd"
            rehash
          fi
        }
        # List installed packages into fzf and remove selection
        # Tip: use -e to list only explicitly installed packages
        yar() {
          pkg=$(paru --color=always -Q "$@" | yzf 1 --tiebreak=length --preview="paru --color always -Qli {1}")
          if test -n "$pkg"
          then echo "Removing $pkg..."
            cmd="paru -R --cascade --recursive $pkg"
            print -s "$cmd"
            eval "$cmd"
          fi
        }
				# }}}
# Others {{{
source ~/.bashrc.d/functions.bash
source ~/.bashrc.d/aliases.bash

source /usr/share/doc/pkgfile/command-not-found.zsh
# Make sure that pinentry uses the correct TTY
export GPG_TTY=$(tty)
# }}}
# vim:foldmethod=marker
fzf-man-widget() {
  batman="man {1} | col -bx | bat --language=man --plain --color always"
   man -k . | sort \
   | awk -v cyan=$(tput setaf 6) -v blue=$(tput setaf 4) -v res=$(tput sgr0) -v bld=$(tput bold) '{ $1=cyan bld $1; $2=res blue;} 1' \
   | fzf  \
      -q "$1" \
      --ansi \
      --tiebreak=begin \
      --prompt='Man > '  \
      --preview-window '50%,rounded,<50(up,85%,border-bottom)' \
      --preview "${batman}" \
      --bind "enter:execute(man {1})" \
      --bind "alt-c:+change-preview(cht.sh {1})+change-prompt(Cheat > )" \
      --bind "alt-m:+change-preview(${batman})+change-prompt(Man > )" \
      --bind "alt-t:+change-preview(tldr {1})+change-prompt(TLDR > )"
  zle reset-prompt
}
# C-A-h keybinding to launch
bindkey '^[^H' fzf-man-widget
zle -N fzf-man-widget
