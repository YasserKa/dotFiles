# Only run if interactive
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

# Add autocompletion for newly added packages
# https://wiki.archlinux.org/title/zsh#Persistent_rehash
zstyle ':completion:*' rehash true

plug "zsh-users/zsh-autosuggestions"

export ZSH_AUTOSUGGEST_PARTIAL_ACCEPT_WIDGETS=(vi-forward-word my-forward-blank-word vi-forward-char)
# Remove forward-char, since it's used in my-forward-blank-word, leading for
# double expansion
EXCEPT=(forward-char vi-forward-char)
ZSH_AUTOSUGGEST_ACCEPT_WIDGETS=${ZSH_AUTOSUGGEST_ACCEPT_WIDGETS:|EXCEPT}
bindkey '^ ' autosuggest-execute
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

source_zsh_config() { source "$ZSHRC" }
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
# From the docs:
# Kill the word behind the cursor, without going past the point where insert mode was last entered.
#
# Alternative: Set mark at cursor position, move backward a word, then delete
# region.
my-backward-kill-word () {
    zle set-mark-command
    zle vi-backward-word
    zle kill-region
}
zle -N my-backward-kill-word

# Remove (word)- instead of (word-)
my-forward-kill-word () {
    zle set-mark-command
    zle vi-forward-word
    zle kill-region
}
zle -N my-forward-kill-word

my-backward-kill-blank-word () {
    zle set-mark-command
    zle vi-backward-blank-word
    zle kill-region
}
zle -N my-backward-kill-blank-word

my-forward-kill-blank-word () {
    zle set-mark-command
    zle vi-forward-blank-word-end
    zle forward-char
    zle kill-region
}
zle -N my-forward-kill-blank-word

my-forward-blank-word () {
    zle vi-forward-blank-word-end
    zle forward-char
}
zle -N my-forward-blank-word

bindkey -M vicmd ':' vi-repeat-find
# Navigation
bindkey -M viins '^a' beginning-of-line
bindkey -M viins '^[a' vi-first-non-blank
bindkey -M viins '^e' end-of-line

bindkey -M viins '^b' vi-backward-char
bindkey -M viins '^f' vi-forward-char
bindkey -M viins '^[b' vi-backward-word
bindkey -M viins '^[f' vi-forward-word
bindkey -M viins '^[B' vi-backward-blank-word
bindkey -M viins '^[F' my-forward-blank-word

# Editing
bindkey -M viins '^u' backward-kill-line
bindkey -M viins '^k' kill-line

bindkey -M viins '^h' backward-delete-char
bindkey -M viins '^d' delete-char
bindkey -M viins '^w' my-backward-kill-word
bindkey -M viins '^[d' my-forward-kill-word
bindkey -M viins '^[w' my-backward-kill-blank-word
bindkey -M viins '^[D' my-forward-kill-blank-word

# Digit arguments
bindkey -M viins '^[1' digit-argument
bindkey -M viins '^[2' digit-argument
bindkey -M viins '^[3' digit-argument
bindkey -M viins '^[4' digit-argument
bindkey -M viins '^[5' digit-argument
bindkey -M viins '^[6' digit-argument

bindkey -M viins '^[-' neg-argument 
# }}}
# Setup FZF {{{
if command -v fzf > /dev/null; then
  [[ -f /usr/share/fzf/completion.zsh ]] && . /usr/share/fzf/completion.zsh
  [[ -f /usr/share/fzf/key-bindings.zsh ]] && . /usr/share/fzf/key-bindings.zsh
fi

# Man widget via C-A-h
fzf-man-widget() {
  batman="man {1} 2>/dev/null | col -bx | bat --language=man --plain --color always"
   man -k . | sort \
   | awk -v cyan=$(tput setaf 6) -v blue=$(tput setaf 4) -v res=$(tput sgr0) -v bld=$(tput bold) '{ $1=cyan bld $1; $2=res blue;} 1' \
   | fzf  \
      -q "$1" \
      --ansi \
      --tiebreak=begin \
      --prompt='Man > '  \
      --preview-window '50%,rounded,<50(up,85%,border-bottom)' \
      --preview "${batman}" \
      --bind "enter:execute(man {1})"
  zle reset-prompt
}
bindkey '^[^H' fzf-man-widget
zle -N fzf-man-widget

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

# Override the widget to remove images made by kitty on completion
_fzf-file-widget() {
fzf-file-widget
printf "\x1b_Ga=d,d=A\x1b\\"
}
zle     -N            _fzf-file-widget
bindkey -M vicmd '^T' _fzf-file-widget
bindkey -M viins '^T' _fzf-file-widget

# Navi's widget
eval "$(navi widget zsh)"

plug "Aloxaf/fzf-tab"
# disable sort when completing `git checkout`
zstyle ':completion:*:git-checkout:*' sort false
# set descriptions format to enable group support
zstyle ':completion:*:descriptions' format '[%d]'
# set list-colors to enable filename colorizing
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# Remove the prefix "."
zstyle ':fzf-tab:*' prefix ''
# switch group using `,` and `.`
zstyle ':fzf-tab:*' switch-group ',' '.'
zstyle ':fzf-tab:complete:systemctl-*:*' fzf-preview 'SYSTEMD_COLORS=1 systemctl status $word'
# give a preview of commandline arguments when completing `kill`
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm -w -w"
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-preview \
  '[[ $group == "[process ID]" ]] && ps --pid=$word -o cmd --no-headers -w -w'
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-flags --preview-window=down:3:wrap

zstyle ':fzf-tab:complete:*:*' fzf-preview '$XDG_CONFIG_HOME/fzf/fzf_preview_media ${(Q)realpath}'

# Override the widget to remove images made by kitty on completion
_fzf-tab-complete() {
fzf-tab-complete
printf "\x1b_Ga=d,d=A\x1b\\"
}

zle     -N            _fzf-tab-complete
bindkey -M emacs '^I'  _fzf-tab-complete
bindkey -M viins '^I'  _fzf-tab-complete

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
pzf() {
  # Position of the value in each candidate
  pos=$1
  AUR_URL='https://aur.archlinux.org/packages'
  OFFICIAL_URL='https://archlinux.org/packages'
  shift
  sed "s/ /\t/g" |
    fzf --ansi --nth=$pos --multi --history="${FZF_HISTDIR:-$XDG_STATE_HOME/fzf}/history-pzf" \
    --preview-window=60%,border-left \
		--bind="ctrl-o:execute(xdg-open \$(paru -Si {$pos} | grep URL | head -1 | awk '{print \$NF}') 2>/dev/null)" \
		--bind="alt-o:execute(2>/dev/null { pacman -Si {$pos} &&  xdg-open '$OFFICIAL_URL/{$pos}' || xdg-open '$AUR_URL?K={$pos}&SB=p&SO=d&PP=100'; })" \
		--header 'C-o: Upstream URL, A-o: Official or AUR URL' \
    "$@" | cut -f$pos | xargs
  }

# Dev note: print -s adds a shell history entry

# List installable packages into fzf and install selection
pai() {
  cache_dir="/tmp/pas-$USER"
  mkdir -p "$cache_dir"
  preview_cache="$cache_dir/preview_{2}"
  list_cache="$cache_dir/list"
  { test "$(cat "$list_cache$@" | wc -l)" -lt 50000 && rm "$list_cache$@"; } 2>/dev/null

  pkg=$( (cat "$list_cache$@" 2>/dev/null || { pacman --color=always -Sl "$@"; paru --color=always -Sl aur "$@" } | sed 's/ [^ ]*unknown-version[^ ]*//' | tee "$list_cache$@") |
             pzf 2 --tiebreak=index --preview="cat $preview_cache 2>/dev/null | grep -v 'Querying' | grep . || paru --color always -Si {2} | tee $preview_cache")
  if test -n "$pkg"
  then echo "Installing $pkg..."
       cmd="paru -S $pkg"
       print -s "$cmd"
       eval "$cmd"
       rehash
       rm -rf "$cache_dir"
  fi
}
# List installed packages into fzf and remove selection
# Tip: use -e to list only explicitly installed packages
par() {
  pkg=$(paru --color=always -Q "$@" | pzf 1 --tiebreak=length --preview="paru --color always -Qli {1}")
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
;
# vim:foldmethod=marker
