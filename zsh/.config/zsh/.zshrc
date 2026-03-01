# Only run if interactive
[[ $- != *i* ]] && return


[[ -f "$HOME/.profile" ]] && source "$HOME/.profile"

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
export HISTFILE=$HOME/.dotfiles-private/zsh/.config/zsh/history
export SAVEHIST=$HISTSIZE
export HISTORY_IGNORE="(&|[ ]*|exit|ls(*| )|cd|cd ..|cd -|bg|fg|history|pls|clear|*/pypoetry/virtualenvs/*)"

setopt HIST_IGNORE_ALL_DUPS  # do not put duplicated command into history list
setopt HIST_SAVE_NO_DUPS  # do not save duplicated command
setopt HIST_REDUCE_BLANKS  # remove unnecessary blanks
setopt HIST_IGNORE_SPACE
# This resets the history if enabled (C-R -> CMD in HISTORY_IGNORE -> C-c)
unsetopt INC_APPEND_HISTORY_TIME  # append command to history file immediately after execution
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
plug "sunlei/zsh-ssh"

zstyle ':completion:*' menu select
autoload -Uz compinit; compinit
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
compdef v='nvim'

# Add autocompletion for newly added packages
# https://wiki.archlinux.org/title/zsh#Persistent_rehash
zstyle ':completion:*' rehash true

plug "zsh-users/zsh-completions"

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
# Change comment color to grey
: ${FAST_HIGHLIGHT_STYLES[${FAST_THEME_NAME}comment]::=fg=243}

plug "hlissner/zsh-autopair"
# Make zsh use system clipboard
plug "kutsan/zsh-system-clipboard"
source "$ZAP_PATH/plugins/zsh-system-clipboard/zsh-system-clipboard.plugin.zsh"
# Change clipboard method to xsel, since xclip is bugged
# https://github.com/kutsan/zsh-system-clipboard/issues/46
if [[ -n "${WAYLAND_DISPLAY}" ]]; then
  ZSH_SYSTEM_CLIPBOARD_METHOD="wlc"
elif [[ -n "${DISPLAY}" ]]; then
  ZSH_SYSTEM_CLIPBOARD_METHOD="xsc"
fi

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

# Add timestamp to fzf history
# This disables fzf's line wrapping because of fc's limitations https://github.com/junegunn/fzf/issues/1049
# Add functionality to remove history entries (check fzf config)
# https://github.com/junegunn/fzf/discussions/3629#discussioncomment-8475902
source <(fzf --zsh | sed -e '/zmodload/s/perl/perl_off/' -e '/selected/s/fc -rl 1/fc -rli 1 | awk '\''{$1 = "\\033[2;37m" $1 "\\033[0m"; $2 = "\\033[1;32m" $2; $3 = $3 "\\033[0m"; for (i=4; i<=NF; i++) $i = "\\033[1;38m" $i "\\033[0m"; print}'\''/' \
| awk '
  /fzf-history-widget\(\)/ {
  print;
  flag = 1;
  next
}
/setopt/ {
print
if (flag) {
  print "  # appends the current shell history buffer to the HISTFILE";
  print "  builtin fc -AI $HISTFILE";
  print "  # pushes entries from the $HISTFILE onto a stack and uses this history";
  print "  builtin fc -p $HISTFILE $HISTSIZE $SAVEHIST";
}
next
}

/FZF_DEFAULT_OPTS/ {
if (flag) {
gsub(/\+m/, "--multi"); print
} else {
print
}
next
}

/zle reset-prompt/ {
if (flag) {
  print "  # Read the history from the history file into the history list"
  print "  builtin fc -R $HISTFILE";
  flag = 0
}
print
}
{ print }
')

# Autojumping {{{
# add zsh hook
_fasd_preexec() {
  { fasd --proc $(fasd --sanitize "$2") } >> "/dev/null" 2>&1
}
autoload -Uz add-zsh-hook
add-zsh-hook preexec _fasd_preexec
# }}}
# Fzf Frecency  {{{
# Relative paths for candidates available in current directory
IFS='' read -r -d '' fre_candidates <<EOF
  fasd -lR | grep "\$(pwd)" | sed -e "s|^\$(pwd)/\?||" -e '/^[[:space:]]*$/d'
EOF
# export fre_candidates

export FZF_BASE_COMMAND='fd --follow --hidden --exclude .git --ignore-file $HOME/.ignore'
# Ignore frecency candidates with fd
export FZF_DEFAULT_COMMAND='command cat <(eval '$fre_candidates') <('$FZF_BASE_COMMAND' --strip-cwd-prefix | grep -Fvx -f <(eval '$fre_candidates'))'

export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_CTRL_T_OPTS="$FZF_DEFAULT_OPTS --tiebreak=index --scheme=path
$(
cat <<'EOF'
 --bind "ctrl-alt-d:execute-silent(fasd --delete \"$(realpath --strip "$(pwd)/{}" | xargs -I{} sh -c '[ -d "{}" ] && echo "{}/" || echo "{}"')\")+reload(eval \"$FZF_CTRL_T_COMMAND\")"
EOF
)"

# Only directories
IFS='' read -r -d '' fre_candidates <<EOF
  fasd -dlR | grep "\$(pwd)" | sed -e "s|^\$(pwd)/\?||" -e '/^[[:space:]]*$/d'
EOF
export FZF_ALT_C_COMMAND='command cat <(eval '$fre_candidates') <('$FZF_BASE_COMMAND' --strip-cwd-prefix --type d | grep -Fvx -f <(eval '$fre_candidates'))'
export FZF_ALT_C_OPTS="$FZF_DEFAULT_OPTS --tiebreak=index --scheme=path
$(
cat <<'EOF'
--bind "ctrl-alt-d:execute-silent(fasd --delete \"$(realpath --strip "$(pwd)/{}")/\")+reload(eval \"$FZF_ALT_C_COMMAND\")"
EOF
)"
unset fre_candidates


# NOTE: It's possible to use OPTS for each compgen, but it requires injecting it in fzf --zsh or a PR
# https://github.com/junegunn/fzf/blob/4bedd33c593ab0cb750e17c42750048904fdf7fb/shell/completion.zsh#L160-L163
_fzf_compgen_path() {
	local _path="$(realpath --strip $1)"
  IFS='' read -r -d '' fre_candidates <<EOF
    fasd -lR | grep "\${_path}[^ ]*" | sed -e "s|^\$(pwd)/\?||" -e '/^[[:space:]]*$/d'
EOF

command cat <(eval $fre_candidates) <(eval "$FZF_BASE_COMMAND . $1" | grep -Fvx -f <(eval $fre_candidates))
}

_fzf_compgen_dir() {
	local _path="$(realpath --strip $1)"
  IFS='' read -r -d '' fre_candidates <<EOF
    fasd -dlR | grep "\${_path}[^ ]*" | sed -e "s|^\$(pwd)/\?||" -e '/^[[:space:]]*$/d'
EOF
  command cat <(eval $fre_candidates) <(eval "$FZF_BASE_COMMAND --type d . $1" | grep -Fvx -f <(eval $fre_candidates))
}
# }}}
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
zstyle ':completion:*' file-sort access follow
zstyle ':completion:complete:*:argument-rest' sort false

# Remove the prefix "."
zstyle ':fzf-tab:*' prefix ''
# switch group using `,` and `.`
zstyle ':fzf-tab:*' switch-group ',' '.'
zstyle ':fzf-tab:*' continuous-trigger 'tab'
zstyle ':fzf-tab:complete:systemctl-*:*' fzf-preview 'SYSTEMD_COLORS=1 systemctl status $word'
# give a preview of commandline arguments when completing `kill`
# zstyle ':completion:*:*:*:*:processes' list-colors '=*=90'
zstyle ':completion:*:*:*:*:processes' command 'ps --user $USER -o ppid,pid,stime,etime,args'
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-preview \
  '[[ $group == "[process ID]" ]] && grc --colour=on ps -F $word | sed 1d'
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-flags --preview-window=down:3:wrap
zstyle ':fzf-tab:complete:(-command-|-parameter-|-brace-parameter-|export|unset|expand):*' \
	fzf-preview 'echo ${(P)word}'
zstyle ':fzf-tab:*' fzf-min-height 40
zstyle ':fzf-tab:*' fzf-min-height 40

zstyle ':fzf-tab:complete:*' fzf-bindings \
  'ctrl-k:kill-line' \
  'alt-n:half-page-down,alt-p:half-page-up' \
  'alt-N:page-down,alt-P:page-up' \
  'ctrl-alt-j:jump' \
  'alt-space:toggle-all' \
  'alt-j:preview-down,alt-k:preview-up' \
  'alt-J:preview-page-down,alt-K:preview-page-up' \
  'alt->:preview-bottom,alt-<:preview-top'

zstyle ':fzf-tab:complete:*:*' fzf-preview '$XDG_CONFIG_HOME/fzf/fzf_preview_media ${(Q)realpath}'

# Override the widget to remove images made by kitty on completion
_fzf-tab-complete() {
  zle fzf-tab-complete
  printf "\x1b_Ga=d,d=A\x1b\\"
}

zle     -N            _fzf-tab-complete
bindkey -M emacs '^I'  _fzf-tab-complete
bindkey -M viins '^I'  _fzf-tab-complete


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
source ~/.bashrc.d/functions.bash
source ~/.bashrc.d/aliases.bash

source /usr/share/doc/pkgfile/command-not-found.zsh
# Make sure that pinentry uses the correct TTY
export GPG_TTY=$(tty)
# }}}
# vim:foldmethod=marker
