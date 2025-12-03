zmodload zsh/zprof

if [[ ("$TERM_PROGRAM" == "WezTerm") && -z "$(tmux list-clients 2>/dev/null)" ]]; then
    exec tmux new -As main;
fi

if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

export LANG="en_US.UTF-8"

alias brewup="brew update && brew upgrade && brew doctor"

source /opt/homebrew/opt/antidote/share/antidote/antidote.zsh
antidote load

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

DIR_WS="$HOME/me/ws"
DIR_DOTFILES="$HOME/me/dotfiles"

w () {
    cd "$DIR_WS/$1"
}

wl () {
    cd "$DIR_WS/learnings/$1"
}

wp () {
    cd "$DIR_WS/projects/$1"
}

wo () {
    cd "$DIR_WS/others/$1"
}

alias d="cd $DIR_DOTFILES"

alias g="git"
alias l="ls -lah"

export PATH="/Users/kimsaram32/me/ws/bin:$PATH"

export PATH="/opt/homebrew/opt/postgresql@16/bin:$PATH"

export FZF_DEFAULT_OPTS=" \
  --preview 'bat --color=always {}' --preview-window up \
  --bind \"ctrl-y:execute-silent(basename {} | cut -d . -f1 | tr -d '\n' | pbcopy)\" \
  --bind \"ctrl-o:execute-silent[ \
    tmux select-pane -R
    tmux send-keys :e Space && \
    tmux send-keys -l {} && \
    tmux send-keys Enter \
  ]\""

fif () {
  if [ ! "$#" -gt 0 ]
  then
    echo "Need a string to search for!"
    return 1
  fi
  rg --files-with-matches --no-messages -i "$1" $PWD | \
  fzf --delimiter / --with-nth -1 \
    --preview "highlight -O ansi -l {} 2> /dev/null | rg --colors 'match:bg:yellow' --ignore-case --pretty --context 10 '$1' || rg --ignore-case --pretty --context 10 '$1' {}"
}

export RESTIC_REPOSITORY=/Volumes/What/backups
export RESTIC_PASSWORD_FILE=~/.restic_password

export PATH="/opt/homebrew/opt/gnu-sed/libexec/gnubin:$PATH"

export PNPM_HOME="/Users/kimsaram32/Library/pnpm"
export PATH="$PNPM_HOME:$PATH"

alias yarn="corepack yarn"
alias yarnpkg="corepack yarnpkg"
alias pnpm="corepack pnpm"
alias pnpx="corepack pnpx"
alias npm="corepack npm"
alias npx="corepack npx"

export PATH="/Users/kimsaram32/.deno/bin:$PATH"

export NVM_DIR="$HOME/.nvm"
export NVM_ROOT="/opt/homebrew/opt/nvm"

lazyload node nvm -- '[ -s "$NVM_ROOT/nvm.sh" ] && \. "$NVM_ROOT/nvm.sh"
	   [ -s "$NVM_ROOT/etc/bash_completion.d/nvm" ] && \. "$NVM_ROOT/etc/bash_completion.d/nvm"
'

export PATH="$HOME/.jenv/bin:$PATH"
lazyload jenv java -- 'eval "$(jenv init -)"'

export PATH="$HOME/.local/bin/:$PATH"

eval "$(mise activate zsh)"
export PATH=$PATH:$HOME/zk/bin

export GOPATH="/Users/kimsaram32/go"
export PATH="$GOPATH/bin:$PATH"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/kimsaram32/.local/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/kimsaram32/.local/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/kimsaram32/.local/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/kimsaram32/.local/google-cloud-sdk/completion.zsh.inc'; fi

__conda_setup="$('/opt/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/opt/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/opt/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/opt/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
