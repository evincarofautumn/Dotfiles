# Bash
if [ -f ~/.bashrc ]; then
   source ~/.bashrc
fi

# Homebrew
eval "$(/opt/homebrew/bin/brew shellenv)"

# Git
alias ga='git add'
alias gb='git branch'
alias gc='git commit'
alias gd='git pull'
alias gD='git diff'
alias gf='git fetch'
alias gl='git log'
alias gm='git merge'
alias go='git checkout'
alias gr='git reset'
alias gs='git status'
alias gu='git push'

# Less
LESS="-iRS"
export LESS
