# Path to your oh-my-zsh installation.
export ZSH="/home/zrik/.oh-my-zsh"

# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Add wisely, as too many plugins slow down shell startup.
plugins=(git zsh-autosuggestions zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

# User configuration

alias ll="colorls -l"
alias _cp="xclip -selection clipboard -i"
alias _ps="xclip -selection clipboard -o"


# java config
export JAVA_HOME=/usr/bin/java
