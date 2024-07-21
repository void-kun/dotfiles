export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="eastwood"

plugins=(git
		 zsh-syntax-highlighting
		 zsh-autosuggestions
		 docker
		 fzf)

source $ZSH/oh-my-zsh.sh
source $HOME/.config/scripts/script.zsh

# User configuration
alias ll="eza -la"
alias upd="sudo apt update"
alias upg="sudo apt upgrade"
alias updg="upd && upg"

alias _cp="xclip -selection clipboard -i"
alias _ps="xclip -selection clipboard -o"

# pnpm
export PNPM_HOME="/home/zrik/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/zrik/environment/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/zrik/environment/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/home/zrik/environment/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/zrik/environment/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<
