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

# activate dev environment
source $HOME/environment/pyenv/bin/activate

# pnpm
export PNPM_HOME="/home/zrik/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end
