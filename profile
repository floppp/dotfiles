source ~/.profile_private

# =====
# Alias
# =====
alias vim='emacs -nw'
alias vi="emacs -q -nw -l ~/.emacs.simpler.d/init.el"
alias uvi="emacs -q -nw -l ~/.emacs.micro.d/init.el"
alias nano='emacs -nw -Q'

# =====
# PATH
# =====
# TODO: creo que esto por defecto se mete, revisar .zshrc
export PATH=$PATH:/home/nando/.local/bin

# ==========
# NVM
# ==========
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Cargamos otras funciones y scripts.
source ~/.functions
. /home/nando/.local/bin/z.sh

# ==========
# NNN
# ==========
export NNN_PLUG="c:fzcd;o:fzopen;p:preview-tui;z:fzz"
# set --export NNN_FIFO "/tmp/nnn.fifo"
export NNN_FIFO="/tmp/nnn.fifo"

# ==========
# DOCKER
# ==========
alias dcls='docker ps -aq'
alias dcrm='docker rm $(docker ps -aq)'
alias dvls='docker volume ls -q'
alias dvrm='docker volume rm $(docker volume ls -q)'
alias dc-up='docker-compose up'
alias dc-down='docker-compose down'
source ~/.functions_docker


################################################################################

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/nando/.sdkman"
[[ -s "/home/nando/.sdkman/bin/sdkman-init.sh" ]] && source "/home/nando/.sdkman/bin/sdkman-init.sh"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/nando/miniconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/nando/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/home/nando/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/nando/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

