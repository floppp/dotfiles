# Directorios de uso comun
export MLBOOK=$HOME/workspaces/python/ws-artificial_intelligence/hands-on-practice-book

# NVIDIA CUDA Toolkit
export PATH=/usr/local/cuda-9.0/bin:$PATH
export LD_LIBRARY_PATH=/usr/local/cuda-9.0/lib64/
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/cuda/extras/CUPTI/lib64
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/home/nando/Software/c_libs
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/lib
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/MATLAB/MATLAB_Runtime/v96/runtime/glnxa64
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/MATLAB/MATLAB_Runtime/v96/bin/glnxa64
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/MATLAB/MATLAB_Runtime/v96/sys/os/glnxa64
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/MATLAB/MATLAB_Runtime/v96/extern/bin/glnxa64

#export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/MATLAB/MATLAB_Runtime/v91/runtime/glnxa64
#export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/MATLAB/MATLAB_Runtime/v91/bin/glnxa64
#export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/MATLAB/MATLAB_Runtime/v91/sys/os/glnxa64

#export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/lib/qt/plugins/platforms

export ANDROID_HOME=$HOME/Android/Sdk

export PATH=$PATH:$ANDROID_HOME/tools
export PATH=$PATH:$ANDROID_HOME/platform-tools
export PATH="/home/nando/bin:$PATH"
export PATH="/home/nando/.local/bin:$PATH"
export PATH="/home/nando/.gem/ruby/2.4.0/bin:$PATH"
export PATH="/home/nando/Software/utils:$PATH"
export PATH="/home/nando/Software/scripts:$PATH"
export PATH="/home/nando/miniconda3/bin:$PATH"
export PATH="/home/nando/flutter/bin:$PATH"

export PYTHONPATH="/home/nando/Software/python_libs:$PYTHONPATH"

alias connect_server="ssh -i /home/nando/Documentos/imm/20190206_fernando/id_rsa fernando@167.114.242.135 -p2345"
# Directorios actualmente en uso (IR LIMPIANDO REGULARMENTE)
alias tcbelt-web="/home/nando/workspaces/imm/tc-belt-nebular/web"
alias cubicdron-web="/home/nando/workspaces/imm/cubicdron/web"
alias ws-master="cd /home/nando/workspaces/master-apps-moviles/1-android-fundamentos/proyectos"
alias emacs-eos='XLIB_SKIP_ARGB_VISUALS=1 emacs'


export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion


function gac {
    [[ $# != 1 ]] && echo 'Hay que pasarle el mensaje del commit a la función.' && return

    git add .
    git commit -m $1

    echo "===>>> Commit realizado."
}

function gacp {
    echo "3 formas de invocar"
    echo "gacp mensaje-commit                                 === gacp mensaje-commit origin master"
    echo "gacp mensaje-commit rama-a-subir                    === gacp mensaje-commit origin rama-a-subir"
    echo "gacp mensaje-commit repositorio-remoto rama-a-subir"
    echo ""

    [[ $# == 1 ]] && gac $1 && echo "===>>> Subiendo rama master al repositorio origin."  && git push origin master && return

    [[ $# == 2 ]] && gac $1 && echo "===>>> Subiendo rama $2 al repositorio origin."      && git push origin $2     && return

    [[ $# != 3 ]] && echo "Hace falta mensaje de commit, rama que subir y repositorio al que subir." && return

    echo "===>>> Subiendo rama $3 al repositorio $2." &&

    gac $1
    git push $2 $3
}
