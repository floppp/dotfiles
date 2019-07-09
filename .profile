# Directorios de uso comun
export MLBOOK=$HOME/workspaces/python/ws-artificial_intelligence/hands-on-practice-book

# NVIDIA CUDA Toolkit
export PATH=/usr/local/cuda-9.0/bin:$PATH
export LD_LIBRARY_PATH=/usr/local/cuda-9.0/lib64/
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/cuda/extras/CUPTI/lib64
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/home/nando/Software/c_libs
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/lib
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib
#export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/MATLAB/MATLAB_Runtime/v96/runtime/glnxa64
#export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/MATLAB/MATLAB_Runtime/v96/bin/glnxa64
#export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/MATLAB/MATLAB_Runtime/v96/sys/os/glnxa64
#export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/MATLAB/MATLAB_Runtime/v96/extern/bin/glnxa64

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/MATLAB/MATLAB_Runtime/v91/runtime/glnxa64
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/MATLAB/MATLAB_Runtime/v91/bin/glnxa64
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/MATLAB/MATLAB_Runtime/v91/sys/os/glnxa64

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/lib/qt/plugins/platforms

export PATH=$PATH:$ANDROID_HOME/tools
export PATH=$PATH:$ANDROID_HOME/platform-tools
export PATH="/home/nando/bin:$PATH"
export PATH="/home/nando/.local/bin:$PATH"
export PATH="/home/nando/.gem/ruby/2.4.0/bin:$PATH"
export PATH="/home/nando/Software/utils:$PATH"
export PATH="/home/nando/Software/scripts:$PATH"
export PATH="/home/nando/miniconda3/bin:$PATH"

export ANDROID_HOME=$HOME/Android/Sdk

export PYTHONPATH="/home/nando/Software/python_libs:$PYTHONPATH"

alias connect_server="ssh -i /home/nando/Documentos/imm/20190206_fernando/id_rsa fernando@167.114.242.135 -p2345"
# Directorios actualmente en uso (IR LIMPIANDO REGULARMENTE)
alias tcbelt-web="/home/nando/workspaces/imm/tc-belt-nebular/web"
