#!/bin/bash
source ~/.functions

pls=(python clojure scala javascript typescript)
declare -A test_command
declare -A test_command
test_command[clojure]="lein test"

function in_array() {
    val=$1
    in_array=$(printf ",%s" "${pls[@]}" | grep -ow $val | wc -w)
    echo $in_array
}

while [[ -n $1 ]]; do
    case "$1" in
        -r)
            remote_folder=$2
            shift
            ;;
        -s)
            server=$2
            shift
            ;;
        -p)
            pl=$2  # pl == programming language
            shift
            ;;
        -e)
            exclude_file=$2
            shift
            ;;
        -h)
            echo "2 parámetros obligatorios:"
            echo "  -r directorio remoto donde subir archivos."
            echo "  -s proveedor donde se encuentra servidor remoto."
            echo "2 parámetros opcionales:"
            echo "  -p lenguaje de programación con el que estamos trabajando."
            echo "  -e archivo con listado a excluir."
            exit
            ;;
    esac
    shift
done

# Obligo a pasar servidor porque sino, al enviar desqe aquí a upload_folder, fzf representa mal los saltos de línea.
[[ -z $remote_folder || -z $server ]] && echo "Se necesita carpeta del servidor remoto y proveedero del servicio donde subir los archivos." && exit

# Detectar qué tipo de proyecto tenemos.
if [[ -z $pl ]]; then
    echo "No Hemos elegido lenguaje de programación, vamos a detectarlo."
    clj_ls=$(find . -type f -name "*.clj" | wc -l)
    py_ls=$(find . -type f -name "*.py" | wc -l)
    sc_ls=$(find . -type f -name "*.scala" -o -name "*.sc" | wc -l)
    js_ls=$(find . -type f -name "*.js" -o -name "*.tsx" | wc -l)
    ts_ls=$(find . -type f -name "*.ts" -o -name "*.tsx" | wc -l)
    n_files=($py_ls $clj_ls $sc_ls $js_ls $ts_ls)

    max_idx=0
    max=${n_files[$max_idx]}
    for (( i=1; i<=${#n_files[*]} ;i++ )); do
        if [[ ${n_files[$i]} > $max ]]; then
            max=${n_files[$i]}
            max_idx=$i
        fi
    done

    pl=${pls[$max_idx]}
    echo "Lenguaje elegido: " $pl
fi

existing_pl=$(in_array $pl)
[[ $existing_pl == 0 ]] && echo "Lenguaje no contemplado." && exit

# TODO: Hay que mejorar, y mucho, cómo compruebo o no si se han pasado los tests.
# Ejecutamos tests y comprobamos la salida para ver que los tests son correctos.
is_ok=0
if [[ $pl == clojure ]]; then
    while read -r line; do
        echo $line
        is_ok=$(echo "$line" | grep -ow "0 failures, 0 errors." | wc -l)
        if [[ $is_ok == 1 ]]; then
            break
        fi
    done <<< $(${test_command[$pl]})
fi

# Si tests OK, subimos.
if [[ $is_ok == 1 ]]; then
    echo "Subimos al destino."
    if [[ -n $exclude_file && -n $server ]]; then
        upload_folder ./ $remote_folder -e $exclude_file -s $server
    elif [[ -n $exclude_file ]]; then
        upload_folder ./ $remote_folder -e $exclude_file
    elif [[ -n $server ]]; then
        upload_folder ./ $remote_folder -s $server
    else
        upload_folder ./ $remote_folder
    fi
fi
