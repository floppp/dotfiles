#!/bin/bash

# echo $1
# echo $2
# echo $3
pattern=$1
glob_expr=$2
substitution=$3

#### Arreglamos contrabarras en caso de haberlas.
# Podríamos usar (ej de tldr):
#     sed 's#{{find}}#{{replace}}#' {{filename}}
# pero para no tener que estar ajustando el separador o encontrar uno
# universal, dejo esto que me funcionará siempre.
####
sed_pattern=$(echo $pattern | sed -r 's/\//\\\//g')
substitution=$(echo $substitution | sed -r 's/\//\\\//g')
sed_expr=s/$sed_pattern/$substitution/g
sed_expr_print=s/$sed_pattern/$substitution/p


echo "----- Cambios a Realizar -----"
for matched_file in $(grep -lR --exclude-dir={coverage,node_modules,.git} $pattern $glob_pattern); do
    echo "Archivo encontrado: $matched_file"
    echo "de ->"
    grep $pattern $matched_file
    echo "a  <-"
    sed -rn $sed_expr_print $matched_file
    echo ""
done

read -p "Está de acuerdo con los cambios? [y/n] " user_selection

case $user_selection in
    ([yY])
        # grep -l "/services/adt" **/*.ts | xargs -I_ sed -ir 's/services\/adt/services\/adt\/adt/g' _
        # echo "grep -lR --exclude-dir={coverage,node_modules,.git} $pattern $glob_expr | xargs -I_ sed -ir $sed_expr _"
        grep -l $pattern $glob_expr | xargs -I_ sed -ir $sed_expr _
        ;;
    ([nN])
        echo "Terminamos sin cambiar."
        ;;
    *) echo "Opción incorrecta"
       ;;
esac

