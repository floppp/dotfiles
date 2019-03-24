if [[ -z $(xrandr | grep "VGA1 connected") ]]; then
    xrandr --output eDP1
else
    xrandr --output VGA1
    xrandr --output eDP1 --off
fi

# Ambas funcionando
# xrandr --output VGA1 --left-of eDP1 --output VGA1 --primary
