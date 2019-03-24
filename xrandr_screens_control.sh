if [[ -z $(xrandr --query | grep "VGA1 connected") ]]; then
    xrandr --output VGA1
    xrandr --output eDP1 --off
else
    xrandr --output eDP1
fi

# Ambas funcionando
# xrandr --output VGA1 --left-of eDP1 --output VGA1 --primary
