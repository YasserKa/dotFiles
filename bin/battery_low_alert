#! /bin/bash

battery_level=`acpi -b | grep -P -o '[0-9]+(?=%)'`
is_charging=`acpi -a | grep 'on-line'`

if [ $battery_level -le 10 ] && [ -z "$is_charging" ]
then
    dunstify "Battery low" "Battery level is ${battery_level}%!" --urgency=2
fi
