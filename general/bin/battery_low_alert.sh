#! /bin/bash

battery_level=`acpi -b | grep -P -o '[0-9]+(?=%)'`

if [ $battery_level -le 10 ]
then
    dunstify "Battery low" "Battery level is ${battery_level}%!" --urgency=2
fi
