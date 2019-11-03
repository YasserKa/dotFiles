#!/bin/bash

minutes=5

dunstify "Tea time in 5 minutes"
sleep $((minutes*60))
dunstify "Tea time"
