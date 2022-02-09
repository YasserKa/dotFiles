#!/usr/bin/env sh
alias | awk -F'[ =]' '{print $2}'
