#!/bin/bash

if [ -z $@ ]
then

    prefix=${PASSWORD_STORE_DIR-~/.password-store}
    password_files=( "$prefix"/**/*.gpg )
    password_files=( "${password_files[@]#"$prefix"/}" )
    password_files=( "${password_files[@]%.gpg}" )

    printf '%s\n' "${password_files[@]}"

else

    pass_name=$@
    pass show --clip "$pass_name" &> /dev/null

fi
