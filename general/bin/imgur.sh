#!/usr/bin/env bash

client_id=$IMGUR_CLIENT_ID

# Check arguments
function upload {
    if [ "$1" == "-i" ]; then
        curl --location --request POST "https://api.imgur.com/3/upload" --header "Authorization: Client-ID $client_id" --form "image=$2"
    elif [ "$1" == "-v" ]; then
        curl --location --request POST "https://api.imgur.com/3/upload" --header "Authorization: Client-ID $client_id" --form "video=$2"
    fi
}

response=$(upload $1 "@$2") 2>/dev/null

link=$(echo $response | jq '.data.link' | sed 's/"//g')

echo $link | xclip -selection clipboard
