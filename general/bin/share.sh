#!/bin/bash


main_dir="$HOME/Desktop"
target_img_dir="$HOME/Pictures/Shareables"
target_vid_dir="$HOME/Videos/Shareables"

# remove all media from desktop if exists
rm -f $main_dir/[dD]eepin*

mkdir -p $target_img_dir
mkdir -p $target_vid_dir

case $1 in
    -i)   deepin-screenshot -s $HOME/Desktop 2>/dev/null ;;
    -v)   deepin-screen-recorder           ;;
    -t)  xclip -o -selection clipboard | gist -p | xclip -selection clipboard
        dunstify "Saved to Clipboard" ;exit ;;
esac

arg=$1
date=`date +"%Y-%m-%d_%T"`
file_name=`ls $main_dir | grep -i deepin`


extension="${file_name##*.}"

if [[ "$extension" == "" ]]; then
    exit
fi

new_name="$date.$extension"

old_path="$main_dir/$file_name"


if [[ "$extension" == "gif" ]]; then
    # for imgur api request
    arg='-i'
    new_path="$target_vid_dir/$new_name"
elif [[ "$extension" == "vid" ]]; then
    new_path="$target_vid_dir/$new_name"
elif [[ "$extension" == "png" ]]; then
    new_path="$target_img_dir/$new_name"
fi

mv "$old_path" "$new_path"

imgur.sh $arg $new_path
dunstify "Saved to Clipboard"