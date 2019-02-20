Using [GNU stow](https://www.gnu.org/software/stow/) for my personal dot files in ubuntu while abiding with this [article](http://brandon.invergo.net/news/2012-05-26-using-gnu-stow-to-manage-your-dotfiles.html)

# Installation on ubuntu
Install stow:
```
sudo apt-get install stow
```
Clone repo:
```
cd $HOME
git clone https://github.com/YasserKa/dotFiles.git
```
Enter dotfiles directory and use stow to create linked files at $HOME
example:
```
cd dotfiles
stow vim
```
