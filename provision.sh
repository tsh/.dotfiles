# xclip to allow access to system clipboard from nvim on ubuntu 18
sudo apt install xclip

sudo apt install snapd
snap install nvim

# vim-plug
curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
nvim --headless +PlugInstall +qall

