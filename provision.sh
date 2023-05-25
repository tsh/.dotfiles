# xclip to allow access to system clipboard from nvim on ubuntu 18
sudo apt install xclip

# install NerdFont for nvim. 
# Note: Use this font in terminal for nvim to work
mkdir -p ~/.local/share/fonts
cd ~/.local/share/fonts && curl -fLo "Droid Sans Mono for Powerline Nerd Font Complete.otf" https://github.com/ryanoasis/nerd-fonts/raw/HEAD/patched-fonts/DroidSansMono/complete/Droid%20Sans%20Mono%20Nerd%20Font%20Complete.otf

sudo apt install snapd

# Neovim specific
snap install nvim
sudo apt-get install ripgrep
# vim-plug
curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
nvim --headless +PlugInstall +qall

