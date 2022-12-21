# xclip to allow access to system clipboard from nvim on ubuntu 18
sudo apt install xclip

# === NIX ===
# Install Nix
sh <(curl -L https://nixos.org/nix/install) --daemon

# Install Packages
nix-env -iA \
	nixpkgs.zsh \
	nixpkgs.git \
	nixpkgs.neovim \
	nixpkgs.tmux \
	nixpkgs.stow \
	nixpkgs.fzf \
	nixpkgs.ripgrep \
	nixpkgs.bat \
	nixpkgs.direnv
	
# === END NIX ===

# vim-plug
curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
nvim --headless +PlugInstall +qall

# emacs
systemctl enable --user emacs
systemctl start --user emacs
