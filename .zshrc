# The following lines were added by compinstall
zstyle :compinstall filename '/home/vagrant/.zshrc'



autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -e
# End of lines configured by zsh-newuser-install



# ls coloring
export LSCOLORS=dxfxcxdxbxegedabagacad

if ls --color > /dev/null 2>&1; then # GNU coreutils
  alias ls='ls --color=auto'
  alias grep="grep --color=auto"
else # BSD coreutils
  alias ls='ls -G'
  alias grep='grep -G'
fi

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8

TERM=xterm-256color
