## TODO figure out what needs to be installed
## TODO Add logic to check OS
## Git status in prompt
#source /usr/share/git-core/contrib/completion/git-prompt.sh
## import terminal prompt customizations
#source ~/.prompt

# set default editor to emacs
export EDITOR=emacs
export VISUAL=$EDITOR

alias em="emacs"


# ## Git status in prompt
# source /usr/share/git-core/contrib/completion/git-prompt.sh
# ## import terminal prompt customizations
# source ~/.prompt

# ## Git status in prompt
# source ~/.git_prompt.sh
# ## import terminal prompt customizations
# source ~/.prompt



### ls aliases
# Relevant flags (see `man ls` for more details):
# -A : do not list . and ..
# --color=always : enable colors
# -d : list only directories, not contents
# -F : display symbol with additional info for directories or files
# -g : like -l but don't list owner
# -h : human readable file size
# -l : display as list
# -o : like -l but don't list group
# -S : sort by size
# -t : sort by timestamp
alias ls="ls --color=always -F"	# Default color and label folders
alias ll="ls -Fhgo"		# List
alias la="ll -A"		# List hidden
alias lla="la"			# List hidden
alias llt="ll -t"		# List sorted by timestamp
alias lls="ll -S"		# List sorted by size
# List only files/directories
# see: https://unix.stackexchange.com/questions/1645/is-there-any-option-with-ls-command-that-i-see-only-the-directories
# Note that `-p` doesn't work on Mac, and `-d` suggestion from link
# prduces an error if no directories are present
# can add `2> /dev/null` to redirect error for latter.
# Just using grep for both files and dirs
alias lsd="ls | \grep /$"	# Show only directories
alias lsf="ls | \grep -v /$"	# Show only files
alias lld="ll | \grep /$"	# List only directories
alias llf="ll | \grep -v /$"	# List only files
alias lltf="llt | \grep -v /$"	# List only files sorted by timestamp
alias llsf="lls | \grep -v /$"	# List only files sorted by size

# Always highlight grep search term
alias grep='grep --color=auto'

# Ask when removing many files
alias rm="rm -I"

# List line conut for file
alias wcl="wc -l"


function h () {
    if [[ $# -eq 2 ]]
    then
        head -n $1 $2
    else
        head -n 5 $1
    fi
}
function t () {
    if [[ $# -eq 2 ]]
    then
        tail -n $1 $2
        
    else
        tail -n 5 $1
    fi
}
function ht () {
    if [[ $# -eq 2 ]]
    then
        head -n $1 $2
        echo "---------------"
        tail -n $1 $2
    else
        head -n 5 $1
        echo "---------------"
        tail -n 5 $1
    fi
}


# # Homebrew autocompletion
# if type brew 2&>/dev/null; then
#   for completion_file in $(brew --prefix)/etc/bash_completion.d/*; do
#     source "$completion_file"
#   done
# fi


# # bash-completion2
# # export BASH_COMPLETION_COMPAT_DIR="/usr/local/etc/bash_completion.d"
# [[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"


# Add the following to your ~/.bash_profile:
#   [[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"

# If you'd like to use existing homebrew v1 completions, add the following before the previous line:
  # export BASH_COMPLETION_COMPAT_DIR="/usr/local/etc/bash_completion.d"

# also see: https://discourse.brew.sh/t/bash-completion-2-vs-brews-auto-installed-bash-completions/2391/2


# # Git prompt
# if [ -f "/usr/local/opt/bash-git-prompt/share/gitprompt.sh" ]; then
#     __GIT_PROMPT_DIR="/usr/local/opt/bash-git-prompt/share"
#     source "/usr/local/opt/bash-git-prompt/share/gitprompt.sh"
# fi


# Bash completion
# https://github.com/scop/bash-completion
# https://formulae.brew.sh/formula/bash-completion@2
# Linux
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    # https://github.com/magicmonty/bash-git-prompt
    if [[ -f "$HOME/.bash-git-prompt/gitprompt.sh" ]]; then
        # GIT_PROMPT_ONLY_IN_REPO=1
        source $HOME/.bash-git-prompt/gitprompt.sh
    fi
    :    # pass
    # Linux config
    # Use bash-completion, if available
    # Don't think it's necessary if installed directly using dnf?
    # [[ $PS1 && -f /usr/share/bash-completion/bash_completion ]] && \
	# . /usr/share/bash-completion/bash_completion
    # ### Blur background behind terminal
    # # Konsole
    # konsolex=$(qdbus | grep konsole | cut -f 2 -d\ )
    # if [ -n konsolex ]; then
    # 	for konsole in $konsolex
    # 	do
    # 	    xprop -f _KDE_NET_WM_BLUR_BEHIND_REGION 32c -set _KDE_NET_WM_BLUR_BEHIND_REGION 0 -id `qdbus $konsole /konsole/MainWindow_1 winId`;
    # 	done
    # fi
    # # Yakuake
    # if [ `qdbus | grep yakuake` ]; then
    # 	xprop -f _KDE_NET_WM_BLUR_BEHIND_REGION 32c -set _KDE_NET_WM_BLUR_BEHIND_REGION 0 -name Yakuake;
    # fi
elif [[ "$OSTYPE" == "darwin"* ]]; then
    # Mac OSX
    if [ -f "$(brew --prefix)/opt/bash-git-prompt/share/gitprompt.sh" ]; then
        __GIT_PROMPT_DIR=$(brew --prefix)/opt/bash-git-prompt/share
        GIT_PROMPT_ONLY_IN_REPO=0
        source "$(brew --prefix)/opt/bash-git-prompt/share/gitprompt.sh"
    fi
    [[ -r "$HOMEBREW_PREFIX/etc/profile.d/bash_completion.sh" ]] && . "$HOMEBREW_PREFIX/etc/profile.d/bash_completion.sh"
else
    # Unknown.
    echo "Unknown OS"
fi


##### Git related functions

# Get current git root directory, otherwise return nothing
cur_git_root () {
    git rev-parse --show-toplevel 2> /dev/null
}

# cd to git root directory
cdgit () {
    if [[ -z "$(cur_git_root)" ]]; then
        echo "Not in a git directory"
    else
        cd "$(cur_git_root)"
    fi
}


##### Virtual environment related functions (using conda)
check_in_conda_env_list () {
    conda env list | grep "/$1$"
}

venv_list () {
    conda env list
}

# Convenience functions to start and stop conda virtual environments
# using name of git root directory as target env
venv_on () {
    local venv_name
    if [[ -n "$1" ]]; then
        # If argument given, use that as name for venv
        venv_name=$1
    elif [[ -n "$(cur_git_root)" ]]; then
        # Else use name of git repo
        venv_name=$(basename "$(cur_git_root)")
    else
        # Else use current directory name
        venv_name=$(basename "$PWD")
    fi
    if [[ -z  "$(check_in_conda_env_list $venv_name)" ]]; then
        echo "No virtual environment named \"$venv_name\""
    else
        if [[ "$CONDA_SHLVL" > 1 ]]; then
            # Avoid stacking envs
            echo "Turning \"$CONDA_DEFAULT_ENV\" off before starting \"$venv_name\""
            conda deactivate
        fi
        conda activate $(basename "$venv_name")
    fi
}
venv_off () {
    conda deactivate
}


# Give warnings if trying to install where likely shouldn't (e.g. base
# environment or env name that doesn't match git root dir)
safe_install () {
    if [[ "$2" = "install" && ("$CONDA_SHLVL" == 0 || -z "$CONDA_DEFAULT_ENV") ]]; then
        # When installing check if either the conda level is zero or the env name is blank
        echo "Don't install! Conda environment not active!"
        echo "(Run command with a backslash to override alias)"
    elif [[  "$2" = "install" && "$CONDA_DEFAULT_ENV" == "base" ]]; then
        # When installing check if current env is 'base'
        echo "Don't install! Currently in base environment!"
        echo "(Run command with a backslash to override alias)"
    elif [[ "$2" = "install" && -n "$(cur_git_root)" && "$CONDA_DEFAULT_ENV" != $(basename "$(cur_git_root)") ]]; then
        # When installing and in a repo, check env matches repo root dir
        echo "Don't install! Environment name and git root don't match!"
        echo "(Run command with a backslash to override alias)"
    else
        # Else run command as normal
        $@
    fi
}
# Alias safe_install to relevant programs
alias conda="safe_install \conda"
alias mamba="safe_install \mamba"
alias pip="safe_install \pip"
