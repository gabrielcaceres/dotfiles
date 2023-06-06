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
alias lsd="ls | grep /$"	# Show only directories
alias lsf="ls | grep -v /$"	# Show only files
alias lld="ll | grep /$"	# List only directories
alias llf="ll | grep -v /$"	# List only files
alias lltf="llt | grep -v /$"	# List only files sorted by timestamp
alias llsf="lls | grep -v /$"	# List only files sorted by size

# Ask when removing many files
alias rm="rm -I"

# List line conut for file
alias wcl="wc -l"


# function h () {
#     if [[ $# -eq 2 ]]
#     then
#         head -n $1 $2
#     else
#         head -n 5 $1
#     fi
# }
# function t () {
#     if [[ $# -eq 2 ]]
#     then
#         tail -n $1 $2
        
#     else
#         tail -n 5 $1
#     fi
# }
# function ht () {
#     if [[ $# -eq 2 ]]
#     then
#         head -n $1 $2
#         echo "---------------"
#         tail -n $1 $2
#     else
#         head -n 5 $1
#         echo "---------------"
#         tail -n 5 $1
#     fi
# }


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
    :                           # pass
else
    # Unknown.
    echo "Unknown OS"
fi


# Functions to start and stop conda virtual environments
startenv () {
    # Use name of git root directory as target env
    conda activate $(basename "$(git rev-parse --show-toplevel)")
}
stopenv () {
    conda deactivate
}
