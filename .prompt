### Details at:
### https://wiki.archlinux.org/index.php/Color_Bash_Prompt
###
### Git info:
### https://fedoraproject.org/wiki/Git_quick_reference

#PS1='[\u@\h \W]\$ '  # Default
PS1='\[\e[1;36m\][\u :: \[\e[1;34m\]\W\[\e[1;36m\]$(declare -F __git_ps1 &>/dev/null && __git_ps1 " (%s)")]\$\[\e[0m\] '

#export PS1='[\u@\h \W$(declare -F __git_ps1 &>/dev/null && __git_ps1 " (%s)")]\$ '

################################################################
### From Adrian M. Price-Whelan
### https://github.com/adrn/dotfiles/blob/master/bash/prompt
################################################################
# Bash prompt configuration, check my post:
# http://apwhelan.blogspot.com/2012/08/bash-prompt-coloring.html
#
# See link for fill reference:
# http://bash-hackers.org/wiki/doku.php/scripting/terminalcodes
#
# \033 indicates a control character is to follow
#
# For full list, "man bash" and search to 2nd "PROMPTING" or see:
# http://tldp.org/HOWTO/Bash-Prompt-HOWTO/bash-prompt-escape-sequences.html
# also: http://www.yolinux.com/HOWTO/Bash-Prompt-HOWTO.html
#
# \u : username
# \h : host to first "."
# \w : current full working directory
# \W : last part of path

# See: http://networking.ringofsaturn.com/Unix/Bash-prompts.php
# for a longer list of colours.

COL_BLACK="\[\033[0;30m\]"
COL_BLUE="\[\033[1;34m\]"
COL_GREEN="\[\033[0;32m\]"
COL_CYAN="\[\033[0;36m\]"
COL_RED="\[\033[0;31m\]"
COL_PURPLE="\[\033[0;35m\]"
COL_BROWN="\[\033[0;33m\]"
COL_LIGHT_GRAY="\[\033[0;37m\]"
COL_DARK_GRAY="\[\033[1;30m\]"
COL_BOLD_BLUE="\[\033[1;34m\]"
COL_BOLD_GREEN="\[\033[1;32m\]"
COL_BOLD_CYAN="\[\033[1;36m\]"
COL_BOLD_RED="\[\033[1;31m\]"
COL_BOLD_PURPLE="\[\033[1;35m\]"
COL_YELLOW="\[\033[1;33m\]"
COL_WHITE="\[\033[1;37m\]"
PMPT_BOLD=$'\[\033[1m\]'
PMPT_NORM=$'\[\033[0m\]'



# Credit for these functions goes to
# https://github.com/mathiasbynens/dotfiles/blob/master/.bash_prompt
function parse_git_dirty() {
    TMP=$(git status 2> /dev/null | tail -n1);
    if [[ $TMP != "nothing to commit, working directory clean" ]]; then
        echo "(!) ";
    else
        echo "";
    fi
    return;
}

function parse_git_branch() {
    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/\1/"
}


#export PS1="$SCREEN_TITLE$LIGHT_GRAY\h$RESET $CYAN[\w]$GREEN \$([[ -n \$(git branch 2> /dev/null) ]] && echo \"git:\")\$(parse_git_branch)$RED \$([[ -n \$(git branch 2> /dev/null) ]] && echo \"\$(parse_git_dirty)\")$WHITE% $RESET"

# For multi-line commands:
#export PS2="$GREEN> $RESET"


#export PS1="${PMPT_BOLD}${COL_RED}\h${COL_NORM} [\w] %${PMPT_NORM} "

#export PS1="${COL_BLUE}[\u${COL_RESET} \W %${PMPT_NORM} "

#[\u@\h \W]\$


