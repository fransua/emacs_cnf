# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

#xmodmap /home/francisco/.emacs.d/ctrlcapswap
# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoredups

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

if [ `whoami` = 'root' ]; then
    export PSmy='${debian_chroot:+($debian_chroot)}\[\033[00;32m\]\H\[\033[00m\] \[\033[01;34m\]\w/\[\033[31m\]\$\[\033[00m\] '
else
    export PSmy='${debian_chroot:+($debian_chroot)}\[\033[00;32m\]\H\[\033[00m\] \[\033[01;34m\]\w/\[\033[00m\]\$ '
fi
# If this is an xterm set the title to user@host:dir
case "$TERM" in
    xterm*|rxvt*)
	PROMPT_COMMAND='echo -ne "\033]0;${HOSTNAME}: ${PWD/$HOME/~}\007";'
	;;
    *)
	;;
esac

function _git_prompt() {
    local git_status="`git status -unormal 2>&1`"
    if ! [[ "$git_status" =~ Not\ a\ git\ repo ]]; then
        if [[ "$git_status" =~ nothing\ to\ commit ]]; then
            local ansi=42
        elif [[ "$git_status" =~ nothing\ added\ to\ commit\ but\ untracked\ files\ present ]]; then
            local ansi=43
        else
            local ansi=45
        fi
        if [[ "$git_status" =~ On\ branch\ ([^[:space:]]+) ]]; then
            branch=${BASH_REMATCH[1]}
            test "$branch" != master || branch=' '
        else
            # Detached HEAD.  (branch=HEAD is a faster alternative.)
            branch="(`git describe --all --contains --abbrev=4 HEAD 2> /dev/null ||
                echo HEAD`)"
        fi
        echo -ne '\[\e[0;37;'"$ansi"';1m\]'"$branch"'\[\e[0m\] '
    fi
}
function _prompt_command() {
    PS1="`_git_prompt`"$PSmy
}

# ads to prompt command the command to set terminal title
case "$TERM" in
    xterm*|rxvt*)
	PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007";_prompt_command'
	;;
    *)
	PROMPT_COMMAND=_prompt_command	
	;;
esac


#echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"

# enable color support of ls and also add handy aliases
if [ "$TERM" != "dumb" ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
fi

# some more ls aliases
alias ll='ls -lh'
alias la='ls -A'
alias lt='ls -ltr'
alias ssh='ssh -X'
alias l='ls -CF -1'
alias g='egrep -i --color'
alias j='jobs'
alias h='head -n 25'
alias grep="grep --color"
alias t='tail -n 25'
alias ssh='ssh -X'
alias pl='perl'
alias rm="rm -i" 
alias mv="mv -i" 
alias cp="cp -i" 


## HISTORY
export HISTSIZE=100000          # big big history

# share same history in all root shells
#export PROMPT_COMMAND=$PROMPT_COMMAND:'history -a;history -n;'
shopt -s histappend

# add date+time to commands in history
HISTTIMEFORMAT="%m/%h - %H:%M:%S   "

# queue system qstat: 
#. /opt/sge/default/common/settings.sh
if [[ -e /opt/sge/default/common/settings.sh ]]; then
    . /opt/sge_settings.sh
    alias qfinish="qstat -f -s prszh"
    alias qall='qstat -u "*"'
    alias qerror='qstat -explain E -j'

    qtodo() { qstat -u `whoami` | grep " $1" | grep ' qw '| sed "s/.* //"| sed "s/:1//" | awk -F'-' '{print $2 - $1}'; }
    qokupied() { qstat -u `whoami` -j $1 | grep usage | wc -l; }
    qmean() { qacct -o `whoami` -j $1 | grep ru_wallclock | cut -d' ' -f2 | awk '{TOTAL+=$1} END{printf("doneJobs:_%d,_in_meanTime:_%dh_%dm_%ds_(%dsec) %d\n",NR,int((TOTAL/NR)/3600),((TOTAL/NR/3600)-int(TOTAL/NR/3600))*60,TOTAL/NR-(60*int(TOTAL/NR/60)),TOTAL/NR,TOTAL/NR)}' ;}
    qrest() { qstat; echo `qmean $1` `qtodo $1` `qokupied $1` | awk '{REST = ($3)*($2)/($4)} END {printf( $1 "\nresting time %dd %dh %dm %ds (= %dsec = %djobs todo in %dsec/%dcpus)\n"),int(REST)/86400,int((REST)/3600)-(int(REST/86400)*24),((REST/3600)-int(REST/3600))*60,REST-(60*int(REST/60)),REST,$3,$2,$4}'| sed "s/_/ /g"; }

    qalltodo() { qstat -u $1 | grep " $2" | grep ' qw '| sed "s/.* //"| sed "s/:1//" | awk -F'-' '{print $2 - $1}'; }
    qallokupied() { qstat -u $1 -j $2 | grep usage | wc -l; }
    qallmean() { qacct -o $1 -j $2 | grep ru_wallclock | cut -d' ' -f2 | awk '{TOTAL+=$1} END{printf("doneJobs:_%d,_in_meanTime:_%dh_%dm_%ds_(%dsec) %d\n",NR,int((TOTAL/NR)/3600),((TOTAL/NR/3600)-int(TOTAL/NR/3600))*60,TOTAL/NR-(60*int(TOTAL/NR/60)),TOTAL/NR,TOTAL/NR)}' ;}
    qallrest() { qstat -u $1; echo `qallmean $1 $2` `qalltodo $1 $2` `qallokupied $1 $2` | awk '{REST = ($3)*($2)/($4)} END {printf( $1 "\nresting time %dd %dh %dm %ds (= %dsec = %djobs todo in %dsec/%dcpus)\n"),int(REST)/86400,int((REST)/3600)-(int(REST/86400)*24),((REST/3600)-int(REST/3600))*60,REST-(60*int(REST/60)),REST,$3,$2,$4}'| sed "s/_/ /g"; }
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

