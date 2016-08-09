#!/bin/bash

# disable path name expansion or * will be expanded in the line
# cmd=( $line )
set -f

monitor=${1:-0}
geometry=( $(herbstclient monitor_rect "$monitor") )
if [ -z "$geometry" ] ;then
    echo "Invalid monitor $monitor"
    exit 1
fi
# geometry has the format: WxH+X+Y
x=${geometry[0]}
y=${geometry[1]}
panel_width=${geometry[2]}
panel_height=16
# font="-*-clean-*-*-*-*-15-*-*-*-*-*-*-*"
font="-*-fixed-medium-*-*-*-12-*-*-*-*-*-*-*"
# 2nd item options: works -> *|fixed|clean| fails -> profont|terminus|courier
# 3rd item options: medium|bold|*

bgcolor=$(herbstclient get frame_border_normal_color)
selbg=$(herbstclient get window_border_active_color)
selfg='#101010'

####
# Try to find textwidth binary.
# In e.g. Ubuntu, this is named dzen2-textwidth.
if [ -e "$(which textwidth 2> /dev/null)" ] ; then
    textwidth="textwidth";
elif [ -e "$(which dzen2-textwidth 2> /dev/null)" ] ; then
    textwidth="dzen2-textwidth";
else
    echo "This script requires the textwidth tool of the dzen2 project."
    exit 1
fi

####
# true if we are using the svn version of dzen2
# kdd: no longer necessary as the dzen2_version doesn't seem to matter any more - see modification below - July 2016
#dzen2_version=$(dzen2 -v 2>&1 | head -n 1 | cut -d , -f 1|cut -d - -f 2)
#if [ -z "$dzen2_version" ] ; then
#    dzen2_svn="true"
#else
#    dzen2_svn=""
#fi

function uniq_linebuffered() {
    awk '$0 != l { print ; l=$0 ; fflush(); }' "$@"
}

herbstclient pad $monitor $panel_height
{
    # events:
    # time
    while true ; do
        date +'date ^fg(#efefef)%H:%M%P %A %Y-%m-%d^fg(#efefef)'
        sleep 1 || break
    done > >(uniq_linebuffered)  &
    # custom pymodoro event
    while true ; do
        echo "pymodoro"
        sleep 1 || break
    done &
    childpid=$!
    herbstclient --idle
    kill $childpid
} 2> /dev/null | {
    TAGS=( $(herbstclient tag_status $monitor) )
    visible=true
    date=""
    windowtitle=""
    pymodoro="$(exec pymodoro -o)" # kdd July 2016 - setting it's value rightaway to avoid the shifting of the tags on the panel
    while true ; do
        bordercolor="#26221C"
        separator="^bg()^fg($selbg)|"
        # update pymodoro status
        echo -n "^fg(#efefef)$pymodoro " # kdd July 2016
        # draw tags
        echo -n "$separator" 
        for i in "${TAGS[@]}" ; do
            case ${i:0:1} in
                '#')
                    echo -n "^bg($selbg)^fg($selfg)"
                    ;;
                '+')
                    echo -n "^bg(#9CA668)^fg(#141414)"
                    ;;
                ':')
                    echo -n "^bg()^fg(#ffffff)"
                    ;;
                '!')
                    echo -n "^bg(#FF0675)^fg(#141414)"
                    ;;
                *)
                    echo -n "^bg()^fg(#ababab)"
                    ;;
            esac
            # kdd: this check was problematic; commented out as it is unnecessary - 2015
            #if [ ! -z "$dzen2_svn" ] ; then
            #    echo -n "^ca(1,herbstclient focus_monitor $monitor && "'herbstclient use "'${i:1}'") '"${i:1} ^ca()"
            #else
                # The problematic line
                # echo -n " ${i:1} "
                # Had to use the same line as in in the if clause
            #    echo -n "^ca(1,herbstclient focus_monitor $monitor && "'herbstclient use "'${i:1}'") '"${i:1} ^ca()"
            #fi
            echo -n "^ca(1,herbstclient focus_monitor $monitor && "'herbstclient use "'${i:1}'") '"${i:1} ^ca()"
        done
        echo -n "$separator"
        echo -n "^bg()^fg() ${windowtitle//^/^^}"

        # small adjustments
        right="$date"
        right_text_only=$(echo -n "$right"|sed 's.\^[^(]*([^)]*)..g')
        # get width of right aligned text.. and add some space..
        width=$($textwidth "$font" "$right_text_only    ")
        # echo -n "$width" # kdd test variable
        echo -n "^pa($(($panel_width - $width)))$right"
        echo
        # wait for next event
        read line || break
        cmd=( $line )
        # find out event origin
        case "${cmd[0]}" in
            tag*)
                #echo "reseting tags" >&2
                TAGS=( $(herbstclient tag_status $monitor) )
                ;;
            date)
                #echo "reseting date" >&2
                date="${cmd[@]:1}"
                ;;
            pymodoro) # kdd July 16
                #echo "reseting pymodoro" >&2
                pymodoro="$(exec pymodoro -o -t -st ~/LTF/Pymodoro-alarms/tick-light.wav -sp ~/LTF/Pymodoro-alarms/alarm-treble.wav -sb ~/LTF/Pymodoro-alarms/alarm-treble-longer.duration.wav)"
                ;;
            quit_panel)
                exit
                ;;
            togglehidepanel)
                currentmonidx=$(herbstclient list_monitors |grep ' \[FOCUS\]$'|cut -d: -f1)
                if [ -n "${cmd[1]}" ] && [ "${cmd[1]}" -ne "$monitor" ] ; then
                    continue
                fi
                if [ "${cmd[1]}" = "current" ] && [ "$currentmonidx" -ne "$monitor" ] ; then
                    continue
                fi
                echo "^togglehide()"
                if $visible ; then
                    visible=false
                    herbstclient pad $monitor 0
                else
                    visible=true
                    herbstclient pad $monitor $panel_height
                fi
                ;;
            reload)
                exit
                ;;
            focus_changed|window_title_changed)
                windowtitle="${cmd[@]:2}"
                ;;
         esac
    done
} 2> /dev/null | dzen2 -w $panel_width -x $x -y $y -fn "$font" -h $panel_height \
    -ta l -bg "$bgcolor" -fg '#efefef' -e 'button2=;'
