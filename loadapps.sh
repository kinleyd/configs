#!/bin/bash

# kdd layouting, works in terminal, not in script
# monitor = physical screen or logical screen if defined
# frame = division of a monitor, created by split; if removed, windows get absorbed in nearest frame
# window = an open application, also referred to as client
# tag = workspace, virtual desktop or window group
# focus_nth INDEX - Focuses nth window in a frame
# use TAG - Switches the focused monitor to specified TAG.
# use_index INDEX [--skip-visible] - Switches the focused monitor to the TAG with the specified INDEX
# move TAG - Moves the focused window to tag named TAG
# move_index INDEX [--skip-visible] - Moves the focused window to the tag specified by INDEX

function hc() {
    herbstclient "$@"
}

function spawn_in_frame() {(
        herbstclient rule once "index=$1"
        shift
        exec "$@"
        ) &
}

# Will succeed in creating attribute my_apps_are_loaded once;
# subsequently will fail to recreate it and thereby not rerun the following code
# quirk: the "my_" prefix is required!
if herbstclient silent new_attr bool my_apps_are_loaded ; then
    
    hc focus_monitor 0 && # centre monitor

        # setup tag
        hc use urxvt &&

        spawn_in_frame 01100 urxvt && 
        sleep 1 &&
        #spawn_in_frame 011011 emacsclient -s server -c /home/kdd/ps.ledger &&
        #sleep 1 &&
        spawn_in_frame 011011 urxvt && 
        sleep 1 &&
        spawn_in_frame 011011 urxvt && 
        sleep 1 &&
        spawn_in_frame 11001 emacsclient -s server -c /home/kdd/LTF/Code-Complete/ps/src/ps/workspace.clj &&
        #google-chrome-stable &&
        sleep 2 &&

        # setup tag
        hc use zen-mode &&
        spawn_in_frame 101 emacsclient -s server -c -e "(kdd-setup-zen-window)" &&
        sleep 1 &&


        hc focus_monitor 1 && # left monitor

        # setup tag
        hc use browser &&

        spawn_in_frame 0110 firefox && 
        sleep 2 &&
        spawn_in_frame 11010 urxvt -e ncmpcpp &&
        sleep 1 &&
        spawn_in_frame 1101110 conky -q &&
        sleep 1 &&

        hc focus_monitor 2 && # right monitor

        # setup tag
        hc use emacs &&
        hc spawn emacsclient -s server -c -e "(kdd-setup-todo-windows)" && 
        sleep 1 &&

        # more emacs examples
        # spawn_in_frame 01 emacsclient -s server -nc &&
        #spawn_in_frame 01 emacsclient -s server -nc /home/kdd/ps.ledger &&

        # layout settings don't render properly without another reload
        hc reload
fi
