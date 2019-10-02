#!/bin/sh
tmux new-session -d -s session1 -n "Window1" -d csh
tmux split-window -v 'csh'
tmux split-window -h 'csh'
tmux new-window 'csh'
tmux rename-window -t 1 "window2"
tmux send -t session1:0.0  "ls -al" ENTER
tmux send -t session1:0.0  "printf '\033]2;%s\033\\' 'Window1:pane 1'" ENTER
tmux send -t session1:0.1  cd ENTER
tmux send -t session1:0.1 "printf '\033]2;%s\033\\' 'Windo1:pane 2'" ENTER
tmux send -t session1:0.2  ls ENTER
tmux send -t session1:0.2 "printf '\033]2;%s\033\\' 'Windo1:pane 3'" ENTER
tmux send -t session1:1.0  ls ENTER
tmux send -t session1:1.0  "printf '\033]2;%s\033\\' 'Window2:pane 1'" ENTER
tmux -2 attach-session -d
