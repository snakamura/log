# xterm-256color on Ubuntu

As I upgraded my Mac to Lion, I found that I got an error that says:

    Cannot find terminfo entry for 'xterm-256color'.

when I launched `screen` on my Ubuntu server. This is because the default term name has changed from `xterm-color` to `xterm-256color`.

To install terminfo for `xterm-256color`, install `ncurses-term` package on your Ubuntu.
