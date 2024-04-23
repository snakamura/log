# `$TERM` becomes unknown when sudoing via ssh

Recently, I found that `$TERM` becomes `unknown` when I told ssh to sudo on a remote server. In short,

    $ ssh localhost sudo /bin/bash -c '"/bin/echo \$TERM"'

prints `unknown` while

    $ ssh localhost /bin/bash -c '"/bin/echo \$TERM"'

prints `dumb`.

I thought it was strange because `sudo` should inherit `TERM` environment variable even when `env_reset` is enabled.

At first, I thought the ssh server set `TERM` environment variable to `dumb`, but it was not. Actually, `TERM` environment variable was even not set. So

    $ ssh localhost env | grep TERM

prints nothing. I looked into the source code of bash and found that the fact is that bash defines `TERM` **shell variable** to `dumb` when it is not set. So, if you use `/bin/sh` (on Ubuntu, it's dash),

    $ ssh localhost /bin/sh -c '"/bin/echo \$TERM"'

prints an empty string.

Although bash defines `TERM` shell variable, it doesn't export it as an environment variable.

Now, I looked into the source code of sudo and found that it sets `TERM` environment variable to `unknown` if it has not been set. That's why $TERM becomes `unknown` in the first example.

As you know, if you pass `-t` to `ssh`, it allocates a pseudo terminal and let the server know its name. So $TERM becomes `xterm` or something based on what terminal you use on the client.
