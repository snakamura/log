# Here strings are seekable

Imagine you have a command that read from a specified file and do something and you'd like to pipe to this command, but the command doesn't support reading from stdin. In this case, you can use `/dev/stdin`,

    your_command | the_command /dev/stdin

or use process substitution if you're using bash.

    the_command <(your_command)

Unfortunately, this doesn’t work if the command seeks on the specified file because pipes don’t support being seeked. In this case, you can use here strings. They’re seekable.

    the_command /dev/stdin <<<$(your_command)

I found this when I tried to get an application identifier from a provisioning profile for iOS app.

    /usr/libexec/PlistBuddy -c "Print :Entitlements:application-identifier" /dev/stdin <<< $(/usr/bin/security cms -D -i MyApp.mobileprovision) | sed -e 's/[^.]*\.//'
