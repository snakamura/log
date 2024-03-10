# Communicating over TCP using bash

`bash` supports TCP connection using pseudo device files like `/dev/tcp/<host>/<port>`. Once you create a file descriptor and associate it with this device file, you can communicate over TCP just with `bash`.

    # Open a new connection to www.google.com:80
    # and associate it with file descriptor 3
    exec 3<> /dev/tcp/www.google.com/80

    # Send a request
    cat >&3 << EOF
    GET / HTTP/1.1
    Host: www.google.com
    Connection: close

    EOF

    # Read a response
    cat <&3

    # Close the file descriptor
    exec 3>&-
