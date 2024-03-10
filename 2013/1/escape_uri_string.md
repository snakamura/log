# Network.URI.escapeURIString converts its input to UTF-8

As of network-2.4.0.0, `Network.URI.escapeURIString` converts its input to UTF-8 bytes before escaping each byte. Earlier versions just escaped each character.

This is basically an improvement, but you should update your code if you converted `String` to UTF-8 bytes by yourself.
