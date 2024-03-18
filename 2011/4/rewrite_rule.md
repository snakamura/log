# B flag for RewriteRule doesn't work in a path

[B flag](http://httpd.apache.org/docs/2.2/rewrite/flags.html#flag_b) can be used to let mod_rewrite escape back references, but since it escapes a space to `+`, it cannot be used when a back reference is used in a path.

For example, assuming you use this rule:

    RewriteRule ^foo/(.+)$ http://example.com/bar/$1 [R=301,L,NE,B]
    RewriteRule ^bar/(.+)$ foo.cgi?name=$1 [L,QSA,B]

When a user visit http://example.com/foo/x%20y, he will be redirected to http://example.com/bar/x+y, then it'll be rewritten to foo.cgi?name=x%2by.

This is because `+` in a query string will be unescaped to a space, but not in a path. To avoid this problem, you have to write your own escape program, and use it like:

    RewriteMap e prg:/path/to/escape
    RewriteRule ^foo/(.+)$ http://example.com/bar/${e:$1} [R=301,L,NE]
    RewriteRule ^bar/(.+)$ foo.cgi?name=$1 [L,QSA,B]
