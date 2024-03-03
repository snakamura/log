# Configure Squid to use Basic authentication

First, create a file containg credentials.

```
$ htpasswd -c /usr/local/etc/squid.htpasswd
New password:
Re-type new password:
```

Then, comment out these lines in `/usr/local/etc/squid.conf`.

```
#http_access allow localnet
#http_access allow localhost
```

Add these lines there instead.

```
auth_param basic program /usr/local/opt/squid/libexec/basic_ncsa_auth /usr/local/etc/squid.htpasswd
auth_param basic children 5 startup=5 idle=1
auth_param basic realm Squid proxy-caching web server
auth_param basic credentialsttl 2 hours
acl basic_ncsa proxy_auth REQUIRED
http_access allow basic_ncsa
```

Restart squid.

```
brew services restart squid
```
