# Creating a NAT instance with Ubuntu

AWS moved from Amazon Linux to Amazon Linux 2, and now they've stopped supporting Amazon-Linux-based AMIs. Also, it seems that they won't release an AMI for a NAT instance based on Amazon Linux 2.

You now need to configure your instance by yourself to use it as a NAT instance.

I've created a NAT instance based on Ubuntu 20.04 LTS AMI, and here is what I've done.

1. Create an EC2 instance from Ubuntu 20.04 LTS AMI
2. Disable "Source / destination check"
3. Login to the instance

Now you can configure your instance using `ufw`. It's a wrapper around good old `iptables`. I was kind of surprised that it still uses `iptables` after 25 years since I first launched a linux-based NAT box at home.

First, allow ssh by `sudo ufw allow ssh`.

Next, edit `/etc/default/ufw` and change `DEFAULT_FORWARD_POLICY` from `DENY` to `ACCEPT`.

Next, edit `/etc/ufw/sysctl.conf` and uncomment `net/ipv4/ip_forward=1`.

Now, edit `/etc/ufw/before.rules` to add NAT rules. You'll add these rules at the end.

```
# NAT
*nat
:POSTROUTING ACCEPT [0:0]
-F
-A POSTROUTING -s 10.0.0.0/16 -j MASQUERADE
COMMIT
```

Don't forget to change `10.0.0.0/16` to your network address.

Once you've done all of them, `sudo ufw enable` to enable these rules.
