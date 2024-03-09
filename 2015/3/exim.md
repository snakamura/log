# Using Exim in a private network

In some situations, you may want to use Exim to deliver messages to a mail server in a private network. By default, Exim refuses to deliver messages when a DNS server returns a MX record which points to a host whose A record is a private IP address. To make this work, you need to configure [`ignore_target_hosts`](http://www.exim.org/exim-html-3.20/doc/html/spec_27.html#SEC687) option.

On Ubuntu 10.04 LTS, you can find this configuration in `/etc/exim4/conf.d/router/200_exim4-config_primary`. Remove your network from this option and Exim will deliver messages to a mail server in your private network.
