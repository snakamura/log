# Increase max number of open files of mongodb on Ubuntu with systemd

You can increase the maximum number of open files of a processs spawn by systemd by editing its config file, for example, `/lib/systemd/system/mongod.service`.

But it'd be better to use drop-in file. In this case, create a directory in `/etc/systemd/system` based on the service name, and put a configuration file in it.

For mongodb, you can create `/etc/systemd/system/mongod.service.d/nofile.conf` (the filename itself doesn't matter, but it should have `.conf` extension), and put the following line to increase the number of open files.

```
[Service]
LimitNOFILE=1006500
```

You need to make systemd reload configuration files by `sudo systemctl daemon-reload`, then restart the service by `sudo systemctl restart mongod`.

Use `systemctl status mongod` to check if this configuration was applied properly.

```
systemctl status mongod
● mongod.service - MongoDB Database Server
   Loaded: loaded (/lib/systemd/system/mongod.service; enabled; vendor preset: e
  Drop-In: /etc/systemd/system/mongod.service.d
           └─nofile.conf
   Active: active (running) since Sun 2020-07-12 23:09:14 UTC; 23h ago
     Docs: https://docs.mongodb.org/manual
 Main PID: 32120 (mongod)
   CGroup: /system.slice/mongod.service
           └─32120 /usr/bin/mongod --config /etc/mongod.conf

Jul 12 23:09:14 ip-10-0-11-10 systemd[1]: Started MongoDB Database Server.
```
