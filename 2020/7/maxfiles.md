# Changing maxfiles for mongodb on Mac OS

I installed mongodb-community via Homebrew on my Mac OS 10.15.5 and found that mongod sometimes died with `Too many open files` error. This looked strange because the limit seemed to be set to 64,000 in `~/Library/LaunchAgents/homebrew.mxcl.mongodb-community@4.0.plist`.

```
<?xml version="1.0" encoding="UTF-8"?>

<plist version="1.0">
<dict>
  <key>Label</key>
  <string>homebrew.mxcl.mongodb-community@4.0</string>
  <key>ProgramArguments</key>
  <array>
    <string>/usr/local/opt/mongodb-community@4.0/bin/mongod</string>
    <string>--config</string>
    <string>/usr/local/etc/mongod.conf</string>
  </array>
  <key>RunAtLoad</key>
  <true></true>
  <key>KeepAlive</key>
  <false></false>
  <key>WorkingDirectory</key>
  <string>/usr/local</string>
  <key>StandardErrorPath</key>
  <string>/usr/local/var/log/mongodb/output.log</string>
  <key>StandardOutPath</key>
  <string>/usr/local/var/log/mongodb/output.log</string>
  <key>HardResourceLimits</key>
  <dict>
    <key>NumberOfFiles</key>
    <integer>64000</integer>
  </dict>
  <key>SoftResourceLimits</key>
  <dict>
    <key>NumberOfFiles</key>
    <integer>64000</integer>
  </dict>
</dict>
</plist>
```

But it turned out that these limits weren't applied.

To make it work, you'd create `/Library/LaunchDaemons/limit.maxfiles.plist` including `maxfiles` configuration and load it by `launchctl load /Library/LaunchDaemons/limit.maxfiles.plist`

```
<?xml version="1.0" encoding="UTF-8"?>

<plist version="1.0">
  <dict>
    <key>Label</key>
    <string>limit.maxfiles</string>
    <key>ProgramArguments</key>
    <array>
      <string>launchctl</string>
      <string>limit</string>
      <string>maxfiles</string>
      <string>64000</string>
      <string>64000</string>
    </array>
    <key>RunAtLoad</key>
    <true></true>
    <key>ServiceIPC</key>
    <false></false>
  </dict>
</plist>
```

`launchctl limit` shows the current limits.

```
	cpu         unlimited      unlimited
	filesize    unlimited      unlimited
	data        unlimited      unlimited
	stack       8388608        67104768
	core        0              unlimited
	rss         unlimited      unlimited
	memlock     unlimited      unlimited
	maxproc     2784           4176
	maxfiles    64000          64000
```

Then, restart the service by `brew services restart mongodb-community@4.0`.
