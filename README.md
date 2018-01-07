
emq-reloader
============

Erlang Module Reloader for Development

NOTICE: Don't load the plugin in production.

Configure Plugin
----------------

File: etc/emq_reloader.conf

```
## Interval of hot code reloading.
##
## Value: Duration
##  - h: hour
##  - m: minute
##  - s: second
##
## Examples:
##  - 2h:  2 hours
##  - 30m: 30 minutes
##  - 20s: 20 seconds
##
## Defaut: 60s
reloader.interval = 60s

## Logfile of reloader.
##
## Value: File
reloader.logfile = reloader.log
```

Load the Plugin
---------------

```
./bin/emqttd_ctl plugins load emq_reloader
```

CLI
---

```
./bin/emqttd_ctl reload <Module>
```

