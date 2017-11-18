
emq-reloader
============

Erlang Module Reloader for Development

NOTICE: Don't load the plugin in production.

Configure Plugin
----------------

File: etc/emq_reloader.conf

```
reloader.interval = 60

reloader.logfile = log/reloader.log
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

