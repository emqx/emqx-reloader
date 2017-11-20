
emqx-reloader
=============

Erlang Module Reloader for Development

NOTICE: Don't load the plugin in production.

Configure Plugin
----------------

File: etc/emqx_reloader.conf

```
reloader.interval = 60

reloader.logfile = log/reloader.log
```

Load the Plugin
---------------

```
./bin/emqx_ctl plugins load emqx_reloader
```

CLI
---

```
./bin/emqx_ctl reload <Module>
```

License
-------

Apache License Version 2.0

Author
------

EMQ X-Men Team.
