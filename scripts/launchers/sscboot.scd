o = ServerOptions.new;
o.maxLogins = 100;
s = Server(\myserver, NetAddr("127.0.0.1", 57110), o);
s.boot;
