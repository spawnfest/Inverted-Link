FotonCMS
=======

**FotonCMS** project is intended to deliver an open-source (Apache
  License 2.0, see LICENSE) solution for lightweight hosted stream of
  client's news, posts or whatever. To do so we develop two parts:
  RESTful server and JavaScript widget. Also server will provide
  administrative web interface.

Server
------

	GET /<server>/feeds/<account>/<feed>

to access your feed named "feed", for example:

	GET /fotoncms/feeds/gabriel/news


Installation
------------

To install **FotonCMS** from sources, clone it:

	git clone https://github.com/Spawnfest2012/Inverted-Link.git
	
Then get dependencies with [rebar](http://github.com/basho/rebar), compile and make release:

	rebar get-deps
	rebar compile
	rebar create-node nodeid=fotoncms
	cd rel
	rebar generate

Now you can launch server:

	cd fotoncms
	bin/fotoncms start

Note that you will need installed [MongoDB](http://mongodb.org).

Downloading
-----------

Alternatively you can download binary distribution for x386 [here](https://github.com/Spawnfest2012/Inverted-Link/downloads)
