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


