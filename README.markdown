LyricWiki - An Emacs mode to fetch lyrics
========================

This small mode fetches your lyrics from LyricWiki.com and pops a new buffer
with them, nothing too complex.

Installation and Usage
------------
Download lyricwiki.el to some directory:
    $ git clone git://github.com/febuiles/lyricwiki.el.git

Add it to your load list and require it:

    (add-to-list 'load-path "~/some_directory/lyricwiki.el")
    (require 'lyricwiki)

Now just press 

    M-x lyrics 

Adand get your lyrics!

If you're using Mac OS X and iTunes you get some extra goodies. Running:

    M-x lyrics-itunes

will get the current playing song in iTunes and fetch its lyrics.

With amarok
     
    M-x lyrics-amarok

License, Contact, E-T-C
-----------------------

* You can find the license in the lyricwiki.el file.
* You can find me at federico.builes@gmail.com
* Couldn't find the lyrics you were looking for? Go to
 [http://lyricwiki.org/](http://lyricwiki.org/) and contribute them yourself!
