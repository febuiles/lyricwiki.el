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

If you want to automatically fetch the lyrics for the current song in
iTunes (OS X), Amarok or Rhythmbox (Linux) you can use:

    M-x lyrics-itunes
    M-x lyrics-amarok
    M-x lyrics-rhythmbox


License, Contact, E-T-C
-----------------------

* You can find the license in the lyricwiki.el file.
* You can find me at federico.builes@gmail.com
* Couldn't find the lyrics you were looking for? Go to
 [http://lyricwiki.org/](http://lyricwiki.org/) and contribute them yourself!
