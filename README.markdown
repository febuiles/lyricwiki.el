LyricWiki - An Emacs mode to fetch lyrics
========================

This small mode fetches your lyrics from LyricWiki.com and pops a new buffer
with them, nothing too complex.

Installation and Usage
------------
Clone the Git repository:
    $ git clone git://github.com/febuiles/lyricwiki.el.git

Or download the .tar.gz file from:

    http://github.com/febuiles/lyricwiki.el/tarball/master

Add it to your load list and require it:

    (add-to-list 'load-path "~/some_directory/lyricwiki.el")
    (require 'lyricwiki)

(Optional) If you want this mode to automatically fetch your current playing
song in Amarok, iTunes or Rhythmbox then set the defalias in lyricwiki.el to
whatever you want:

    ;; Any of these 3 will do
    (defalias 'lyrics 'lyrics-itunes)
    (defalias 'lyrics 'lyrics-amarok)
    (defalias 'lyrics 'lyrics-rhythmbox)

Now just press:

    M-x lyrics

Finally, if you want to manually enter the song details you can just call:

    M-x lyrics-manual

License, Contact, E-T-C
-----------------------

* You can find the license in the lyricwiki.el file.
* You can find me at federico.builes@gmail.com
* Couldn't find the lyrics you were looking for? Go to
 [http://lyricwiki.org/](http://lyricwiki.org/) and contribute them yourself!
