LyricWiki - An Emacs mode to fetch lyrics
========================

**Update**: The LyricWiki API was [brought
  down](http://groups.google.com/group/lyricwiki-api/browse_thread/thread/733ccd919d654040) by the
  awesome music labels, so we're obtaining the lyrics from somewhere else (but maintaining the same name).

This small mode fetches your lyrics from different online sources and opens a new buffer
with them.

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

Helping, Contact, E-T-C
--------------------------------
* Want to help? If you find any lyrics rendering with weird characters please
  report them in the Issues tracker.


