LyricWiki - An Emacs mode to fetch lyrics
========================
This mode opens a new buffer with the lyrics of the song you're currently
listening to.

Installation and Usage
------------
Clone the Git repository:

    $ git clone git://github.com/febuiles/lyricwiki.el.git

Or download the .tar.gz file from:

    http://github.com/febuiles/lyricwiki.el/tarball/master

Add it to your load list and require it:

    (add-to-list 'load-path "~/some_directory/lyricwiki.el")
    (require 'lyricwiki)

If you want this mode to automatically fetch your current playing
song in iTunes, Rhytmbox or Amarok then set the `defalias` in `lyricwiki.el`:

    ;; Any of these 3 will do
    (defalias 'lyricwiki/lyrics 'lyricwiki/lyrics-itunes)
    (defalias 'lyricwiki/lyrics 'lyricwiki/lyrics-amarok)
    (defalias 'lyricwiki/lyrics 'lyricwiki/lyrics-rhythmbox)

Now just press:

    M-x lyricwiki/lyrics

Finally, if you want to manually enter the song details you can just call:

    M-x lyricwiki/lyrics-manual

Helping, Contact, E-T-C
--------------------------------
Pull requests are always welcome, bugs should be filed in the [Issue Tracker](https://github.com/febuiles/lyricwiki.el/issues).
