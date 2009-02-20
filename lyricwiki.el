;;; lyricwiki.el --- an Emacs mode to fetch lyrics from LyricWiki
     
;; Copyright (C) 2009 Federico Builes
     
;; Author: Federico Builes <federico.builes@gmail.com>
;; Created: 1 Dec 2008
;; Version: 0.2
;; Keywords: lyrics lyricwiki
     
;; This file is NOT part of GNU Emacs.
       
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
          
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
          
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA
     
;;; Commentary:

;; This is an Emacs mode to fetch lyrics from LyricWiki.com. 
;; Just press M-x lyrics, enter your artist and the song and you 
;; should get your lyrics right away.

;; Installation: 

;; Download lyricwiki.el to some directory:
;; $ git clone git://github.com/febuiles/lyricwiki.el.git
;;
;; Add it to your load list and require it:
;;
;; (add-to-list 'load-path "~/some_directory/lyricwiki.el")
;; (require 'lyricwiki)
;;
;; Now just press: 
;;
;;   M-x lyrics 
;;
;; And get your lyrics.
;; 
;; If you want to automatically fetch the lyrics for the current song in
;; iTunes (OS X), Amarok or Rhythmbox (Linux) you can use:
;;
;;   M-x lyrics-itunes
;;   M-x lyrics-amarok
;;   M-x lyrics-rhythmbox
;;
;; And it will automatically get the current artist/song in iTunes 
;; and fetch the lyrics for you.

;;; Code:

(let* ((path (file-name-directory
              (or (buffer-file-name) load-file-name))))
  (add-to-list 'load-path "./"))
(require 'osx-osascript)
(require 'http-get)

(defun lyrics (artist song)
  "Fetches the lyrics of SONG by ARTIST from LyricWiki.com"
  (interactive "sArtist: \nsSong: ")
  (let* ((api "http://lyricwiki.org/api.php?")
         (_artist (concat "artist=" artist))
         (_song (concat "&song=" song))
         (fmt "&fmt=text")
         (url (concat api _artist _song fmt)))
    (http-get url nil nil nil (capitalize (concat artist " - " song)) 'iso-8859-1)))

(defun lyrics-amarok ()
  "Grabs current playing song in amarok and fetches its lyrics"
  (interactive)
  (let ((song (shell-command-to-string "dcop amarok player title"))
        (artist (shell-command-to-string "dcop amarok player artist")))
    (lyrics artist song)))

(defun lyrics-rhythmbox ()
  "Grabs current playing song in Rhythmbox and fetches its lyrics"
  (interactive)
  (let ((song (shell-command-to-string "rhythmbox-client --print-playing-format %tt"))
	(artist (shell-command-to-string "rhythmbox-client --print-playing-format %ta")))
    (lyrics artist song)))

;; Only available for iTunes in OS X
(defun lyrics-itunes ()
  "Grabs current playing song in iTunes and fetches its lyrics"
  (interactive)
  (let* ((song-info (itunes-get-info-sexp))
         (artist (cadadr song-info))
         (song (cadar song-info)))
    (lyrics artist song)))

;; Borrowed the following functions from osx-itunes.el

(defun itunes-do (&rest pgm)
  "Tell iTunes to run the osascript PGM."
  (osascript-run-str-elispify `("
set retval to false
tell application \"iTunes\"
" ,@pgm "
end tell
elispify(retval)
")))

(defun itunes-get-info-sexp ()
  "Tell itunes to tell us about what it is playing."
  (let* ((osascript-keep-output t)
         (flist '("artist" "name"))
         (script nil))
    (while flist
      (let ((fname (car flist)))
        (setq str (format "{\"%s\", %s of currtrack}%s"
                          fname fname (if script "," ""))))
      (setq script (cons str script))
      (setq flist (cdr flist)))
    (setq itunes-info
          (itunes-do
           "set currtrack to current track\n"
           "set retval to {" script "}"))
    itunes-info))

(provide 'lyricwiki)
;;; lyricwiki.el ends here
