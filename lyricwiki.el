;;; lyricwiki.el --- an Emacs mode to fetch lyrics.

;; Copyright (C) 2009 Federico Builes

;; Author: Federico Builes <federico.builes@gmail.com>
;; Contributors:
;; Adolfo Builes <builes.adolfo@gmail.com>

;; Created: 1 Dec 2008
;; Version: 1.4
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

;; This is an Emacs mode to fetch lyrics different online sources.
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
;; Finally, if you always want me to automatically fetch the current playing
;; artist/song then just set me up in the defalias down there!
;;
;;; Code:

;; Modify the second symbol to use your preferred player:
;; lyrics-manual: Manually enter the artist and song name.
;; lyrics-amarok: Use the current playing track in Amarok.
;; lyrics-itunes: Use the current playing track in iTunes (OS X).
;; lyrics-rhythmbox: Use the current playing track in Rhythmbox.
(require 'url)

(defalias 'lyrics 'lyrics-manual)

;; Stop modifying stuff here

(defun amarok-song ()
  (interactive)
  (let ((trackMetadata (shell-command-to-string "qdbus org.mpris.amarok /Player GetMetadata")))
    (and (string-match (format "\\<%s: \\([ \\\|:';\?\/>\.<,0-9A-Za-z¢-ÿ]+\\)\\>" "title") trackMetadata))
    (match-string 1 trackMetadata)))

(defun amarok-artist ()
  (interactive)
  (let ((trackMetadata (shell-command-to-string "qdbus org.mpris.amarok /Player GetMetadata")))
    (and (string-match (format "\\<%s: \\([ \\\|:';\?\/>\.<,0-9A-Za-z¢-ÿ]+\\)\\>" "artist") trackMetadata))
    (match-string 1 trackMetadata)))

(defun lyrics-amarok ()
  "Grabs current playing song in Amarok and fetches its lyrics"
  (interactive)
  (let ((artist (amarok-artist ))
        (song (amarok-song)))
    (fetch-lyrics artist song)))

(defun lyrics-rhythmbox ()
  "Grabs current playing song in Rhythmbox and fetches its lyrics"
  (interactive)
  (let ((song (shell-command-to-string "rhythmbox-client --print-playing-format %tt"))
        (artist (shell-command-to-string "rhythmbox-client --print-playing-format %ta")))
    (fetch-lyrics (substring artist 0 -1) (substring song 0 -1))))

;; Only available for OS X
(defun lyrics-itunes ()
  "Grabs current playing song in iTunes and fetches its lyrics"
  (interactive)
  (let* ((artist
          (shell-command-to-string
           "arch -i386 osascript -e'tell application \"iTunes\"' -e'get artist of current track' -e'end tell'"))
         (song
          (shell-command-to-string
           "arch -i386 osascript -e'tell application \"iTunes\"' -e'get name of current track' -e'end tell'" )))
    (fetch-lyrics (substring artist 0 -1) (substring song 0 -1))))


(defun lyrics-manual (artist song)
  "Fetches the lyrics of SONG by ARTIST"
  (interactive "sArtist: \nsSong: ")
  (fetch-lyrics artist song))

(defun fetch-lyrics (artist song)
  "Fetches the lyrics of SONG by ARTIST"
  (kill-new (build-query-string artist song))
  (with-current-buffer (url-retrieve-synchronously (concat "http://www.lyricsplugin.com/plugin/" (build-query-string artist song)))
    (re-search-forward "<div id=\"lyrics\">\\(\\(.*\n\\)*\\)</div>\n\n<div id=\"admin\">" nil t) ; yarly.
    (let ((match (match-string 1)))
      (if (equal match nil)
          (lyric-not-found)
        (kill-new match)
        (kill-buffer (current-buffer))
        (switch-to-buffer (concat artist (concat " - " song)))
        (yank)
        (replace-regexp "<br />.*$" "" nil (point-min) (point-max))
        (goto-char (point-min))))))

(defun capitalize-string (string)
  "Correctly capitalize english strings"
  (concat (capitalize (substring string 0 1)) (substring string 1)))

(defun build-query-string (artist song)
  (let ((artist (replace-regexp-in-string "\s" "%20" (mapconcat 'capitalize-string (split-string artist) " ")))
        (song (replace-regexp-in-string "\s" "%20" (mapconcat 'capitalize-string (split-string song) " "))))
    (format "?title=%s&artist=%s" song artist)))

(defun lyric-not-found ()
  (message "Lyrics not found"))

(provide 'lyricwiki)
;;; lyricwiki.el ends here
