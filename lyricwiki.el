;;; lyricwiki.el --- an Emacs mode to fetch lyrics.

;; Copyright (C) 2009 Federico Builes

;; Author: Federico Builes <federico.builes@gmail.com>
;; Contributors:
;; Francisco Jurado <fjurado@cimat.mx>
;; Adolfo Builes <builes.adolfo@gmail.com>

;; Created: 1 Dec 2008
;; Version: 1.5
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
;; Finally, if you always want to automatically fetch the current playing
;; artist/song then just set up the defalias below:
;;
;;; Code:

(require 'url)

;; Modify the second symbol to use your preferred player:
;; lyrics-manual: Manually enter the artist and song name.
;; lyrics-amarok: Use the current playing track in Amarok.
;; lyrics-itunes: Use the current playing track in iTunes (OS X).
;; lyrics-rhythmbox: Use the current playing track in Rhythmbox.
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
           "osascript -e'tell application \"iTunes\"' -e'get artist of current track' -e'end tell'"))
         (song
          (shell-command-to-string
           "osascript -e'tell application \"iTunes\"' -e'get name of current track' -e'end tell'" )))
    (fetch-lyrics (substring artist 0 -1) (substring song 0 -1))))

(defun lyrics-manual (artist song)
  "Fetches the lyrics of SONG by ARTIST"
  (interactive "sArtist: \nsSong: ")
  (fetch-lyrics artist song))

(defconst lyrics-not-found-msg
  "Sorry, We don't have lyrics for this song yet")

(defun fetch-lyrics (artist song)
  "Fetches the lyrics of SONG by ARTIST"
  (with-current-buffer
      (url-retrieve-synchronously
       (concat "http://makeitpersonal.co/lyrics"
               (lyrics-build-query-s artist song)))
    (let* ((artist (lyrics-trim-s artist))
           (song (lyrics-trim-s song))
           (title (lyrics-capitalize-s
                   (concat artist " - " song)))
           (buffer-name (format "*Lyrics: %s*" title)))
      (beginning-of-buffer)
      (if (re-search-forward lyrics-not-found-msg nil t)
          (progn
            (kill-buffer (current-buffer))
            (message lyrics-not-found-msg))
        (kill-buffer-if-exists buffer-name)
        (lyrics-delete-http-header (point))
        (insert (format "%s" title))
        (rename-buffer buffer-name)
        (switch-to-buffer buffer-name)
        (special-mode)))))

(defun lyrics-capitalize-s (string)
  "Correctly capitalize English STRING."
  (modify-syntax-entry ?' "w")
  (capitalize string))

(defun kill-buffer-if-exists (buffer-name)
  "Kills `buffer-name' if it exists, does nothing if it doesn't"
  (let ((buffer (get-buffer buffer-name)))
    (if buffer
        (kill-buffer buffer))))

(defun lyrics-delete-http-header (p)
  "Delete paragraph corresponding to the HTTP header
starting on point P."
  (goto-char p)
  (delete-region
   (point) (let ((n 2))
             ; There's a discrepancy between windows and unix
             ; on how the response is presented.
             ; 1 paragraph in windows and 2 paragraphs in unix.
             (if (eq system-type 'windows-nt) (setq n 1))
             (forward-paragraph n)
             (point))))

(defun lyrics-build-query-s (artist title)
  (let ((artist (lyrics-trim-s artist))
        (title (lyrics-trim-s title)))
    (url-encode-url (format "?artist=%s&title=%s" artist title))))

(defun lyrics-trim-s (string)
  (replace-regexp-in-string
   "\\`\\s-+" "" (replace-regexp-in-string "\\s-+\\'" "" string)))

(provide 'lyricwiki)
;;; lyricwiki.el ends here
