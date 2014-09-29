;;; lyricwiki.el --- an Emacs mode to fetch lyrics.

;; Copyright (C) 2009 Federico Builes

;; Author: Federico Builes <federico.builes@gmail.com>
;;         Francisco Jurado <fjurado@cimat.mx>
;; Contributors:
;; Adolfo Builes <builes.adolfo@gmail.com>

;; Created: 1 Dec 2008
;; Version: 2.0
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

;; Just press M-x lyricwiki/lyrics, enter your artist and the song and you
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
;;   M-x lyricwiki/lyrics
;;
;; And get your lyrics.
;;
;; If you want to automatically fetch the lyrics for the current song in
;; iTunes (OS X), Amarok or Rhythmbox (Linux) you can use:
;;
;;   M-x lyricwiki/lyrics-itunes
;;   M-x lyricwiki/lyrics-amarok
;;   M-x lyricwiki/lyrics-rhythmbox
;;
;; Finally, if you always want to automatically fetch the current playing
;; artist/song then just set up the defalias below:
;;
;;; Code:

(require 'url)

;; Modify the second symbol to use your preferred player:
;; lyricwiki/lyrics-manual: Manually enter the artist and song name.
;; lyricwiki/lyrics-amarok: Use the current playing track in Amarok.
;; lyricwiki/lyrics-itunes: Use the current playing track in iTunes (OS X).
;; lyricwiki/lyrics-rhythmbox: Use the current playing track in Rhythmbox.
(defalias 'lyricwiki/lyrics 'lyricwiki/lyrics-manual)

;; Stop modifying stuff here

(defvar lyricwiki/syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?' "w" st)
    st)
  "Syntax table used for `lyricwiki-mode'.")

(defvar lyricwiki/map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "l" 'lyricwiki/lyrics)
    (define-key map "m" 'lyricwiki/lyrics-manual)
    (define-key map "k" 'lyricwiki/kill-this-buffer)
    map)
  "Keymap for `lyricwiki-mode'.")

(define-derived-mode lyricwiki-mode special-mode "lyricwiki"
  "Major mode for fetching and displaying lyrics \\[lyricwiki/lyrics].
\\{lyricwiki/map}"
  (use-local-map lyricwiki/map)
  (set-syntax-table lyricwiki/syntax-table))

(defun lyricwiki/amarok-song ()
  (interactive)
  (let ((trackMetadata (shell-command-to-string "qdbus org.mpris.amarok /Player GetMetadata")))
    (and (string-match (format "\\<%s: \\([ \\\|:';\?\/>\.<,0-9A-Za-z¢-ÿ]+\\)\\>" "title") trackMetadata))
    (match-string 1 trackMetadata)))

(defun lyricwiki/amarok-artist ()
  (interactive)
  (let ((trackMetadata (shell-command-to-string "qdbus org.mpris.amarok /Player GetMetadata")))
    (and (string-match (format "\\<%s: \\([ \\\|:';\?\/>\.<,0-9A-Za-z¢-ÿ]+\\)\\>" "artist") trackMetadata))
    (match-string 1 trackMetadata)))

(defun lyricwiki/lyrics-amarok ()
  "Grabs current playing song in Amarok and fetches its lyrics"
  (interactive)
  (let ((artist (amarok-artist ))
        (song (lyricwiki/amarok-song)))
    (lyricwiki/fetch-lyrics artist song)))

(defun lyricwiki/lyrics-rhythmbox ()
  "Grabs current playing song in Rhythmbox and fetches its lyrics"
  (interactive)
  (let ((song (shell-command-to-string "rhythmbox-client --print-playing-format %tt"))
        (artist (shell-command-to-string "rhythmbox-client --print-playing-format %ta")))
    (lyricwiki/fetch-lyrics (substring artist 0 -1) (substring song 0 -1))))

;; Only available for OS X
(defun lyricwiki/lyrics-itunes ()
  "Grabs current playing song in iTunes and fetches its lyrics"
  (interactive)
  (let* ((artist
          (shell-command-to-string
           "osascript -e'tell application \"iTunes\"' -e'get artist of current track' -e'end tell'"))
         (song
          (shell-command-to-string
           "osascript -e'tell application \"iTunes\"' -e'get name of current track' -e'end tell'" )))
    (lyricwiki/fetch-lyrics (substring artist 0 -1) (substring song 0 -1))))

(defun lyricwiki/lyrics-manual (artist song)
  "Fetches the lyrics of SONG by ARTIST"
  (interactive "sArtist: \nsSong: ")
  (lyricwiki/fetch-lyrics artist song))

(defconst lyricwiki/not-found-msg
  "Sorry, We don't have lyrics for this song yet")

(defun lyricwiki/fetch-lyrics (artist song)
  "Fetches the lyrics of SONG by ARTIST"
  (with-current-buffer
      (url-retrieve-synchronously
       (concat "http://makeitpersonal.co/lyrics"
               (lyricwiki/build-query-s artist song)))
    (let* ((artist (lyricwiki/trim-s artist))
           (song (lyricwiki/trim-s song))
           (title (lyricwiki/capitalize-s
                   (concat artist " - " song)))
           (buffer-name (format "*Lyrics: %s*" title)))
      (beginning-of-buffer)
      (if (re-search-forward lyricwiki/not-found-msg nil t)
          (progn
            (kill-buffer (current-buffer))
            (message lyricwiki/not-found-msg))
        (lyricwiki/kill-buffer-if-exists buffer-name)
        (lyricwiki/delete-http-header (point))
        (lyricwiki/set-title title)
        (rename-buffer buffer-name)
        (switch-to-buffer buffer-name)
        (lyricwiki-mode)))))

(defun lyricwiki/set-title (title)
  (save-excursion
    (insert (format "\n%s" title))
    (let ((end (point)))
      (forward-line 0)
      (add-text-properties (point) end
                           '(face (bold underline))))))

(defun lyricwiki/capitalize-s (string)
  "Correctly capitalize English STRING."
  (capitalize string))

(defun lyricwiki/kill-buffer-if-exists (buffer-name)
  "Kills `buffer-name' if it exists, does nothing if it doesn't"
  (let ((buffer (get-buffer buffer-name)))
    (if buffer
        (kill-buffer buffer))))

(defun lyricwiki/delete-http-header (p)
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

(defun lyricwiki/kill-this-buffer ()
  "Kill this buffer without prompting to save it."
  (interactive)
  (set-buffer-modified-p nil)
  (kill-buffer))

(defun lyricwiki/build-query-s (artist title)
  (let ((artist (lyricwiki/trim-s artist))
        (title (lyricwiki/trim-s title)))
    (url-encode-url (format "?artist=%s&title=%s" artist title))))

(defun lyricwiki/trim-s (string)
  (replace-regexp-in-string
   "\\`\\s-+" "" (replace-regexp-in-string "\\s-+\\'" "" string)))

(provide 'lyricwiki)
;;; lyricwiki.el ends here
