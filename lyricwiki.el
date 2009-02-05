;;; lyricwiki.el --- an Emacs mode to fetch lyrics from LyricWiki
     
;; Copyright (C) 2009 Federico Builes
     
;; Author: Federico Builes <federico.builes@gmail.com>
;; Created: 1 Dec 2008
;; Version: 0.1
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
;; Now just press M-x lyrics and get your lyrics.

;;; Code:

(let* ((path (file-name-directory
              (or (buffer-file-name) load-file-name))))
  (add-to-list 'load-path "./"))
(require 'http-get)

(defun lyrics (artist song)
  ;; Fetches the lyrics of SONG by ARTIST from LyricWiki.com
  (interactive "sArtist: \nsSong: ")
  (let* ((api "http://lyricwiki.org/api.php?")
        (_artist (concat "artist=" artist))
        (_song (concat "&song=" song))
        (fmt "&fmt=text")
        (url (concat api _artist _song fmt)))
    (http-get url nil nil nil (concat artist " - " song) 'iso-8859-1)))


(provide 'lyricwiki)
;;; lyricwiki.el ends here
