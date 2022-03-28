;;; stimmung-themes.el --- Themes tuned to inner harmonies -*- lexical-binding: t -*-
;; Copyright © 2019

;; Author: Love Lagerkvist
;; URL: https://github.com/motform/stimmung-themes
;; Package-Requires: ((emacs "25"))
;; Created: 2019-12-20
;; Version: 2021-03-20
;; Keywords: faces

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Stimmung (dark and light) is a pair of monochrome Emacs themes
;; with minimal syntax highlighting.  They are inspired by Tonsky's
;; Alabaster theme (https://github.com/tonsky/sublime-scheme-alabaster),
;; following the maxim that a theme that highlights everything
;; paradoxically highlights nothing.  Text backgrounds (comments,
;; strings and constants) and font variations (definitions) are used
;; as alternatives to text colors, ensuring a harmonious reading
;; experience.  Use `stimmung-themes-dark-highlight-color' and
;; `stimmung-themes-light-highlight-color' to customize the highlight.
;;
;; Screenshots are available at: https://github.com/motform/stimmung-themes

;;; Code:

;;; Theme loading/toggle inspired/sourced from the fantastic `protesilaos/modus-themes'

(defgroup stimmung-themes nil
  "Stimmung settings.
You have to re-load the theme for these changes to take effect."
  :group 'faces
  :prefix "stimmung-theme-"
  :link   '(url-link "https://github.com/motform/stimmung-themes"))

(defcustom stimmung-themes-dark-highlight-color "#40382b" ; I dub this shade "Japanese gravy"
  "The primarily color for highlights, the only non-monochrome color in code."
  :type 'string
  :group 'stimmung-themes)

(defcustom stimmung-themes-light-highlight-color "cornsilk1"
  "The primarily color for highlights, the only non-monochrome color in code."
  :type 'string
  :group 'stimmung-themes)

;;;###autoload
(defun stimmung-themes--toggle-prompt ()
  "Helper for `stimmung-themes-toggle'."
  (let ((theme (intern (completing-read "Load Stimmung theme: "
                           '(stimmung-themes-light stimmung-themes-dark) nil t))))
	(pcase theme
	  ('stimmung-themes-light (load-theme 'stimmung-themes-light t))
	  ('stimmung-themes-dark  (load-theme 'stimmung-themes-dark  t)))))

;;;###autoload
(defun stimmung-themes-toggle ()
  "Toggle between the dark and light version of `stimming-themes'.
Prompt the user for which to pick in case none is enabled.
Currently assumes the themes is loaded, which might be an issue.
Inspired by modus-themes."
  (interactive)
  (pcase (car custom-enabled-themes)
	('stimmung-themes-light (load-theme 'stimmung-themes-dark  t))
	('stimmung-themes-dark  (load-theme 'stimmung-themes-light t))
	(_ (stimmung-themes--toggle-prompt))))

(provide 'stimmung-themes)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; stimmung-themes.el ends here
