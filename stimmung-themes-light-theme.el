;;; stimmung-themes-light-theme.el --- A light theme tuned to inner harmonies -*- lexical-binding: t -*-
;; Copyright © 2019

;; Author: Love Lagerkvist
;; URL: https://github.com/motform/stimmung-themes
;; Created: 2019-12-20
;; Version: 2022-03-26
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

(require 'stimmung-themes)

(deftheme stimmung-themes-light
  "A light theme tuned to inner harmonies.")

(let ((bg1 "white")
	  (bg2 "gray95")
	  (bg3 "gray90")
	  (bg4 "gray85")
	  (bg5 "gray95")
	  (bg6 "gray99")
	  (bg7 "gray80")

	  (fg1  "black")
	  (fg2 "gray90")
	  (fg3 "gray80")
	  (fg4 "gray70")
	  (fg5 "gray60")

	  (str    "gray95")
	  (str-fg "gray40")

	  (search   "#e8e800")
	  (search2  "#ffffb4")

	  (warning "orange")
	  (red     "darkred")
	  (ok      "DarkGreen"))
  (cl-flet ((user-controlled-with (var &key bg fg italic? bold?)
			  "Utility function for user-customizable faces.
VAR is the variable that is checked, like `stimmung-themes-comment'.
BG and FG  correspond to the highlight used by 'background and 'foreground,
if something other than the standard is desired.
ITALIC? and BOLD? control font variant."
			  (let ((font-variants `(,@(when italic? `(:italic t))
									 ,@(when bold? `(:bold t)))))
				`((t ,(pcase var
						('background `(:background ,(or bg stimmung-themes-light-highlight-color) :foreground ,fg1 ,@font-variants))
						('foreground `(:background ,bg1 :foreground ,(or fg stimmung-themes-light-highlight-color-foreground) ,@font-variants))
						('none `(:background ,bg1 :foreground ,fg1 ,@font-variants))))))))
	(custom-theme-set-faces
	 'stimmung-themes-light

	 `(default  ((t (:background ,bg1 :foreground ,fg1))))
	 ;; `(shadow   ((t (:background ,bg1))))
	 `(hl-line  ((t (:background ,bg2 :extend t))))

	 `(region              ((t (:background ,bg4))))
	 `(lazy-highlight      ((t (:foreground ,fg1 :background ,search2))))
	 `(secondary-selection ((t (:foreground ,fg1 :background ,search2))))
	 `(highlight           ((t (:foreground ,fg1 :background ,bg3 :bold t))))
	 `(fringe              ((t (:foreground ,fg1 :background ,bg1))))
	 `(match               ((t (:foreground ,ok  :bold t))))
	 `(scroll-bar          ((t (:foreground ,fg5 :background ,bg1))))

	 `(link                ((t (:underline t))))
	 `(link-visited        ((t (:underline t :italic t))))
	 `(button              ((t (:underline t))))
	 `(tooltip             ((t (:foreground ,fg1 :background ,bg3))))
	 `(vertical-border     ((t (:foreground ,bg2 :background ,bg2))))
	 `(info-string         ((t (:background ,stimmung-themes-light-highlight-color))))
	 `(default-italic      ((t (:slant italic))))

	 `(error                       ((t (:foreground ,red))))
	 `(warning                     ((t (:foreground ,warning))))
	 `(success                     ((t (:foreground ,ok))))
	 `(cancel                      ((t (:foreground ,red :strike-through t))))
	 `(font-lock-warning-face      ((t (:foreground ,fg1 :underline (:style wave :color ,warning)))))

	 `(minibuffer-noticable-prompt ((t (:foreground ,fg1 :bold t))))
	 `(minibuffer-prompt           ((t (:foreground ,fg1 :bold t))))

	 `(isearch                     ((t (:foreground ,fg1 :background ,search))))
	 `(isearch-highlight           ((t (:foreground ,fg1 :background ,search2))))
	 `(isearch-fail                ((t (:foreground ,fg1 :background ,search2))))

	 `(paren-matched               ((t (:foreground ,ok  :background ,bg1))))
	 `(paren-unmatched             ((t (:foreground ,red :background ,bg1))))
	 `(escape-glyph                ((t (:foreground ,red :bold t))))
	 `(homoglyph                   ((t (:foreground ,red))))

	 `(line-number              ((t (:foreground ,fg5 :background ,bg1))))
	 `(line-number-current-line ((t (:foreground ,fg1 :background ,bg1))))
	 `(linum                    ((t (:inherit 'line-number))))

	 ;; syntax, user customizable
	 `(font-lock-comment-delimiter-face ,(user-controlled-with stimmung-themes-comment :bg str :fg fg5 :italic? t))
	 `(font-lock-comment-face           ,(user-controlled-with stimmung-themes-comment :bg str :fg fg5 :italic? t))
	 `(font-lock-doc-face               ,(user-controlled-with stimmung-themes-comment :bg str :fg fg5 :italic? t))

	 `(font-lock-negation-char-face     ,(user-controlled-with stimmung-themes-preprocessor))
	 `(font-lock-preprocessor-face      ,(user-controlled-with stimmung-themes-preprocessor))
	 `(font-lock-preprocessor-char-face ,(user-controlled-with stimmung-themes-preprocessor))

	 `(font-lock-regexp-grouping-backslash ,(user-controlled-with stimmung-themes-regex :bold? t))
	 `(font-lock-regexp-grouping-construct ,(user-controlled-with stimmung-themes-regex :bold? t))

	 `(font-lock-builtin-face           ,(user-controlled-with stimmung-themes-builtin :italic? t))
	 `(font-lock-constant-face          ,(user-controlled-with stimmung-themes-constant :italic? t))
	 `(font-lock-function-name-face     ,(user-controlled-with stimmung-themes-function-name :bold? t))
	 `(font-lock-keyword-face           ,(user-controlled-with stimmung-themes-keyword))
	 `(font-lock-type-face              ,(user-controlled-with stimmung-themes-type))
	 `(font-lock-variable-name-face     ,(user-controlled-with stimmung-themes-variable-name :bold? t))
	 `(font-lock-string-face            ,(user-controlled-with stimmung-themes-string :bg str :fg str-fg))
	 `(font-lock-doc-markup-face        ,(user-controlled-with stimmung-themes-markup))

	 ;; custom
	 `(custom-invalid		((t (:background ,bg1 :foreground ,fg1 :underline (:style wave :color ,warning)))))
	 `(custom-rogue		    ((t (:background ,bg1 :foreground ,fg5 :underline nil))))
	 `(custom-modified		((t (:background ,stimmung-themes-light-highlight-color :foreground ,fg1))))
	 `(custom-set			((t (:background ,bg1 :foreground ,fg1 :bold t))))
	 `(custom-changed		((t (:background ,bg1 :foreground ,fg1 :italic t))))
	 `(custom-themed		((t (:background ,str :foreground ,fg1))))
	 `(custom-saved			((t (:background ,bg1 :foreground ,fg1 :bold t))))
	 `(custom-state			((t (:background ,bg1 :foreground ,fg1 :italic t))))
	 `(custom-link			((t (:background ,str :foreground ,fg1 :underline nil))))
	 `(custom-visibility	((t (:background ,bg1 :foreground ,fg5 :height 0.8))))
	 `(custom-comment		((t (:background ,bg1 :foreground ,fg1 :italic t))))
	 `(custom-comment-tag	((t (:background ,bg1 :foreground ,fg1 :italic t))))

	 `(custom-group-tag-1		((t (:background ,bg1 :foreground ,fg1 :bold t :height 1.1 :inherit variable-pitch))))
	 `(custom-group-tag			((t (:background ,bg1 :foreground ,fg1 :bold t :height 1.1 :inherit variable-pitch))))
	 `(custom-group-subtitle	((t (:background ,bg1 :foreground ,fg1 :bold t))))

	 `(custom-button			((t (:background ,bg5 :foreground ,fg1 :box (:line-width 1 :color ,fg5)))))
	 `(custom-button-mouse		((t (:background ,bg3 :foreground ,fg1 :box (:line-width 1 :color ,bg4)))))
	 `(custom-button-unraised	((t (:background ,bg5 :foreground ,fg1 :box (:line-width 1 :color ,fg5)))))

	 `(custom-variable-obsolete	((t (:background ,bg1 :foreground ,fg5))))
	 `(custom-variable-tag		((t (:background ,bg1 :foreground ,fg5))))
	 `(custom-variable-button	((t (:background ,bg5 :foreground ,fg1 :box (:line-width 1 :color ,fg5)))))

	 ;; widget
	 `(widget-inactive			((t (:background ,bg1 :foreground ,fg5))))
	 `(widget-documentation		((t (:background ,bg1 :foreground ,fg1))))
	 `(widget-button			((t (:background ,bg5 :foreground ,fg1 :box (:line-width 1 :color ,fg5)))))
	 `(widget-field				((t (:background ,bg2 :foreground ,fg1 :box (:line-width 1 :color ,bg4)))))
	 `(widget-single-line-field	((t (:background ,bg2 :foreground ,fg1 :box (:line-width 1 :color ,bg4)))))

	 ;; message
	 `(message-cited-text-1      ((t (:foreground ,fg5))))
	 `(message-cited-text-2      ((t (:foreground ,fg4))))
	 `(message-cited-text-3      ((t (:foreground ,fg3))))
	 `(message-cited-text-4      ((t (:inherit message-cited-text-3))))
	 `(message-header-cc         ((t (:foreground ,fg1))))
	 `(message-header-name       ((t (:foreground ,fg1))))
	 `(message-header-newsgroups ((t (:inherit message-header-other))))
	 `(message-header-other      ((t (:foreground ,fg5))))
	 `(message-header-subject    ((t (:foreground ,fg1 :inherit bold))))
	 `(message-header-to         ((t (:foreground ,fg1))))
	 `(message-header-xheader    ((t (:background ,bg2))))
	 `(message-mml               ((t (:foreground ,fg5 :background ,bg2))))
	 `(message-separator         ((t (:background ,bg3))))

	 ;; eshell
	 `(eshell-ls-directory  ((t (:background ,stimmung-themes-light-highlight-color :bold t))))
	 `(eshell-prompt        ((t (:foreground ,fg1 :bold t))))
	 `(eshell-ls-executable ((t (:foreground ,fg1 :bold t))))
	 `(eshell-ls-symlink    ((t (:foreground ,fg1 :italic t))))
	 `(eshell-ls-special    ((t (:foreground ,ok  :italic t))))
	 `(eshell-ls-backup     ((t (:foreground ,fg1 :italic t))))
	 `(eshell-ls-readonly   ((t (:foreground ,red))))
	 `(eshell-ls-unreadable ((t (:foreground ,red))))
	 `(eshell-ls-missing    ((t (:foreground ,red))))
	 `(eshell-ls-product    ((t (:foreground ,fg1))))
	 `(eshell-ls-archive    ((t (:foreground ,fg1))))
	 `(eshell-ls-entries    ((t (:foreground ,fg1))))

	 ;; avy
	 `(avy-lead-face   ((t (:background ,bg2 :foreground ,fg1 :distant-foreground ,fg1 :bold t))))
	 `(avy-lead-face-0 ((t (:inherit 'avy-lead-face))))
	 `(avy-lead-face-1 ((t (:inherit 'avy-lead-face))))
	 `(avy-lead-face-2 ((t (:inherit 'avy-lead-face))))

	 ;; flyspell
	 `(flycheck-note            ((t (:underline (:style wave :color ,ok)))))
	 `(flyspell-incorrect       ((t (:underline (:style wave :color ,red) ))))
	 `(flycheck-error           ((t (:underline (:style wave :color ,red)))))
	 `(flyspell-duplicate       ((t (:underline (:style wave :color ,warning)))))
	 `(flysheck-warning         ((t (:underline (:style wave :color ,warning)))))
	 `(flysheck-warning-overlay ((t (:underline (:style wave :color ,warning)))))

	 ;; hydra
	 `(hydra-face-red      ((t (:foreground ,fg1 :bold t))))
	 `(hydra-face-blue     ((t (:foreground ,fg1 :bold t))))
	 `(hydra-face-amaranth ((t (:foreground ,fg1 :bold t))))
	 `(hydra-face-pink     ((t (:foreground ,fg1 :bold t))))
	 `(hydra-face-teal     ((t (:foreground ,fg1 :bold t))))

	 ;; cider
	 `(cider-fringe-good-face		((t (:foreground ,ok))))
	 ;; `(cider-test-success-face   ((t (:background ,ok  :foreground ,bg1))))
	 ;; `(cider-test-failure-face   ((t (:background ,red :foreground ,bg1))))
	 `(cider-test-error-face		((t (:background ,stimmung-themes-light-highlight-color))))
	 `(cider-result-overlay-face	((t (:background ,bg5 :box (:line-width 1 :color ,fg5)))))

	 ;; company
	 `(company-scrollbar-bg						((t (:background ,bg2))))
	 `(company-scrollbar-fg						((t (:background ,fg5))))
	 `(company-tooltip-scrollbar-thumb			((t (:background ,fg5))))
	 `(company-tooltip-scrollbar-track			((t (:background ,bg2))))
	 `(company-tooltip-common					((t (:foreground ,fg1))))
	 `(company-tooltip							((t (:background ,bg2 :foreground ,fg1 :inherit fixed-pitch))))
	 `(company-tooltip-selection				((t (:background ,bg3 :foreground ,fg1))))
	 `(company-tooltip-common					((t (:foreground ,fg1 :background ,search2))))
	 `(company-tooltip-common-selection			((t (:foreground ,fg1 :background ,search2))))
	 `(company-tooltip-search					((t (:foreground ,fg1 :background ,search2 :inherit fixed-pitch))))
	 `(company-tooltip-search-selection			((t (:foreground ,fg1 :background ,search2))))
	 `(company-tooltip-annotation				((t (:foreground ,fg5 :inherit fixed-pitch))))
	 `(company-tooltip-quick-access				((t (:foreground ,fg4))))
	 `(company-tooltip-quick-access-selection	((t (:foreground ,fg5))))
	 `(company-tooltip-mouse					((t (:inherit highlight))))
	 `(company-preview							((t (:background ,stimmung-themes-light-highlight-color :foreground ,fg1))))
	 `(company-echo-common						((t (:background ,fg1 :foreground ,bg1))))

	 ;; compilation
	 `(compilation-line-number    ((t (:bold t))))
	 `(compilation-column-number  ((t (:inherit 'font-lock-comment-face))))
	 `(compilation-error          ((t (:inherit 'error :bold t))))
	 `(compilation-warning        ((t (:inherit 'warning))))
	 `(compilation-info           ((t (:inherit 'success))))
	 `(compilation-mode-line-exit ((t (:inherit 'compilation-info))))
	 `(compilation-mode-line-fail ((t (:inherit 'compilation-error))))

	 ;; corfu
	 `(corfu-current ((t (:background ,bg4 :foreground ,fg1))))
	 `(corfu-echo    ((t (:background ,bg1 :foreground ,fg1 :italic t))))

	 ;; consult
	 `(consult-project-extra-projects ((t (:foreground ,str-fg :italic t))))
	 `(consult-file               ((t (:foreground ,fg1))))
	 `(consult-grep-context       ((t (:foreground ,fg1))))
	 `(consult-grep-help          ((t (:foreground ,ok))))
	 `(consult-highlight-match    ((t (:background ,search))))
	 `(consult-preview-insertion  ((t (:foreground ,fg1))))
	 `(consult-preview-line       ((t (:foreground ,fg1))))
	 `(consult-separator          ((t (:foreground ,fg5))))

	 ;; modeline
	 `(header-line         ((t (:inherit 'mode-line :distant-foreground ,bg1))))
	 `(mode-line           ((t (:foreground ,fg1 :background ,bg5 :box (:line-width 1 :color ,fg5 :style nil)))))
	 `(mode-line-inactive  ((t (:foreground ,fg5 :background ,bg1 :box (:line-width 1 :color ,bg3 :style nil)))))
	 `(mode-line-buffer-id ((t (:foreground ,fg1 :bold t :distant-foreground ,bg1))))
	 `(mode-line-emphasis  ((t (:foreground ,fg1 :bold t))))
	 `(mode-line-highlight ((t (:foreground ,bg3))))

	 ;; centaur tabs
	 `(centaur-tabs-default             ((t (:foreground ,fg1 :background ,bg5))))
	 `(centaur-tabs-unselected          ((t (:foreground ,fg5 :background ,bg2))))
	 `(centaur-tabs-selected            ((t (:foreground ,fg1 :background ,bg1))))
	 `(centaur-tabs-unselected-modified ((t (:foreground ,fg5 :background ,bg2))))
	 `(centaur-tabs-selected-modified   ((t (:foreground ,fg1 :background ,bg1))))
	 `(centaur-tabs-close-unselected    ((t (:foreground ,fg3 :background ,bg2))))
	 `(centaur-tabs-close-selected      ((t (:foreground ,fg3 :background ,bg1))))
	 `(centaur-tabs-name-mouse-face     ((t (:foreground ,fg1))))
	 `(centaur-tabs-close-mouse-face    ((t (:foreground ,fg1))))
	 `(centaur-tabs-active-bar-face     ((t (:background ,stimmung-themes-light-highlight-color))))

	 ;; completions
	 `(completions-common-part ((t (:foreground ,fg1 :bold t))))

	 ;; doom-modeline
	 `(doom-modeline-buffer-path        ((t (:foreground ,fg1))))
	 `(doom-modeline-buffer-file        ((t (:foreground ,fg1 :weight bold))))
	 `(doom-modeline-buffer-modified    ((t (:foreground ,red :weight bold))))
	 `(doom-modeline-project-dir        ((t (:foreground ,fg1 :weight bold))))
	 `(doom-modeline-project-root-dir   ((t (:foreground ,fg1 :weight normal))))
	 `(doom-modeline-project-parent-dir ((t (:foreground ,fg1 :weight normal))))
	 `(doom-modeline-bar-inactive       ((t (:foreground ,fg1 :background ,bg1))))
	 `(doom-modeline-bar                ((t (:background ,stimmung-themes-light-highlight-color)))) ; the leftmost bar
	 `(doom-modeline-evil-insert-state  ((t (:foreground ,fg1))))
	 `(doom-modeline-evil-visual-state  ((t (:foreground ,fg1))))
	 `(doom-modeline-evil-normal-state  ((t (:foreground ,fg1))))
	 `(doom-modeline-evil-emacs-state   ((t (:foreground ,red :italic nil))))
	 `(doom-modeline-buffer-minor-mode  ((t (:background ,bg5))))

	 ;; dired
	 `(dired-directory  ((t (:foreground ,fg1 :bold t))))
	 `(dired-ignored    ((t (:foreground ,fg1))))
	 `(dired-flagged    ((t (:foreground ,ok))))
	 `(dired-header     ((t (:foreground ,fg1 :bold t))))
	 `(dired-mark       ((t (:foreground ,red :bold t))))
	 `(dired-marked     ((t (:foreground ,red :bold t))))
	 `(dired-perm-write ((t (:foreground ,fg1 :underline t))))
	 `(dired-symlink    ((t (:foreground ,fg1 :italic t))))
	 `(dired-warning    ((t (:foreground ,fg1 :underline (:style wave :color ,warning)))))

	 ;; diredfl
	 `(diredfl-compressed-file-name   ((t (:foreground ,fg1))))
	 `(diredfl-compressed-file-suffix ((t (:foreground ,fg4))))
	 `(diredfl-date-time              ((t (:foreground ,fg4 :inherit bold))))
	 `(diredfl-deletion               ((t (:inherit error))))
	 `(diredfl-deletion-file-name     ((t (:inherit error :strike-through ,red))))
	 `(diredfl-dir-heading            ((t (:foreground ,fg1 :background ,bg2))))
	 `(diredfl-dir-name               ((t (:foreground ,fg5 :background ,bg5 :inherit bold))))
	 `(diredfl-executable-tag         ((t (:foreground ,fg1))))
	 `(diredfl-file-name              ((t (:foreground ,fg1))))
	 `(diredfl-file-suffix            ((t (:foreground ,fg1 :background ,stimmung-themes-light-highlight-color))))
	 `(diredfl-flag-mark              ((t (:background ,stimmung-themes-light-highlight-color))))
	 `(diredfl-flag-mark-line         ((t (:background ,stimmung-themes-light-highlight-color))))
	 `(diredfl-ignored-file-name      ((t (:foreground ,fg4))))
	 `(diredfl-number                 ((t (:foreground ,fg4 :inherit bold))))
	 `(diredfl-dir-priv               ((t (:foreground ,fg1 :inherit bold))))
	 `(diredfl-exec-priv              ((t (:foreground ,fg1))))
	 `(diredfl-link-priv              ((t (:foreground ,fg1))))
	 `(diredfl-no-priv                ((t (:foreground ,fg1))))
	 `(diredfl-other-priv             ((t (:foreground ,fg1))))
	 `(diredfl-rare-priv              ((t (:foreground ,fg1))))
	 `(diredfl-read-priv              ((t (:foreground ,fg1))))
	 `(diredfl-write-priv             ((t (:foreground ,fg1))))
	 `(diredfl-symlink                ((t (:foreground ,fg5))))
	 `(diredfl-autofile-name          ((t (:foreground ,fg1))))
	 `(diredfl-tagged-autofile-name   ((t (:foreground ,fg1))))

	 ;; evil
	 `(evil-ex-info                   ((t (:foreground ,red :italic t))))
	 `(evil-ex-search                 ((t (:background ,bg2 :foreground ,fg1 :bold t))))
	 `(evil-search-highlight-persist-highlight-face ((t (:inherit 'lazy-highlight))))

	 ;; evil-mc
	 `(evil-mc-cursor-default-face ((t (:foreground ,bg1 :background ,fg1))))
	 `(evil-mc-region-face         ((t (:foreground ,bg1 :background ,fg1))))
	 `(evil-mc-cursor-bar-face     ((t (:foreground ,fg1))))
	 `(evil-mc-cursor-hbar-face    ((t (:foreground ,fg1))))

	 ;; info
	 `(info-quoted    ((t (:inherit 'default :bold t))))
	 `(info-menu-star ((t (:bold t))))

	 ;; Mini-frame

	 `(mini-frame-mode ((t (:bold t))))

	 ;; ivy
	 `(ivy-current-match              ((t (:background ,bg1 :bold t))))
	 `(ivy-minibuffer-match-highlight ((t (:foreground ,ok))))
	 `(ivy-minibuffer-match-face-1    ((t (:foreground ,fg1 :bold t))))
	 `(ivy-minibuffer-match-face-2    ((t (:foreground ,fg1 :bold t))))
	 `(ivy-minibuffer-match-face-3    ((t (:foreground ,fg1 :bold t))))
	 `(ivy-minibuffer-match-face-4    ((t (:foreground ,fg1 :bold t))))
	 `(ivy-confirm-face               ((t (:foreground ,ok))))
	 `(ivy-required-face              ((t (:foreground ,red))))
	 `(ivy-subdir                     ((t (:foreground ,fg1))))
	 `(ivy-modified-buffer            ((t (:foreground ,red :bold t))))
	 `(ivy-modified-outside-buffer    ((t (:foreground ,red))))
	 `(ivy-remote                     ((t (:foreground ,fg1))))
	 `(ivy-virtual                    ((t (:foreground ,fg1 :italic t))))
	 `(ivy-prompt                     ((t (:foreground ,red))))
	 `(ivy-prompt-match               ((t (:foreground ,red))))
	 `(ivy-separator                  ((t (:foreground ,stimmung-themes-light-highlight-color))))
	 `(ivy-highlight-face             ((t (:foreground ,red))))
	 `(ivy-grep-info                  ((t (:foreground ,red))))
	 `(ivy-completions-annotations    ((t (:foreground ,red))))

	 ;; helm
	 `(helm-header						((t (:foreground ,fg5 :background ,bg1 :underline nil :box nil))))
	 `(helm-source-header				((t (:foreground ,fg1 :background ,bg1 :underline nil :weight bold :box (:line-width 1 :style released-button)))))
	 `(helm-selection					((t (:background ,fg1 :underline t :foreground ,bg1))))
	 `(helm-selection-line				((t (:background ,fg1))))
	 `(helm-visible-mark				((t (:foreground ,fg5 :background ,bg4))))
	 `(helm-candidate-number			((t (:foreground ,fg5 :background ,bg1))))
	 `(helm-separator					((t (:foreground ,fg1 :background ,bg1))))
	 `(helm-time-zone-current			((t (:foreground ,fg1 :background ,bg1))))
	 `(helm-time-zone-home				((t (:foreground ,fg1 :background ,bg1))))
	 `(helm-bookmark-addressbook		((t (:foreground ,fg5 :background ,bg1))))
	 `(helm-bookmark-directory			((t (:foreground nil  :background nil :inherit helm-ff-directory))))
	 `(helm-bookmark-file				((t (:foreground nil  :background nil :inherit helm-ff-file))))
	 `(helm-bookmark-gnus				((t (:foreground ,fg1 :background ,bg1))))
	 `(helm-bookmark-info				((t (:foreground ,fg5 :background ,bg1))))
	 `(helm-bookmark-man				((t (:foreground ,fg1 :background ,bg1))))
	 `(helm-bookmark-w3m				((t (:foreground ,fg1 :background ,bg1))))
	 `(helm-buffer-not-saved			((t (:foreground ,fg1 :background ,bg1))))
	 `(helm-buffer-process				((t (:foreground ,fg1 :background ,bg1))))
	 `(helm-buffer-saved-out			((t (:foreground ,fg5 :background ,bg1))))
	 `(helm-buffer-size					((t (:foreground ,fg5 :background ,bg1))))
	 `(helm-ff-directory				((t (:foreground ,fg1 :background ,bg1 :weight bold))))
	 `(helm-ff-file						((t (:foreground ,fg5 :background ,bg1 :weight normal))))
	 `(helm-ff-file-extension			((t (:foreground ,fg5 :weight bold))))
	 `(helm-ff-executable				((t (:foreground ,fg5 :background ,bg1 :weight normal))))
	 `(helm-ff-invalid-symlink			((t (:foreground ,fg1 :background ,bg1 :weight bold))))
	 `(helm-ff-symlink					((t (:foreground ,fg1 :background ,bg1 :weight bold))))
	 `(helm-ff-prefix					((t (:foreground ,bg1 :background ,fg1 :weight normal))))
	 `(helm-grep-cmd-line				((t (:foreground ,fg1 :background ,bg1))))
	 `(helm-grep-file					((t (:foreground ,fg5 :background ,bg1))))
	 `(helm-grep-finish					((t (:foreground ,fg5 :background ,bg1))))
	 `(helm-grep-lineno					((t (:foreground ,fg5 :background ,bg1))))
	 `(helm-grep-match					((t (:foreground nil  :background nil :inherit helm-match))))
	 `(helm-grep-running				((t (:foreground ,fg1 :background ,bg1))))
	 `(helm-lisp-show-completion		((t (:background ,bg2))))
	 `(helm-match						((t (:background ,search))))
	 `(helm-moccur-buffer				((t (:foreground ,fg1 :background ,bg1))))
	 `(helm-mu-contacts-address-face	((t (:foreground ,fg5 :background ,bg1))))
	 `(helm-mu-contacts-name-face		((t (:foreground ,fg5 :background ,bg1))))

	 ;; orderless
	 `(orderless-match-face-0 ((t (:foreground ,fg1 :bold t))))
	 `(orderless-match-face-1 ((t (:foreground ,fg1 :bold t))))
	 `(orderless-match-face-2 ((t (:foreground ,fg1 :bold t))))
	 `(orderless-match-face-3 ((t (:foreground ,fg1 :bold t))))

	 ;; magit
	 `(magit-bisect-bad        ((t (:foreground ,red))))
	 `(magit-bisect-good       ((t (:foreground ,ok))))
	 `(magit-bisect-skip       ((t (:foreground ,fg1))))
	 `(magit-blame-date        ((t (:foreground ,red))))
	 `(magit-branch            ((t (:foreground ,fg1 :bold t))))
	 `(magit-branch-local      ((t (:foreground ,fg1 :bold t))))
	 `(magit-branch-remote     ((t (:foreground ,fg1 :bold t))))
	 `(magit-diff-file-heading ((t (:foreground ,fg1 :bold nil))))
	 `(magit-diff-whitespace-warning ((t (:background ,red))))

	 `(magit-branch-remote-head	    ((t (:foreground ,fg5 :background ,bg5 :box (:line-width 1 :color ,fg5)))))
	 `(magit-branch-current         ((t (:foreground ,fg1 :background ,bg5 :box (:line-width 1 :color ,fg5)))))
	 `(magit-diff-context-highlight ((t (:foreground ,fg1 :background ,bg3))))
	 `(magit-diff-file-header       ((t (:foreground ,fg1 :background ,bg3))))
	 `(magit-diffstat-added         ((t (:foreground ,ok))))
	 `(magit-diffstat-removed       ((t (:foreground ,red))))
	 `(magit-dimmed                 ((t (:foreground ,fg1))))
	 `(magit-hash                   ((t (:foreground ,fg1 :background ,str))))
	 `(magit-hunk-heading           ((t (:background ,bg3))))
	 `(magit-hunk-heading-highlight ((t (:background ,bg3))))
	 `(magit-item-highlight         ((t (:background ,bg3))))
	 `(magit-log-author             ((t (:foreground ,fg1))))
	 `(magit-process-ng             ((t (:background ,stimmung-themes-light-highlight-color :bold t))))
	 `(magit-process-ok             ((t (:foreground ,ok :bold t))))
	 `(magit-section-heading        ((t (:foreground ,fg1 :bold t))))
	 `(magit-section-highlight      ((t (:background ,bg3))))
	 `(magit-tag                    ((t ,(user-controlled-with stimmung-themes-constant :italic? t))))

	 `(magit-blame-highlight        ((t (:foreground ,fg1 :background ,bg5 :box (:line-width 1 :color ,fg5 :style nil)))))
	 `(magit-blame-name             ((t (:bold t))))
	 `(magit-blame-date             ((t (:foreground ,fg1 :italic t))))
	 `(magit-blame-summary          ((t (:foreground ,fg1))))
	 `(magit-blame-hash             ((t (:foreground ,fg1))))
	 
	 `(git-commit-comment-action    ((t ((:inherit 'font-lock-comment-face) :bold t))))

	 ;; diff-hl
	 `(diff-hl-insert         ((t (:foreground ,fg1 :background ,ok     :bold nil :italic nil))))
	 `(diff-hl-delete         ((t (:foreground ,fg1 :background ,red    :bold nil :italic nil))))
	 `(diff-hl-change         ((t (:foreground ,fg1 :background ,search :bold nil :italic nil))))
	 `(diff-hl-ignore         ((t (:foreground ,fg1 :background ,bg1    :bold nil :italic nil))))
	 `(diff-hl-margin-ignore  ((t (:foreground ,fg1 :background ,bg1    :bold nil :italic nil))))
	 `(diff-hl-margin-unknown ((t (:foreground ,fg1 :background ,bg1    :bold nil :italic nil))))

	 ;; git-gutter-fringe
	 `(git-gutter-fr:added    ((t (:foreground ,ok :bold nil :italic nil))))
	 `(git-gutter-fr:deleted  ((t (:foreground ,red :bold nil :italic nil))))
	 `(git-gutter-fr:modified ((t (:foreground ,search :bold nil :italic nil))))
	 `(git-gutter:added       ((t (:foreground ,ok :bold nil :italic nil))))
	 `(git-gutter:deleted     ((t (:foreground ,red :bold nil :italic nil))))
	 `(git-gutter:modified    ((t (:foreground ,search :bold nil :italic nil))))

	 ;; help
	 `(help-key-binding ((t (:foreground ,fg1 :background ,bg5 :box (:line-width 1 :color ,fg5)))))

	 ;; outline, extends org-outline
	 `(outline-1 ((t (:foreground ,fg1 :bold t :extend t))))
	 `(outline-2 ((t (:foreground ,fg1 :bold t :extend t))))
	 `(outline-3 ((t (:foreground ,fg1 :bold t :extend t))))
	 `(outline-4 ((t (:foreground ,fg1 :bold t :extend t))))
	 `(outline-5 ((t (:foreground ,fg1 :bold t :extend t))))
	 `(outline-6 ((t (:foreground ,fg1 :bold t :extend t))))
	 `(outline-7 ((t (:foreground ,fg1 :bold t :extend t))))
	 `(outline-8 ((t (:foreground ,fg1 :bold t :extend t))))

	 ;; TODO org-agenda

	 ;; org
	 `(org-code                  ((t (:background ,str :distant-foreground ,bg1 :background ,stimmung-themes-light-highlight-color))))
	 `(org-link                  ((t (:underline t))))
	 `(org-block                 ((t (:foreground ,fg1 :background ,bg5 :extend t :inherit fixed-pitch))))
	 `(org-block-begin-line      ((t (:foreground ,fg5 :background ,bg5 :extend t :inherit fixed-pitch))))
	 `(org-block-end-line        ((t (:foreground ,fg5 :background ,bg5 :extend t :inherit fixed-pitch))))
	 `(org-drawer                ((t (:foreground ,fg1 :bold t :inherit fixed-pitch))))
	 `(org-document-info         ((t (:foreground ,fg1 :background ,bg1 :italic t))))
	 `(org-document-info-keyword ((t (:foreground ,fg1 :background ,bg1))))
	 `(org-document-title        ((t (:foreground ,fg1 :weight bold))))
	 `(org-done                  ((t (:foreground ,ok :bold t :strike-through t))))
	 `(org-ellipsis              ((t (:foreground ,fg1))))
	 `(org-meta-line             ((t (:background ,bg1))))
	 `(org-formula               ((t (:foreground ,fg1))))
	 `(org-headline-done         ((t (:foreground ,fg1 :weight normal :strike-through t))))
	 `(org-hide                  ((t (:foreground ,bg1 :background ,bg1))))
	 `(org-list-dt               ((t (:foreground ,fg1 :bold t))))
	 `(org-scheduled             ((t (:foreground ,red))))
	 `(org-scheduled-today       ((t (:foreground ,ok))))
	 `(org-table                 ((t (:foreground ,fg1 :inherit fixed-pitch))))
	 `(org-tag                   ((t (:foreground ,fg1 :background ,bg1 :bold t))))
	 `(org-todo                  ((t (:foreground ,red :bold t))))
	 `(org-warning               ((t (:inherit 'warning))))
	 `(org-upcoming-deadline     ((t (:foreground ,red))))
	 `(org-priority              ((t (:background ,stimmung-themes-light-highlight-color))))
	 `(org-footnote              ((t (:background ,stimmung-themes-light-highlight-color))))
	 `(org-scheduled-previously  ((t (:background ,stimmung-themes-light-highlight-color))))
	 `(org-sexp-date             ((t (:background ,stimmung-themes-light-highlight-color))))
	 `(org-special-keyword       ((t (:background ,stimmung-themes-light-highlight-color))))
	 `(org-date                  ((t (:background ,bg1 :bold t))))
	 `(org-checkbox              ((t (:foreground ,fg1 :bold t :inherit fixed-pitch))))

	 ;; markdown mode
	 `(markdown-header-face             ((t (:foreground ,fg1 :bold t))))
	 `(markdown-list-face               ((t (:foreground ,fg1 :bold t))))
	 `(markdown-bold-face               ((t (:foreground ,fg1 :bold t))))
	 `(markdown-blockquote-face         ((t (:foreground ,fg1 :italic t))))
	 `(markdown-italic-face             ((t (:foreground ,fg1 :italic t))))
	 `(markdown-link-face               ((t (:foreground ,fg1 :underline t))))
	 `(markdown-url-face                ((t (:foreground ,fg1 :underline t))))
	 `(markdown-header-delimiter-face   ((t (:inherit 'markdown-header-face))))
	 `(markdown-metadata-key-face       ((t (:foreground ,fg1))))
	 `(markdown-markup-face             ((t (:foreground ,fg1 :inherit fixed-pitch))))
	 `(markdown-pre-face                ((t (:inherit org-block))))
	 `(markdown-inline-code-face        ((t (:foreground ,fg1 :background ,stimmung-themes-light-highlight-color :inherit fixed-pitch))))
	 `(markdown-code-face               ((t (:foreground ,fg1 :background ,bg5 :extend t))))
	 `(markdown-reference-face          ((t (:foreground ,fg1))))
	 `(markdown-html-attr-name-face     ((t (:inherit 'font-lock-variable-name-face :inherit fixed-pitch))))
	 `(markdown-html-attr-value-face    ((t (:inherit 'font-lock-string-face :inherit fixed-pitch))))
	 `(markdown-html-entity-face        ((t (:inherit 'font-lock-variable-name-face))))
	 `(markdown-html-tag-delimiter-face ((t (:inherit 'markdown-markup-face :inherit fixed-pitch))))
	 `(markdown-html-tag-name-face      ((t (:inherit 'font-lock-keyword-face :inherit fixed-pitch))))
	 `(markdown-language-keyword-face   ((t (:background ,stimmung-themes-light-highlight-color :inherit fixed-pitch))))
	 `(markdown-table-face              ((t (:inherit fixed-pitch))))

	 ;; solaire
	 `(solaire-default-face				((t (:background ,bg5 :foreground ,fg1))))
	 `(solaire-fringe-face				((t (:background ,bg5 :foreground ,fg1))))
	 `(solaire-line-number-face			((t (:background ,bg5 :foreground ,fg5))))
	 `(solaire-hl-line-face				((t (:background ,bg2 :extend t))))
	 `(solaire-org-hide-face			((t (:background ,bg2 :foreground ,fg5))))
	 `(solaire-mode-line-face			((t (:foreground ,fg1 :background ,bg6 :box (:line-width 1 :color ,fg5 :style nil)))))
	 `(solaire-mode-line-inactive-face	((t (:foreground ,fg5 :background ,bg2 :box (:line-width 1 :color ,bg3 :style nil)))))
	 `(solaire-header-line-face			((t (:background ,bg5 :foreground ,fg1))))

	 ;; sh
	 `(sh-quoted-exec ((t (:background ,stimmung-themes-light-highlight-color))))
	 `(sh-heredoc     ((t (:background ,stimmung-themes-light-highlight-color))))

	 ;; Shortdoc
	 `(shortdoc-heading  ((t (:background ,bg1 :foreground ,fg1 :bold t))))
	 `(shortdoc-section  ((t (:background ,bg1 :foreground ,fg1))))

	 ;; show-paren
	 `(show-paren-match-face       ((t (:background ,search :bold t))))
	 `(show-paren-match            ((t (:background ,search :bold t))))
	 `(show-paren-match-expression ((t (:background ,search :bold t))))
	 `(show-paren-mismatch         ((t (:background ,fg1 :foreground ,red :bold t))))

	 ;; smartparens
	 `(sp-show-pair-match-face    ((t (:inherit 'paren-matched))))
	 `(sp-show-pair-mismatch-face ((t (:inherit 'paren-unmatched))))

	 ;; rainbow-delimiters
	 `(rainbow-delimiters-depth-1-face  ((t (:foreground ,fg1))))
	 `(rainbow-delimiters-depth-2-face  ((t (:foreground ,fg4))))
	 `(rainbow-delimiters-depth-3-face  ((t (:foreground ,fg1))))
	 `(rainbow-delimiters-depth-4-face  ((t (:foreground ,fg5))))
	 `(rainbow-delimiters-depth-5-face  ((t (:foreground ,fg1))))
	 `(rainbow-delimiters-depth-6-face  ((t (:foreground ,fg4))))
	 `(rainbow-delimiters-depth-7-face  ((t (:foreground ,fg1))))
	 `(rainbow-delimiters-depth-8-face  ((t (:foreground ,fg5))))
	 `(rainbow-delimiters-depth-9-face  ((t (:foreground ,fg1))))
	 `(rainbow-delimiters-depth-10-face ((t (:foreground ,fg4))))
	 `(rainbow-delimiters-depth-11-face ((t (:foreground ,fg1))))
	 `(rainbow-delimiters-depth-12-face ((t (:foreground ,fg5))))

	 ;; treemacs
	 `(treemacs-directory-face					((t (:foreground ,fg1))))
	 `(treemacs-directory-collapsed-face		((t (:foreground ,fg1))))
	 `(treemacs-file-face						((t (:foreground ,fg1))))
	 `(treemacs-root-face						((t (:foreground ,fg1  :bold t))))
	 `(treemacs-root-unreadable-face			((t (:foreground ,fg1  :underline t :strike-through t))))
	 `(treemacs-root-remote-face				((t (:foreground ,fg1))))
	 `(treemacs-root-remote-disconnected-face	((t (:foreground ,red))))
	 `(treemacs-root-remote-unreadable-face		((t (:foreground ,red))))
	 `(treemacs-term-node-face					((t (:foreground ,fg1))))
	 `(treemacs-git-unmodified-face				((t (:foreground ,fg1))))
	 `(treemacs-git-modified-face				((t (:foreground ,fg1 :italic t))))
	 `(treemacs-git-renamed-face				((t (:foreground ,fg1 :italic t))))
	 `(treemacs-git-added-face					((t (:foreground ,fg1))))
	 `(treemacs-git-ignored-face				((t (:foreground ,fg5 :italic nil))))
	 `(treemacs-git-untracked-face				((t (:foreground ,fg5 :italic nil))))
	 `(treemacs-git-conflict-face				((t (:foreground ,red))))
	 `(treemacs-on-failure-pulse-face			((t (:foreground ,red))))
	 `(treemacs-on-success-pulse-face			((t (:foreground ,ok))))
	 `(treemacs-tags-face						((t (:foreground ,fg1 :italic nil))))
	 `(treemacs-help-title-face					((t (:foreground ,fg1 :bold t))))
	 `(treemacs-help-column-face				((t (:foreground ,fg1 :bold t))))
	 `(treemacs-on-failure-pulse-face			((t (:background ,warning :foreground ,fg1 :extend t))))
	 `(treemacs-on-success-pulse-face			((t (:background ,ok :foreground ,fg1 :extend t))))
	 `(treemacs-fringe-indicator-face			((t (:background ,bg1 :foreground ,fg5))))
	 `(treemacs-peek-mode-indicator-face		((t (:background ,stimmung-themes-light-highlight-color))))
	 `(treemacs-header-button-face				((t (:background ,stimmung-themes-light-highlight-color))))
	 `(treemacs-header-button-face				((t (:foreground ,fg1 :background ,bg5 :box (:line-width 1 :color ,fg5 :style nil)))))

	 ;; tab-bar-mode
	 `(tab-bar              ((t (:background ,bg1 :foreground ,fg1))))
	 `(tab-bar-tab          ((t (:background ,bg5 :foreground ,fg1 :box (:line-width 1 :color ,fg5 :style nil)))))
	 `(tab-bar-tab-inactive ((t (:background ,bg1 :foreground ,fg5 :box (:line-width 1 :color ,fg2 :style nil)))))

	 ;; tab-line-mode
	 `(tab-line							((t (:background ,bg1 :foreground ,fg1))))
	 `(tab-line-tab						((t (:background ,bg5 :foreground ,fg1 :box (:line-width 1 :color ,fg5 :style nil)))))
	 `(tab-line-tab-current				((t (:background ,bg5 :foreground ,fg1 :box (:line-width 1 :color ,fg5 :style nil)))))
	 `(tab-line-highlight				((t (:background ,bg2 :foreground ,fg1 :box (:line-width 1 :color ,fg5 :style nil)))))
	 `(tab-line-close-highlight			((t (:foreground ,red))))
	 `(tab-line-tab-modified			((t (:italic t))))
	 `(tab-line-tab						((t (:background ,bg5 :foreground ,fg1 :italic t:box (:line-width 1 :color ,fg5 :style nil)))))
	 `(tab-line-tab-inactive			((t (:background ,bg1 :foreground ,fg5 :box (:line-width 1 :color ,fg2 :style nil)))))
	 `(tab-line-tab-inactive-alternate	((t (:background ,bg1 :foreground ,fg4 :box (:line-width 1 :color ,fg2 :style nil)))))

	 ;; perspective
	 `(persp-selected-face ((t (:bold t))))

	 ;; LaTeX
	 `(font-latex-sectioning-0-face ((t (:bold t))))
	 `(font-latex-sectioning-1-face ((t (:bold t))))
	 `(font-latex-sectioning-2-face ((t (:bold t))))
	 `(font-latex-sectioning-3-face ((t (:bold t))))
	 `(font-latex-sectioning-4-face ((t (:italic t))))
	 `(font-latex-sedate-face       ((t (:foreground ,fg5))))
	 `(font-latex-italic-face       ((t (:foreground ,fg1 :italic t))))
	 `(font-latex-bold-face         ((t (:foreground ,fg1 :bold t))))
	 `(font-latex-verbatim-face     ((t (:background ,stimmung-themes-light-highlight-color :bold t))))
	 `(font-latex-string-face       ((t (:foreground ,fg1))))
	 `(font-latex-warning-face      ((t (:foreground ,fg5))))
	 `(font-latex-math-face         ((t (:foreground ,fg1))))
	 `(font-latex-script-char-face  ((t (:foreground ,fg1))))

	 ;; copilot

	 `(copilot-overlay-face         ((t (:foreground ,fg5 :italic t))))

	 ;; eglot

	 `(eglot-highlight-symbol-face           ((t (:foregroud ,fg1 :background ,search))))
	 `(eglot-mode-line                       ((t (:foregroud ,fg1 :background ,bg5))))
	 `(eglot-diagnostic-tag-unnecessary-face ((t (:foregroud ,str-fg))))
	 `(eglot-diagnostic-tag-deprecated-face  ((t (:foregroud ,str-fg :strike-through t))))

	 ;; eldoc-box

	 `(eldoc-box-border ((t (:background ,fg5))))
	 `(eldoc-box-body   ((t (:foreground ,fg1 :background ,bg1))))

	 ;; lsp-mode

	 `(lsp-dired-path-face			((t (:foreground ,fg1))))
	 `(lsp-dired-path-error-face	((t (:foreground ,fg1 :underline (:style wave :color ,red)))))
	 `(lsp-dired-path-warning-face	((t (:foreground ,fg1 :underline (:style wave :color ,warning)))))
	 `(lsp-dired-path-info-face		((t (:foreground ,fg1 :italic t))))
	 `(lsp-dired-path-hint-face		((t (:foreground ,fg1 :italic t))))

	 `(lsp-modeline-code-actions-face			((t :foreground ,fg1 :bold t)))
	 `(lsp-modeline-code-actions-preferred-face	((t :foreground ,fg1 :bold t)))

	 `(lsp-lens-mouse-face			((t (:foreground ,fg5 :underline t))))
	 `(lsp-lens-face				((t (:foreground ,fg5))))
	 `(lsp-face-highlight-textual	((t (:foreground ,fg1 :background ,search2))))
	 `(lsp-face-highlight-read		((t (:foreground ,fg1 :background ,search2 :underline nil))))

	 `(lsp-headerline-breadcrumb-separator-face					((t (:foreground ,fg5))))
	 `(lsp-headerline-breadcrumb-path-face						((t (:foreground ,fg1))))
	 `(lsp-headerline-breadcrumb-path-error-face				((t (:foreground ,fg1 :underline (:style wave :color ,red)))))
	 `(lsp-headerline-breadcrumb-path-warning-face				((t (:foreground ,fg1 :underline (:style wave :color ,warning)))))
	 `(lsp-headerline-breadcrumb-path-info-face					((t (:foreground ,fg1))))
	 `(lsp-headerline-breadcrumb-path-hint-face					((t (:foreground ,fg1))))
	 `(lsp-headerline-breadcrumb-project-prefix-face			((t (:foreground ,fg1 :bold t))))
	 `(lsp-headerline-breadcrumb-unknown-project-prefix-face	((t (:foreground ,fg5))))
	 `(lsp-headerline-breadcrumb-symbols-face					((t (:foreground ,fg1))))
	 `(lsp-headerline-breadcrumb-symbols-error-face				((t (:foreground ,fg1 :underline (:style wave :color ,red)))))
	 `(lsp-headerline-breadcrumb-symbols-warning-face			((t (:foreground ,fg1 :underline (:style wave :color ,warning)))))
	 `(lsp-headerline-breadcrumb-symbols-info-face				((t (:foreground ,fg1))))
	 `(lsp-headerline-breadcrumb-symbols-hint-face				((t (:foreground ,fg1))))
	 `(lsp-headerline-breadcrumb-deprecated-face				((t (:foreground ,red))))

	 ;; LSP UI
	 `(lsp-ui-doc-background			((t (:foreground ,fg1 :background ,bg5 :box (:line-width 1 :color ,fg5 :style nil)))))
	 `(lsp-ui-doc-header				((t (:foreground ,fg1 :bold t))))
	 `(lsp-ui-doc-highlight-hover		((t (:foreground ,fg1 :background ,stimmung-themes-light-highlight-color))))
	 `(lsp-ui-doc-url					((t (:foreground ,fg1 :underline t))))

	 `(lsp-ui-peek-peek					((t (:foreground ,fg1 :background ,bg5 :box (:line-width 1 :color ,fg5 :style nil)))))
	 `(lsp-ui-peek-list					((t (:foreground ,fg1 :background ,bg5 :box (:line-width 1 :color ,fg5 :style nil)))))
	 `(lsp-ui-peek-filename				((t (:foreground ,fg5 :underline t))))
	 `(lsp-ui-peek-line-number			((t (:foreground ,fg5 :background ,bg1))))
	 `(lsp-ui-peek-highlight			((t (:foreground ,fg1 :background ,stimmung-themes-light-highlight-color))))
	 `(lsp-ui-peek-header				((t (:foreground ,fg1 :bold))))
	 `(lsp-ui-peek-footer				((t (:foreground ,fg5))))
	 `(lsp-ui-peek-selection			((t (:background ,bg4 :foreground ,fg1))))

	 `(lsp-ui-sideline-symbol			((t (:foreground ,fg1 :background ,search2))))
	 `(lsp-ui-sideline-current-symbol	((t (:foreground ,fg1 :background ,stimmung-themes-light-highlight-color))))
	 `(lsp-ui-sideline-code-action		((t (:foreground ,fg1 :background ,str))))
	 `(lsp-ui-sideline-symbol-info		((t (:foreground ,fg1 :background ,search2))))
	 `(lsp-ui-sideline-global			((t (:foreground ,fg1 :background ,bg1))))

	 ;; tree-sitter

	 `(tree-sitter-hl-face:attribute             ((t (:inherit 'font-lock-constant-face))))
	 `(tree-sitter-hl-face:comment               ((t (:inherit 'font-lock-comment-face))))
	 `(tree-sitter-hl-face:constant              ((t (:inherit 'font-lock-constant-face))))
	 `(tree-sitter-hl-face:constant.builtin      ((t (:inherit 'font-lock-constant-face))))
	 `(tree-sitter-hl-face:constructor           ((t (:foreground ,fg1 :background ,bg1))))
	 `(tree-sitter-hl-face:doc                   ((t (:inherit 'font-lock-comment-face))))
	 `(tree-sitter-hl-face:embedded              ((t (:foreground ,fg1 :background ,bg1))))
	 `(tree-sitter-hl-face:escape                ((t (:foreground ,fg1 :background ,bg1))))
	 `(tree-sitter-hl-face:function              ((t (:inherit 'font-lock-function-name-face))))
	 `(tree-sitter-hl-face:function.builtin      ((t (:inherit 'font-lock-function-name-face))))
	 `(tree-sitter-hl-face:function.call         ((t (:foreground ,fg1 :background ,bg1))))
	 `(tree-sitter-hl-face:function.macro        ((t (:foreground ,fg1 :background ,bg1))))
	 `(tree-sitter-hl-face:function.special      ((t (:foreground ,fg1 :background ,bg1))))
	 `(tree-sitter-hl-face:keyword               ((t (:foreground ,fg1 :background ,bg1))))
	 `(tree-sitter-hl-face:label                 ((t (:foreground ,fg1 :background ,bg1))))
	 `(tree-sitter-hl-face:method                ((t (:foreground ,fg1 :background ,bg1))))
	 `(tree-sitter-hl-face:method.call           ((t (:foreground ,fg1 :background ,bg1))))
	 `(tree-sitter-hl-face:number                ((t (:foreground ,fg1 :background ,bg1))))
	 `(tree-sitter-hl-face:operator              ((t (:foreground ,fg1 :background ,bg1))))
	 `(tree-sitter-hl-face:property              ((t (:foreground ,fg1 :background ,bg1))))
	 `(tree-sitter-hl-face:property.definition   ((t (:foreground ,fg1 :background ,bg1))))
	 `(tree-sitter-hl-face:punctuation           ((t (:foreground ,fg1 :background ,bg1))))
	 `(tree-sitter-hl-face:punctuation.bracket   ((t (:foreground ,fg1 :background ,bg1))))
	 `(tree-sitter-hl-face:punctuation.special   ((t (:foreground ,fg1 :background ,bg1))))
	 `(tree-sitter-hl-face:punctuation.delimiter ((t (:foreground ,fg1 :background ,bg1))))
	 `(tree-sitter-hl-face:string                ((t (:inherit 'font-lock-string-face))))
	 `(tree-sitter-hl-face:string.special        ((t (:inherit 'font-lock-string-face))))
	 `(tree-sitter-hl-face:tag                   ((t (:foreground ,fg1 :background ,bg1 :bold t))))
	 `(tree-sitter-hl-face:type                  ((t (:inherit font-lock-type-face))))
	 `(tree-sitter-hl-face:type.argument         ((t (:foreground ,fg1 :background ,bg1 :italic t))))
	 `(tree-sitter-hl-face:type.builtin          ((t (:foreground ,fg1 :background ,bg1 :italic t))))
	 `(tree-sitter-hl-face:type.parameter        ((t (:foreground ,fg1 :background ,bg1 :italic t))))
	 `(tree-sitter-hl-face:type.super            ((t (:foreground ,fg1 :background ,bg1 :italic t))))
	 `(tree-sitter-hl-face:type.variable         ((t (:foreground ,fg1 :background ,bg1 :italic t))))
	 `(tree-sitter-hl-face:variable.builtin      ((t (:foreground ,fg1 :background ,bg1))))
	 `(tree-sitter-hl-face:variable.parameter    ((t (:foreground ,fg1 :background ,bg1))))
	 `(tree-sitter-hl-face:variable.special      ((t (:foreground ,fg1 :background ,bg1))))

	 ;; Typescript

	 `(typescript-jsdoc-value ((t (:foreground ,fg1 :background ,stimmung-themes-light-highlight-color))))
	 `(typescript-jsdoc-type  ((t (:inherit font-lock-type-face))))
	 `(typescript-jsdoc-tag   ((t (:foreground ,fg1 :bold t))))

	 ;; rg.el

	 `(rg-match-face          ((t (:foreground ,fg1 :background ,search2))))
	 `(rg-error-face          ((t (:inherit error))))
	 `(rg-warning-face        ((t (:inherit warning))))
	 `(rg-context-face        ((t (:foreground ,fg5))))
	 `(rg-filename-face       ((t (:foreground ,fg1 :bold t))))
	 `(rg-info-face           ((t (:foreground ,fg1))))
	 `(rg-tag-face            ((t ,(user-controlled-with stimmung-themes-constant :italic? t))))
	 `(rg-file-tag-face       ((t ,(user-controlled-with stimmung-themes-constant :italic? t))))
	 `(rg-line-number-face    ((t (:inherit 'line-number))))
	 `(rg-column-number-face  ((t (:inherit 'line-number))))
	 `(rg-match-position-face ((t (:inherit 'line-number))))

	 ;; DAP

	 ;; TODO I'm not sure how popular DAP is is, but it would be nice to support it.
	 ;;      It has a million faces though, so it would be great if someone who has
	 ;;      experience would like to help!

	 ;; re-builder
	 `(reb-match-0 ((t (:foreground ,fg1 :inverse-video t :bold t))))
	 `(reb-match-1 ((t (:foreground ,fg1 :inverse-video t :bold t))))
	 `(reb-match-2 ((t (:foreground ,fg1 :inverse-video t :bold t))))
	 `(reb-match-3 ((t (:foreground ,fg1 :inverse-video t :bold t))))

	 ;; undo-tree
	 `(undo-tree-visualizer-default-face       ((t (:foreground ,fg1))))
	 `(undo-tree-visualizer-current-face       ((t (:foreground ,ok :bold t))))
	 `(undo-tree-visualizer-unmodified-face    ((t (:foreground ,fg1 :italic t))))
	 `(undo-tree-visualizer-active-branch-face ((t (:foreground ,fg1))))
	 `(undo-tree-visualizer-register-face      ((t (:foreground ,fg1))))

	 `(window-divider             ((t (:foreground ,bg1))))
	 `(window-divider-first-pixel ((t (:foreground ,bg1))))
	 `(window-divider-last-pixel  ((t (:foreground ,bg1))))

	 ;; wo/man
	 `(Man-overstrike ((t (:foreground ,fg1 :bold t))))
	 `(Man-underline  ((t (:foreground ,fg1 :underline nil :italic t))))
	 `(woman-bold     ((t (:inherit 'Man-overstrike))))
	 `(woman-italic   ((t (:inherit 'Man-underline))))

	 ;; web-mode
	 `(web-mode-doctype-face           ((t (:foreground ,fg1))))
	 `(web-mode-html-tag-face          ((t (:foreground ,fg1 :italic t))))
	 `(web-mode-html-tag-bracket-face  ((t (:foreground ,fg1))))
	 `(web-mode-html-attr-name-face    ((t (:foreground ,fg1 :bold t))))
	 `(web-mode-html-entity-face       ((t (:foreground ,fg1 :italic t))))
	 `(web-mode-block-control-face     ((t (:foreground ,fg1))))
	 `(web-mode-html-tag-bracket-face  ((t (:foreground ,fg1 :bold t))))

	 ;; visual-regexp
	 `(vr/match-0 ((t (:background ,search2 :foreground ,fg1))))
	 `(vr/match-1 ((t (:background ,search2 :foreground ,fg1))))
	 `(vr/group-0 ((t (:background ,search2 :foreground ,fg1))))
	 `(vr/group-1 ((t (:background ,search2 :foreground ,fg1))))
	 `(vr/group-2 ((t (:background ,search2 :foreground ,fg1))))

	 ;; elfeed
	 `(elfeed-search-date-face         ((t (:foreground ,fg5 :inherit bold))))
	 `(elfeed-search-feed-face         ((t (:foreground ,fg1 :background ,bg5))))
	 `(elfeed-search-filter-face       ((t (:inherit bold :foreground ,fg1 :background ,bg3))))
	 `(elfeed-search-last-update-face  ((t (:foreground ,fg5 :inherit bold))))
	 `(elfeed-search-tag-face          ((t (:foreground ,fg5))))
	 `(elfeed-search-title-face        ((t (:foreground ,fg5))))
	 `(elfeed-search-unread-title-face ((t (:foreground ,fg1 :inherit bold))))
	 `(elfeed-search-unread-count-face ((t (:foreground ,fg5 :inherit bold))))
	 `(elfeed-log-date-face            ((t (:inherit elfeed-search-date-face))))
	 `(elfeed-log-debug-level-face     ((t (:inherit elfeed-search-filter-face))))
	 `(elfeed-log-error-level-face     ((t (:inherit error))))
	 `(elfeed-log-info-level-face      ((t (:inherit success))))
	 `(elfeed-log-warn-level-face      ((t (:inherit warning))))

	 ;; notmuch
	 `(notmuch-crypto-decryption           ((t (:inherit (shadow bold)))))
	 `(notmuch-crypto-part-header          ((t (:inherit message-mml))))
	 `(notmuch-crypto-signature-bad        ((t (:inherit error))))
	 `(notmuch-crypto-signature-unknown    ((t (:inherit warning))))
	 `(notmuch-crypto-signature-good       ((t (:inherit success))))
	 `(notmuch-crypto-signature-good-key   ((t (:inherit success))))
	 `(notmuch-hello-logo-background       ((t (:background ,bg5))))
	 `(notmuch-jump-key                    ((t (:inherit help-key-binding))))
	 `(notmuch-message-summary-face        ((t (:foreground ,fg1))))
	 `(notmuch-search-count                ((t (:foreground ,fg5))))
	 `(notmuch-search-date                 ((t (:foreground ,fg5 :inherit bold))))
	 `(notmuch-search-flagged-face         ((t (:background ,search2))))
	 `(notmuch-search-matching-authors     ((t (:foreground ,fg1))))
	 `(notmuch-search-non-matching-authors ((t (:inherit notmuch-tree-no-match-face))))
	 `(notmuch-search-subject              ((t (:foreground ,fg1))))
	 `(notmuch-search-unread-face          ((t (:foreground ,fg1 :inherit bold))))
	 `(notmuch-tag-added                   ((t (:underline ,fg1 :background ,bg6))))
	 `(notmuch-tag-deleted                 ((t (:strike-through ,red))))
	 `(notmuch-tag-face                    ((t (:foreground ,fg5))))
	 `(notmuch-tag-flagged                 ((t (:foreground ,fg1))))
	 `(notmuch-tag-unread                  ((t (:foreground ,fg1 :background ,stimmung-themes-light-highlight-color))))
	 `(notmuch-tree-match-author-face      ((t (:inherit notmuch-search-matching-authors))))
	 `(notmuch-tree-match-date-face        ((t (:inherit notmuch-search-date))))
	 `(notmuch-tree-match-face             ((t (:foreground ,fg1))))
	 `(notmuch-tree-match-tag-face         ((t (:inherit notmuch-tag-face))))
	 `(notmuch-tree-no-match-face          ((t (:foreground ,fg4 :background ,bg5))))
	 `(notmuch-tree-no-match-date-face     ((t (:foreground ,fg4 :background ,bg5))))
	 `(notmuch-wash-cited-text             ((t (:inherit message-cited-text-1))))
	 `(notmuch-wash-toggle-button          ((t (:foreground ,fg5 :background ,bg5))))

	 ;; white-space
	 `(whitespace-empty       ((t (:background ,bg3))))
	 `(whitespace-space       ((t (:foreground ,fg1))))
	 `(whitespace-newline     ((t (:foreground ,fg1))))
	 `(whitespace-tab         ((t (:foreground ,fg1  :background ,bg1))))
	 `(whitespace-indentation ((t (:foreground ,red  :background ,fg1))))
	 `(whitespace-line        ((t (:foreground ,red  :background ,fg1 :weight bold))))
	 `(nobreak-space          ((t (:inherit 'default :underline nil))))
	 `(whitespace-trailing    ((t (:foreground ,red))))

	 ;; yasnippet
	 `(yas-field-highlight-face ((t (:background ,bg5 :foreground ,fg1 :box (:line-width 1 :color ,fg5)))))

	 ;; xref

	 `(next-error-message ((t (:background ,search2))))
	 `(xref-file-header ((t (:foreground ,fg1 :bold t))))
	 `(xref-match       ((t (:foreground ,fg1))))
	 `(xref-line-number ((t (:inherit 'line-number))))
	 )))

(custom-theme-set-variables
 'stimmung-themes-light
 '(ansi-color-names-vector ["black" "black" "black" "black"
							"black" "black" "black" "white"]))

;;;###autoload
(when (and (boundp 'custom-theme-load-path)
		   load-file-name)
  (add-to-list 'custom-theme-load-path
			   (file-name-as-directory
				(file-name-directory load-file-name))))

(provide-theme 'stimmung-themes-light)
(provide 'stimmung-themes-light-theme)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; stimmung-themes-light-theme.el ends here
