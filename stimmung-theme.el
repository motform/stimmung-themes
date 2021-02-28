;;; stimmung-theme.el --- A theme tuned to inner harmonies. -*- lexical-binding: t -*-

;; Copyright © 2019

;; Author: Love Lagerkvist
;; URL: https://github.com/motform/stimmung
;; Package-Requires: ((emacs "24"))
;; Created: 2019-12-20
;; Version: 2021-01-01
;; Keywords: color theme

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

;; The idea behind this theme is to decrease fruit salad factor,
;; emphasize comments and mesh well with the colors used by MacOS (Big Sur) dark mode.
;; Stimmung makes heavy use of typographic features to distinguish syntactic elements
;; instead of with colors.  Thus it assumes a font with bold, italic and bold italic.

;;; Code:

(deftheme stimmung
  "A theme tuned to inner harmonies.")

(defgroup stimmung nil
  "Stimmung theme settings.

You might have to re-load the theme for these changes to take effect."
  :group 'faces
  :prefix "stimmung-")

(defcustom stimmung-highlight-color "bisque1"
  "The primarily color for highlights, the only non-monochrome color in code."
  :type 'string
  :group 'stimmung)

(custom-theme-set-faces
 'stimmung

 `(default  ((t (:background "gray12" :foreground "white"))))
 `(shadow   ((t (:background "gray15"))))
 `(hl-line  ((t (:background "gray15" :extend t))))

 `(region              ((t (:background "gray30"))))
 `(secondary-selection ((t (:background "gray20"     :foreground "IndianRed1" :bold t))))
 `(highlight           ((t (:foreground "white"      :background "gray20" :bold t))))
 `(lazy-highlight      ((t (:foreground "IndianRed1" :background "gray12" :bold t))))
 `(default             ((t (:foreground "white"      :background "gray12"))))
 `(fringe              ((t (:foreground "white"      :background "gray12"))))
 `(match               ((t (:foreground "PaleGreen4" :bold t))))
 `(link                ((t (:foreground ,stimmung-highlight-color    :underline t))))
 `(link-visited        ((t (:foreground ,stimmung-highlight-color    :underline t :italic t))))
 `(button              ((t (:foreground ,stimmung-highlight-color    :underline t))))
 `(header-line         ((t (:foreground ,stimmung-highlight-color    :bold t))))
 `(tooltip             ((t (:foreground "white"      :background "gray20"))))
 `(vertical-border     ((t (:foreground "gray15"     :background "gray15"))))
 `(info-string         ((t (:foreground ,stimmung-highlight-color))))
 `(default-italic      ((t (:slant italic))))

 `(error                       ((t (:foreground "IndianRed1"))))
 `(warning                     ((t (:foreground "IndianRed1"))))
 `(success                     ((t (:foreground "PaleGreen4"))))
 `(cancel                      ((t (:foreground "IndianRed1" :strike-through t))))
 
 `(minibuffer-noticable-prompt ((t (:foreground "gray60"     :bold t))))
 `(minibuffer-prompt           ((t (:foreground "gray60"     :bold t))))
 `(isearch                     ((t (:foreground "IndianRed1" :background "white" :bold t))))
 `(isearch-highlight           ((t (:foreground "IndianRed1" :background "white"))))
 `(isearch-fail                ((t (:foreground "IndianRed1" :background "gray12"))))
 `(paren-matched               ((t (:foreground "PaleGreen4" :background "gray12"))))
 `(paren-unmatched             ((t (:foreground "IndianRed1" :background "gray12"))))
 `(escape-glyph                ((t (:foreground "IndianRed1" :bold t))))
 `(homoglyph                   ((t (:foreground "IndianRed1" :bold t))))
 
 `(line-number              ((t (:foreground "gray60" :background "gray12"))))
 `(line-number-current-line ((t (:foreground "white"  :background "gray12"))))
 `(linum                    ((t (:inherit 'line-number))))
 
 ;; syntax
 `(font-lock-builtin-face              ((t (:foreground ,stimmung-highlight-color :italic t))))
 `(font-lock-comment-delimiter-face    ((t (:foreground "gray65"  :italic t))))
 `(font-lock-comment-face              ((t (:foreground "gray65"  :italic t))))
 `(font-lock-doc-face                  ((t (:foreground "gray65"  :italic t))))
 `(font-lock-constant-face             ((t (:foreground ,stimmung-highlight-color :bold t :italic t))))
 `(font-lock-function-name-face        ((t (:foreground "white"   :bold t))))
 `(font-lock-keyword-face              ((t (:foreground "gray60"  :bold t))))
 `(font-lock-type-face                 ((t (:foreground ,stimmung-highlight-color :bold t))))
 `(font-lock-variable-name-face        ((t (:foreground "white"   :italic t))))
 `(font-lock-negation-char-face        ((t (:foreground "white"   :bold t))))
 `(font-lock-preprocessor-face         ((t (:foreground "white"   :bold t))))
 `(font-lock-preprocessor-char-face    ((t (:foreground "white"   :bold t))))
 `(font-lock-regexp-grouping-backslash ((t (:foreground "white"   :bold t))))
 `(font-lock-regexp-grouping-construct ((t (:foreground "white"   :bold t))))
 `(font-lock-string-face               ((t (:foreground "gray60"))))
 `(font-lock-warning-face              ((t (:foreground "IndianRed1"))))
 
 ;; eshell
 `(eshell-prompt        ((t (:foreground "gray60"     :bold t))))
 `(eshell-ls-directory  ((t (:foreground ,stimmung-highlight-color    :bold t))))
 `(eshell-ls-executable ((t (:foreground "gray20"     :bold t))))
 `(eshell-ls-symlink    ((t (:foreground "white"      :italic t))))
 `(eshell-ls-special    ((t (:foreground "PaleGreen4" :italic t))))
 `(eshell-ls-backup     ((t (:foreground "gray60"     :italic t))))
 `(eshell-ls-readonly   ((t (:foreground "IndianRed1"))))
 `(eshell-ls-unreadable ((t (:foreground "gray20")))) ; too dark?
 `(eshell-ls-missing    ((t (:foreground "IndianRed1"))))
 `(eshell-ls-product    ((t (:foreground "white"))))
 `(eshell-ls-archive    ((t (:foreground "gray60"))))
 `(eshell-ls-entries    ((t (:foreground "white"))))
 
 ;; avy
 `(avy-lead-face   ((t (:background "gray15" :foreground "white" :distant-foreground "gray65" :bold t))))
 `(avy-lead-face-0 ((t (:inherit 'avy-lead-face))))
 `(avy-lead-face-1 ((t (:inherit 'avy-lead-face))))
 `(avy-lead-face-2 ((t (:inherit 'avy-lead-face))))
 
 ;; flyspell
 `(flyspell-incorrect       ((t (:underline (:style wave :color "IndianRed1") ))))
 `(flyspell-duplicate       ((t (:underline (:style wave :color ,stimmung-highlight-color)))))
 `(flycheck-error           ((t (:underline (:style wave :color "IndianRed1")))))
 `(flysheck-warning         ((t (:underline (:style wave :color ,stimmung-highlight-color)))))
 `(flysheck-warning-overlay ((t (:underline (:style wave :color ,stimmung-highlight-color)))))
 `(flycheck-note            ((t (:underline (:style wave :color "PaleGreen4")))))
 
 ;; hydra
 `(hydra-face-red      ((t (:foreground "white"  :bold t))))
 `(hydra-face-blue     ((t (:foreground "gray60" :bold t))))
 `(hydra-face-amaranth ((t (:foreground "gray60" :bold t))))
 `(hydra-face-pink     ((t (:foreground "gray60" :bold t))))
 `(hydra-face-teal     ((t (:foreground "gray60" :bold t))))
 
 ;; cider
 
 ;; company
 `(company-scrollbar-bg             ((t (:background "gray60"))))
 `(company-scrollbar-fg             ((t (:foreground "white"))))
 `(company-echo-common              ((t (:background "white"  :foreground "gray12"))))
 `(company-preview                  ((t (:background "gray12" :foreground ,stimmung-highlight-color))))
 `(company-tooltip                  ((t (:background "gray20" :foreground "white"))))
 `(company-tooltip-annotation       ((t (:foreground ,stimmung-highlight-color))))
 `(company-tooltip-common           ((t (:foreground "gray60"))))
 `(company-tooltip-common-selection ((t (:foreground ,stimmung-highlight-color))))
 `(company-tooltip-selection        ((t (:background "gray20" :foreground "gray60"))))
 `(company-tooltip-selection-       ((t (:background "gray20" :foreground "gray60"))))
 `(company-tooltip-mouse            ((t (:inherit highlight))))
 
 ;; compilation
 `(compilation-line-number    ((t (:foreground ,stimmung-highlight-color :bold t))))
 `(compilation-column-number  ((t (:inherit 'font-lock-comment-face))))
 `(compilation-error          ((t (:inherit 'error   :bold t))))
 `(compilation-warning        ((t (:inherit 'warning :italic t))))
 `(compilation-info           ((t (:inherit ,success))))
 `(compilation-mode-line-exit ((t (:inherit 'compilation-info))))
 `(compilation-mode-line-fail ((t (:inherit 'compilation-error))))
 
 ;; TODO
 ;; custom
 `(custom-variable-tag    ((t (:foreground ,stimmung-highlight-color :bold t))))

 ;; modeline
 `(header-line         ((t (:inherit 'mode-line  :distant-foreground "gray12"))))
 `(mode-line           ((t (:foreground "white"  :background "gray20"))))
 `(mode-line-inactive  ((t (:foreground "gray60" :background "gray12"))))
 `(mode-line-buffer-id ((t (:foreground "white"  :bold t :italic t))))
 `(mode-line-emphasis  ((t (:foreground "white"  :bold t))))
 `(mode-line-highlight ((t (:foreground ,stimmung-highlight-color))))
 
 ;; doom-modeline
 `(doom-modeline-buffer-path        ((t (:foreground "gray60"))))
 `(doom-modeline-buffer-file        ((t (:foreground "white"      :weight bold))))
 `(doom-modeline-buffer-modified    ((t (:foreground "IndianRed1" :weight bold))))
 `(doom-modeline-project-dir        ((t (:foreground "white"      :weight bold))))
 `(doom-modeline-project-root-dir   ((t (:foreground "gray60"     :weight normal))))
 `(doom-modeline-project-parent-dir ((t (:foreground "gray60"     :weight normal))))
 `(doom-modeline-bar-inactive       ((t (:foreground "white"      :background ,stimmung-highlight-color))))
 `(doom-modeline-bar                ((t (:background "gray12")))) ; the leftmost bar
 `(doom-modeline-evil-insert-state  ((t (:foreground "white"))))
 `(doom-modeline-evil-visual-state  ((t (:foreground "white"))))
 `(doom-modeline-evil-normal-state  ((t (:foreground "gray60"))))
 `(doom-modeline-evil-emacs-state   ((t (:foreground "IndianRed1" :italic nil))))
 
 ;; dired
 `(dired-directory  ((t (:foreground "gray60" :bold t))))
 `(dired-ignored    ((t (:foreground "gray65"))))
 `(dired-flagged    ((t (:foreground "PaleGreen4"))))
 `(dired-header     ((t (:foreground "white"      :bold t))))
 `(dired-mark       ((t (:foreground "IndianRed1" :bold t))))
 `(dired-marked     ((t (:foreground "IndianRed1" :bold t :italic t))))
 `(dired-perm-write ((t (:foreground "white"      :underline t))))
 `(dired-symlink    ((t (:foreground "white"      :italic t))))
 `(dired-warning    ((t (:foreground "IndianRed1"))))
 
 ;; evil
 `(evil-ex-info                   ((t (:foreground "IndianRed1" :italic t))))
 `(evil-ex-search                 ((t (:background "gray15" :foreground "white" :bold t))))
 ;; (evil-ex-sub,stitute-matches     :background base0 :foreground red   :strike-through t :weight 'bold)
 ;; (evil-ex-sub,stitute-replacement :background base0 :foreground green :weight 'bold)
 `(evil-search-highlight-persist-highlight-face ((t (:inherit 'lazy-highlight))))
 
 ;; evil-mc
 `(evil-mc-cursor-default-face ((t (:foreground "gray12" :background "white"))))
 `(evil-mc-region-face         ((t (:foreground "gray12" :background "white"))))
 `(evil-mc-cursor-bar-face     ((t (:foreground "gray65"))))
 `(evil-mc-cursor-hbar-face    ((t (:foreground "gray65"))))
 
 ;; info
 `(info-quoted    ((t (:foreground ,stimmung-highlight-color :inherit 'default :bold t))))
 `(info-menu-star ((t (:foreground "white" :bold t))))
 ;; NOTE this should maybe have another color

 ;; ivy
 `(ivy-current-match              ((t (:foreground ,stimmung-highlight-color :background "gray12" :bold t))))
 `(ivy-minibuffer-match-highlight ((t (:foreground "PaleGreen4"))))
 `(ivy-minibuffer-match-face-1    ((t (:foreground "gray65" :bold t :italic t))))
 `(ivy-minibuffer-match-face-2    ((t (:foreground "gray65" :bold t :italic t))))
 `(ivy-minibuffer-match-face-3    ((t (:foreground "gray65" :bold t :italic t))))
 `(ivy-minibuffer-match-face-4    ((t (:foreground "gray65" :bold t :italic t))))
 `(ivy-confirm-face               ((t (:foreground "PaleGreen4"))))
 `(ivy-required-face              ((t (:foreground "IndianRed1"))))
 `(ivy-subdir                     ((t (:foreground "gray65"))))
 `(ivy-modified-buffer            ((t (:foreground "IndianRed1" :bold t))))
 `(ivy-modified-outside-buffer    ((t (:foreground "IndianRed1"))))
 `(ivy-remote                     ((t (:foreground "gray65"))))
 `(ivy-virtual                    ((t (:foreground "gray65" :italic t))))
 `(ivy-prompt                     ((t (:foreground "IndianRed1"))))
 `(ivy-prompt-match               ((t (:foreground "IndianRed1"))))
 `(ivy-separator                  ((t (:foreground ,stimmung-highlight-color))))
 `(ivy-highlight-face             ((t (:foreground "IndianRed1"))))
 `(ivy-grep-info                  ((t (:foreground "IndianRed1"))))
 `(ivy-completions-annotations    ((t (:foreground "IndianRed1"))))
 
 ;; TODO magit
 `(magit-bisect-bad        ((t (:foreground "IndianRed1"))))
 `(magit-bisect-good       ((t (:foreground "PaleGreen4"))))
 `(magit-bisect-skip       ((t (:foreground "gray65"))))
 `(magit-blame-date        ((t (:foreground "IndianRed1"))))
 `(magit-branch            ((t (:foreground ,stimmung-highlight-color :bold t))))
 
 `(magit-diff-context-highlight ((t (:foreground "gray60" :background "gray20"))))
 `(magit-diff-file-header       ((t (:foreground "gray60" :background "gray20"))))
 `(magit-diffstat-added         ((t (:foreground "PaleGreen4"))))
 `(magit-diffstat-removed       ((t (:foreground "IndianRed1"))))
 `(magit-dimmed                 ((t (:foreground "gray65"))))
 `(magit-hash                   ((t (:foreground "gray60"))))
 `(magit-hunk-heading           ((t (:background "gray20"))))
 `(magit-hunk-heading-highlight ((t (:background "gray20"))))
 `(magit-item-highlight         ((t (:background "gray20"))))
 `(magit-log-author             ((t (:foreground "gray60"))))
 `(magit-process-ng             ((t (:foreground ,stimmung-highlight-color :bold t))))
 `(magit-process-ok             ((t (:foreground "PaleGreen4" :bold t))))
 `(magit-section-heading        ((t (:foreground "gray60" :bold t))))
 `(magit-section-highlight      ((t (:background "gray20"))))
 
 ;; diff-hl
 `(diff-hl-insert         ((t (:foreground "PaleGreen4" :background "gray12" :bold nil :italic nil))))
 `(diff-hl-delete         ((t (:foreground "IndianRed1" :background "gray12" :bold nil :italic nil))))
 `(diff-hl-change         ((t (:foreground "gray60"     :background "gray12" :bold nil :italic nil))))
 `(diff-hl-ignore         ((t (:foreground "gray65"     :background "gray12" :bold nil :italic nil))))
 `(diff-hl-margin-ignore  ((t (:foreground "gray65"     :background "gray12" :bold nil :italic nil))))
 `(diff-hl-margin-unknown ((t (:foreground "gray65"     :background "gray12" :bold nil :italic nil))))
 
 ;; outline, extends org-outline
 `(outline-1 ((t (:foreground "white" :bold t :extend t))))
 `(outline-2 ((t (:foreground "white" :bold t :extend t))))
 `(outline-3 ((t (:foreground "white" :bold t :extend t))))
 `(outline-4 ((t (:foreground "white" :bold t :extend t))))
 `(outline-5 ((t (:foreground "white" :bold t :extend t))))
 `(outline-6 ((t (:foreground "white" :bold t :extend t))))
 `(outline-7 ((t (:foreground "white" :bold t :extend t))))
 `(outline-8 ((t (:foreground "white" :bold t :extend t))))

 ;; TODO org-agenda
 
 ;; org
 `(org-code                  ((t (:foreground ,stimmung-highlight-color :distant-foreground "gray12" :background ,stimmung-highlight-color))))
 `(org-block                 ((t (:foreground "white"        :background "gray20" :extend t))))
 `(org-block-begin-line      ((t (:foreground "gray60"       :background "gray20" :bold t :extend t)))) ; could be a better fg
 `(org-block-end-line        ((t (:foreground "gray60"       :background "gray20" :bold t :extend t))))
 `(org-date                  ((t (:foreground ,stimmung-highlight-color :bold t))))
 `(org-drawer                ((t (:foreground "gray60"       :bold t))))
 `(org-document-info         ((t (:foreground "white"        :background "gray12" :italic t)))) ;; BUG does not seem to correctly color fg
 `(org-document-info-keyword ((t (:foreground "gray65"       :background "gray12"))))
 `(org-document-title        ((t (:foreground "white"        :weight bold))))
 `(org-done                  ((t (:foreground "PaleGreen4"   :bold t :strike-through t))))
 `(org-ellipsis              ((t (:foreground "gray65"))))
 `(org-footnote              ((t (:foreground ,stimmung-highlight-color))))
 `(org-formula               ((t (:foreground "gray65"))))
 `(org-headline-done         ((t (:foreground "gray65"       :weight normal :strike-through t))))
 `(org-hide                  ((t (:foreground "gray12"       :background "gray12"))))
 `(org-link                  ((t (:foreground ,stimmung-highlight-color      :bold t :underline t)))) ;; BUG foreground not respected
 `(org-list-dt               ((t (:foreground "gray65"       :bold t))))
 `(org-priority              ((t (:foreground ,stimmung-highlight-color))))
 `(org-scheduled             ((t (:foreground "IndianRed1"))))
 `(org-scheduled-previously  ((t (:foreground ,stimmung-highlight-color))))
 `(org-scheduled-today       ((t (:foreground "PaleGreen4"))))
 `(org-sexp-date             ((t (:foreground ,stimmung-highlight-color))))
 `(org-special-keyword       ((t (:foreground ,stimmung-highlight-color))))
 `(org-table                 ((t (:foreground "gray65"))))
 `(org-tag                   ((t (:foreground "gray65"       :background "gray12" :bold t))))
 `(org-todo                  ((t (:foreground "IndianRed1"   :bold t))))
 `(org-warning               ((t (:foreground "IndianRed1"   :bold t))))
 `(org-upcoming-deadline     ((t (:foreground "IndianRed1"))))
 
 ;; markdown mode
 `(markdown-header-face             ((t (:foreground "white"  :bold t))))
 `(markdown-list-face               ((t (:foreground "gray60" :bold t))))
 `(markdown-bold-face               ((t (:foreground "white"  :bold t))))
 `(markdown-blockquote-face         ((t (:foreground "gray65" :italic t))))
 `(markdown-italic-face             ((t (:foreground "white"  :italic t))))
 `(markdown-link-face               ((t (:foreground "gray65" :underline t))))
 `(markdown-url-face                ((t (:foreground "gray65" :underline t))))
 `(markdown-header-delimiter-face   ((t (:inherit 'markdown-header-face))))
 `(markdown-metadata-key-face       ((t (:foreground "gray65"))))
 `(markdown-markup-face             ((t (:foreground "white"))))
 `(markdown-pre-face                ((t (:foreground "white"))))
 `(markdown-code-face               ((t (:background "gray60" :extend t))))
 `(markdown-reference-face          ((t (:foreground "gray65"))))
 `(markdown-html-attr-name-face     ((t (:inherit 'font-lock-variable-name-face))))
 `(markdown-html-attr-value-face    ((t (:inherit 'font-lock-string-face))))
 `(markdown-html-entity-face        ((t (:inherit 'font-lock-variable-name-face))))
 `(markdown-html-tag-delimiter-face ((t (:inherit 'markdown-markup-face))))
 `(markdown-html-tag-name-face      ((t (:inherit 'font-lock-keyword-face))))
 `(markdown-inline-code-face        ((t (:inherit 'markdown-code-face :extend nil))))
 
 ;; show-paren
 `(show-paren-match-face       ((t (:background "gray12"     :foreground "red" :bold t :italic t))))
 `(show-paren-match            ((t (:background "gray12"     :foreground "red" :bold t :italic t))))
 `(show-paren-match-expression ((t (:background "gray12"     :foreground "red" :bold t :italic t))))
 `(show-paren-mismatch         ((t (:background "IndianRed1" :foreground "gray12" :bold t :italic t))))
 
 ;; smartparens
 `(sp-show-pair-match-face    ((t (:inherit 'paren-matched))))
 `(sp-show-pair-mismatch-face ((t (:inherit 'paren-unmatched))))
 
 ;; LaTeX
 `(font-latex-sectioning-0-face ((t (:foreground ,stimmung-highlight-color :bold t))))
 `(font-latex-sectioning-1-face ((t (:foreground ,stimmung-highlight-color :bold t))))
 `(font-latex-sectioning-2-face ((t (:foreground ,stimmung-highlight-color :bold t))))
 `(font-latex-sectioning-3-face ((t (:foreground ,stimmung-highlight-color :bold t :italic t))))
 `(font-latex-sectioning-4-face ((t (:foreground ,stimmung-highlight-color :italic t))))
 `(font-latex-italic-face       ((t (:foreground "white"   :italic t))))
 `(font-latex-bold-face         ((t (:foreground "white"   :bold t))))
 `(font-latex-verbatim-face     ((t (:foreground ,stimmung-highlight-color :bold t))))
 `(font-latex-string-face       ((t (:foreground "gray60"))))
 `(font-latex-warning-face      ((t (:foreground "IndianRed1"))))
 `(font-latex-math-face         ((t (:foreground "gray60"))))
 `(font-latex-script-char-face  ((t (:foreground ,stimmung-highlight-color))))

 ;; re-builder
 `(reb-match-0 ((t (:foreground "white" :inverse-video t :bold t))))
 `(reb-match-1 ((t (:foreground "white" :inverse-video t :bold t))))
 `(reb-match-2 ((t (:foreground "white" :inverse-video t :bold t))))
 `(reb-match-3 ((t (:foreground "white" :inverse-video t :bold t))))
 
 ;; undo-tree
 `(undo-tree-visualizer-default-face       ((t (:foreground "white"))))
 `(undo-tree-visualizer-current-face       ((t (:foreground "PaleGreen4" :bold t))))
 `(undo-tree-visualizer-unmodified-face    ((t (:foreground "gray60" :italic t))))
 `(undo-tree-visualizer-active-branch-face ((t (:foreground "white"))))
 `(undo-tree-visualizer-register-face      ((t (:foreground "white"))))

 `(window-divider             ((t (:foreground "gray12"))))
 `(window-divider-first-pixel ((t (:foreground "gray12"))))
 `(window-divider-last-pixel  ((t (:foreground "gray12"))))
 
 ;; wo/man
 `(Man-overstrike ((t (:foreground "gray65" :bold t))))
 `(Man-underline  ((t (:foreground "gray65" :underline nil :italic t))))
 `(woman-bold     ((t (:inherit 'Man-overstrike))))
 `(woman-italic   ((t (:inherit 'Man-underline))))
 
 ;; web-mode
 `(web-mode-doctype-face           ((t (:foreground "gray65"))))
 `(web-mode-html-tag-face          ((t (:foreground "white" :italic t))))
 `(web-mode-html-tag-bracket-face  ((t (:foreground "white"))))
 `(web-mode-html-attr-name-face    ((t (:foreground "white" :bold t))))
 `(web-mode-html-entity-face       ((t (:foreground "white" :italic t))))
 `(web-mode-block-control-face     ((t (:foreground "gray60"))))
 `(web-mode-html-tag-bracket-face  ((t (:foreground "white" :bold t))))

 ;; "white"space
 `(whitespace-empty       ((t (:background "gray20"))))
 `(whitespace-space       ((t (:foreground "gray60"))))
 `(whitespace-newline     ((t (:foreground "gray60"))))
 `(whitespace-tab         ((t (:foreground "gray60"     :background "gray12"))))
 `(whitespace-indentation ((t (:foreground "IndianRed1" :background "gray60"))))
 `(whitespace-line        ((t (:foreground "IndianRed1" :background "white" :weight bold))))
 `(nobreak-space          ((t (:inherit 'default        :underline nil))))
 `(whitespace-trailing    ((t (:foreground "IndianRed1")))))

(custom-theme-set-variables
 'stimmung
 '(ansi-color-names-vector ["#1e1e1e" "#dddddd" "#dddddd" "#dddddd"
                            "#dddddd" "#dddddd" "#dddddd" "#dddddd"]))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'stimmung)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; stimmung-theme.el ends here
