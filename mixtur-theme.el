;;; mixtur-theme.el --- A theme tuned to inner harmonies. -*- lexical-binding: t -*-

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

(deftheme mixtur
  "A theme tuned to inner harmonies.")

(defgroup mixtur nil
  "Stimmung theme settings.

You might have to re-load the theme for these changes to take effect."
  :group 'faces
  :prefix "mixtur-")

(defcustom mixtur-highlight-color "DarkGoldenrod4"
  "The primarily color for highlights, the only non-monochrome color in code."
  :type 'string
  :group 'mixtur)

(let ((bg1 "gray80")
      (bg2 "gray75")
      (bg3 "gray70")
      (bg4 "gray60")

      (fg1 "black")
      (fg2 "black")
      (fg3 "black")
      (str "gray30")

      (attention "firebrick3")
      (ok "PaleGreen4"))
  (custom-theme-set-faces
   'mixtur

   `(default  ((t (:background ,bg1 :foreground ,fg1))))
   `(shadow   ((t (:background ,bg2))))
   `(hl-line  ((t (:background ,bg2 :extend t))))

   `(region              ((t (:background ,bg4))))
   `(lazy-highlight      ((t (:foreground ,attention :background ,bg1 :bold t))))
   `(secondary-selection ((t (:background ,bg3       :foreground ,attention :bold t))))
   `(highlight           ((t (:foreground ,fg1       :background ,bg3 :bold t))))
   `(default             ((t (:foreground ,fg1       :background ,bg1))))
   `(fringe              ((t (:foreground ,fg1       :background ,bg1))))
   `(match               ((t (:foreground ,ok        :bold t))))

   `(link                ((t (:foreground ,mixtur-highlight-color :underline t))))
   `(link-visited        ((t (:foreground ,mixtur-highlight-color :underline t :italic t))))
   `(button              ((t (:foreground ,mixtur-highlight-color :underline t))))
   `(header-line         ((t (:foreground ,mixtur-highlight-color :bold t))))
   `(tooltip             ((t (:foreground ,fg1 :background ,bg3))))
   `(vertical-border     ((t (:foreground ,bg2 :background ,bg2))))
   `(info-string         ((t (:foreground ,mixtur-highlight-color))))
   `(default-italic      ((t (:slant italic))))

   `(error                       ((t (:foreground ,attention))))
   `(warning                     ((t (:foreground ,attention))))
   `(success                     ((t (:foreground ,ok))))
   `(cancel                      ((t (:foreground ,attention :strike-through t))))
   
   `(minibuffer-noticable-prompt ((t (:foreground ,fg3       :bold t))))
   `(minibuffer-prompt           ((t (:foreground ,fg3       :bold t))))
   `(isearch                     ((t (:foreground ,attention :background ,fg1 :bold t))))
   `(isearch-highlight           ((t (:foreground ,attention :background ,fg1))))
   `(isearch-fail                ((t (:foreground ,attention :background ,bg1))))
   `(paren-matched               ((t (:foreground ,ok        :background ,bg1))))
   `(paren-unmatched             ((t (:foreground ,attention :background ,bg1))))
   `(escape-glyph                ((t (:foreground ,attention :bold t))))
   `(homoglyph                   ((t (:foreground ,attention :bold t))))
   
   `(line-number              ((t (:foreground ,fg3 :background ,bg1))))
   `(line-number-current-line ((t (:foreground ,fg1 :background ,bg1))))
   `(linum                    ((t (:inherit 'line-number))))
   
   ;; syntax
   `(font-lock-builtin-face              ((t (:foreground ,mixtur-highlight-color :italic t))))
   `(font-lock-comment-delimiter-face    ((t (:foreground ,fg2  :italic t))))
   `(font-lock-comment-face              ((t (:foreground ,fg2  :italic t))))
   `(font-lock-doc-face                  ((t (:foreground ,fg2  :italic t))))
   `(font-lock-constant-face             ((t (:foreground ,mixtur-highlight-color :bold t :italic t))))
   `(font-lock-function-name-face        ((t (:foreground ,fg1  :bold t))))
   `(font-lock-keyword-face              ((t (:foreground ,fg3  :bold t))))
   `(font-lock-type-face                 ((t (:foreground ,mixtur-highlight-color :bold t))))
   `(font-lock-variable-name-face        ((t (:foreground ,fg1  :italic t))))
   `(font-lock-negation-char-face        ((t (:foreground ,fg1  :bold t))))
   `(font-lock-preprocessor-face         ((t (:foreground ,fg1  :bold t))))
   `(font-lock-preprocessor-char-face    ((t (:foreground ,fg1  :bold t))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,fg1  :bold t))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,fg1  :bold t))))
   `(font-lock-string-face               ((t (:foreground ,str))))
   `(font-lock-warning-face              ((t (:foreground ,attention))))
   
   ;; eshell
   `(eshell-ls-directory  ((t (:foreground ,mixtur-highlight-color :bold t))))
   `(eshell-prompt        ((t (:foreground ,fg3 :bold t))))
   `(eshell-ls-executable ((t (:foreground ,bg3 :bold t))))
   `(eshell-ls-symlink    ((t (:foreground ,fg1 :italic t))))
   `(eshell-ls-special    ((t (:foreground ,ok  :italic t))))
   `(eshell-ls-backup     ((t (:foreground ,fg3 :italic t))))
   `(eshell-ls-readonly   ((t (:foreground ,attention))))
   `(eshell-ls-unreadable ((t (:foreground ,bg3)))) ; too dark?
   `(eshell-ls-missing    ((t (:foreground ,attention))))
   `(eshell-ls-product    ((t (:foreground ,fg1))))
   `(eshell-ls-archive    ((t (:foreground ,fg3))))
   `(eshell-ls-entries    ((t (:foreground ,fg1))))
   
   ;; avy
   `(avy-lead-face   ((t (:background ,bg2 :foreground ,fg1 :distant-foreground ,fg2 :bold t))))
   `(avy-lead-face-0 ((t (:inherit 'avy-lead-face))))
   `(avy-lead-face-1 ((t (:inherit 'avy-lead-face))))
   `(avy-lead-face-2 ((t (:inherit 'avy-lead-face))))
   
   ;; flyspell
   `(flyspell-incorrect       ((t (:underline (:style wave :color ,attention) ))))
   `(flyspell-duplicate       ((t (:underline (:style wave :color ,mixtur-highlight-color)))))
   `(flycheck-error           ((t (:underline (:style wave :color ,attention)))))
   `(flysheck-warning         ((t (:underline (:style wave :color ,mixtur-highlight-color)))))
   `(flysheck-warning-overlay ((t (:underline (:style wave :color ,mixtur-highlight-color)))))
   `(flycheck-note            ((t (:underline (:style wave :color ,ok)))))
   
   ;; hydra
   `(hydra-face-red      ((t (:foreground ,fg1  :bold t))))
   `(hydra-face-blue     ((t (:foreground ,fg3 :bold t))))
   `(hydra-face-amaranth ((t (:foreground ,fg3 :bold t))))
   `(hydra-face-pink     ((t (:foreground ,fg3 :bold t))))
   `(hydra-face-teal     ((t (:foreground ,fg3 :bold t))))
   
   ;; cider
   
   ;; company
   `(company-scrollbar-bg             ((t (:background ,fg3))))
   `(company-scrollbar-fg             ((t (:foreground ,fg1))))
   `(company-echo-common              ((t (:background ,fg1 :foreground ,bg1))))
   `(company-preview                  ((t (:background ,bg1 :foreground ,mixtur-highlight-color))))
   `(company-tooltip                  ((t (:background ,bg3 :foreground ,fg1))))
   `(company-tooltip-annotation       ((t (:foreground ,mixtur-highlight-color))))
   `(company-tooltip-common           ((t (:foreground ,fg3))))
   `(company-tooltip-common-selection ((t (:foreground ,mixtur-highlight-color))))
   `(company-tooltip-selection        ((t (:background ,bg3 :foreground ,fg3))))
   `(company-tooltip-selection-       ((t (:background ,bg3 :foreground ,fg3))))
   `(company-tooltip-mouse            ((t (:inherit highlight))))
   
   ;; compilation
   `(compilation-line-number    ((t (:foreground ,mixtur-highlight-color :bold t))))
   `(compilation-column-number  ((t (:inherit 'font-lock-comment-face))))
   `(compilation-error          ((t (:inherit 'error   :bold t))))
   `(compilation-warning        ((t (:inherit 'warning :italic t))))
   `(compilation-info           ((t (:inherit ,success))))
   `(compilation-mode-line-exit ((t (:inherit 'compilation-info))))
   `(compilation-mode-line-fail ((t (:inherit 'compilation-error))))
   
   ;; TODO
   ;; custom
   `(custom-variable-tag    ((t (:foreground ,mixtur-highlight-color :bold t))))

   ;; modeline
   `(header-line         ((t (:inherit 'mode-line  :distant-foreground ,bg1))))
   `(mode-line           ((t (:foreground ,fg1     :background ,bg3))))
   `(mode-line-inactive  ((t (:foreground ,fg3     :background ,bg1))))
   `(mode-line-buffer-id ((t (:foreground ,fg1     :bold t :italic t))))
   `(mode-line-emphasis  ((t (:foreground ,fg1     :bold t))))
   `(mode-line-highlight ((t (:foreground ,bg3))))
   
   ;; doom-modeline
   `(doom-modeline-buffer-path        ((t (:foreground ,fg3))))
   `(doom-modeline-buffer-file        ((t (:foreground ,fg1       :weight bold))))
   `(doom-modeline-buffer-modified    ((t (:foreground ,attention :weight bold))))
   `(doom-modeline-project-dir        ((t (:foreground ,fg1       :weight bold))))
   `(doom-modeline-project-root-dir   ((t (:foreground ,fg3       :weight normal))))
   `(doom-modeline-project-parent-dir ((t (:foreground ,fg3       :weight normal))))
   `(doom-modeline-bar-inactive       ((t (:foreground ,fg1       :background ,bg1))))
   `(doom-modeline-bar                ((t (:background ,bg1)))) ; the leftmost bar
   `(doom-modeline-evil-insert-state  ((t (:foreground ,fg1))))
   `(doom-modeline-evil-visual-state  ((t (:foreground ,fg1))))
   `(doom-modeline-evil-normal-state  ((t (:foreground ,fg3))))
   `(doom-modeline-evil-emacs-state   ((t (:foreground ,attention :italic nil))))
   
   ;; dired
   `(dired-directory  ((t (:foreground ,fg3 :bold t))))
   `(dired-ignored    ((t (:foreground ,fg2))))
   `(dired-flagged    ((t (:foreground ,ok))))
   `(dired-header     ((t (:foreground ,fg1      :bold t))))
   `(dired-mark       ((t (:foreground ,attention :bold t))))
   `(dired-marked     ((t (:foreground ,attention :bold t :italic t))))
   `(dired-perm-write ((t (:foreground ,fg1      :underline t))))
   `(dired-symlink    ((t (:foreground ,fg1      :italic t))))
   `(dired-warning    ((t (:foreground ,attention))))
   
   ;; evil
   `(evil-ex-info                   ((t (:foreground ,attention :italic t))))
   `(evil-ex-search                 ((t (:background ,bg2 :foreground ,fg1 :bold t))))
   ;; (evil-ex-sub,stitute-matches     :background base0 :foreground red   :strike-through t :weight 'bold)
   ;; (evil-ex-sub,stitute-replacement :background base0 :foreground green :weight 'bold)
   `(evil-search-highlight-persist-highlight-face ((t (:inherit 'lazy-highlight))))
   
   ;; evil-mc
   `(evil-mc-cursor-default-face ((t (:foreground ,bg1 :background ,fg1))))
   `(evil-mc-region-face         ((t (:foreground ,bg1 :background ,fg1))))
   `(evil-mc-cursor-bar-face     ((t (:foreground ,fg2))))
   `(evil-mc-cursor-hbar-face    ((t (:foreground ,fg2))))
   
   ;; info
   `(info-quoted    ((t (:foreground ,mixtur-highlight-color :inherit 'default :bold t))))
   `(info-menu-star ((t (:foreground ,fg1 :bold t))))
   ;; NOTE this should maybe have another color

   ;; ivy
   `(ivy-current-match              ((t (:foreground ,mixtur-highlight-color :background ,bg1 :bold t))))
   `(ivy-minibuffer-match-highlight ((t (:foreground ,ok))))
   `(ivy-minibuffer-match-face-1    ((t (:foreground ,fg2 :bold t :italic t))))
   `(ivy-minibuffer-match-face-2    ((t (:foreground ,fg2 :bold t :italic t))))
   `(ivy-minibuffer-match-face-3    ((t (:foreground ,fg2 :bold t :italic t))))
   `(ivy-minibuffer-match-face-4    ((t (:foreground ,fg2 :bold t :italic t))))
   `(ivy-confirm-face               ((t (:foreground ,ok))))
   `(ivy-required-face              ((t (:foreground ,attention))))
   `(ivy-subdir                     ((t (:foreground ,fg2))))
   `(ivy-modified-buffer            ((t (:foreground ,attention :bold t))))
   `(ivy-modified-outside-buffer    ((t (:foreground ,attention))))
   `(ivy-remote                     ((t (:foreground ,fg2))))
   `(ivy-virtual                    ((t (:foreground ,fg2 :italic t))))
   `(ivy-prompt                     ((t (:foreground ,attention))))
   `(ivy-prompt-match               ((t (:foreground ,attention))))
   `(ivy-separator                  ((t (:foreground ,mixtur-highlight-color))))
   `(ivy-highlight-face             ((t (:foreground ,attention))))
   `(ivy-grep-info                  ((t (:foreground ,attention))))
   `(ivy-completions-annotations    ((t (:foreground ,attention))))
   
   ;; TODO magit
   `(magit-bisect-bad        ((t (:foreground ,attention))))
   `(magit-bisect-good       ((t (:foreground ,ok))))
   `(magit-bisect-skip       ((t (:foreground ,fg2))))
   `(magit-blame-date        ((t (:foreground ,attention))))
   `(magit-branch            ((t (:foreground ,mixtur-highlight-color :bold t))))
   
   `(magit-diff-context-highlight ((t (:foreground ,fg3 :background ,bg3))))
   `(magit-diff-file-header       ((t (:foreground ,fg3 :background ,bg3))))
   `(magit-diffstat-added         ((t (:foreground ,ok))))
   `(magit-diffstat-removed       ((t (:foreground ,attention))))
   `(magit-dimmed                 ((t (:foreground ,fg2))))
   `(magit-hash                   ((t (:foreground ,fg3))))
   `(magit-hunk-heading           ((t (:background ,bg3))))
   `(magit-hunk-heading-highlight ((t (:background ,bg3))))
   `(magit-item-highlight         ((t (:background ,bg3))))
   `(magit-log-author             ((t (:foreground ,fg3))))
   `(magit-process-ng             ((t (:foreground ,mixtur-highlight-color :bold t))))
   `(magit-process-ok             ((t (:foreground ,ok :bold t))))
   `(magit-section-heading        ((t (:foreground ,fg3 :bold t))))
   `(magit-section-highlight      ((t (:background ,bg3))))
   
   ;; diff-hl
   `(diff-hl-insert         ((t (:foreground ,ok :background ,bg1 :bold nil :italic nil))))
   `(diff-hl-delete         ((t (:foreground ,attention :background ,bg1 :bold nil :italic nil))))
   `(diff-hl-change         ((t (:foreground ,fg3       :background ,bg1 :bold nil :italic nil))))
   `(diff-hl-ignore         ((t (:foreground ,fg2       :background ,bg1 :bold nil :italic nil))))
   `(diff-hl-margin-ignore  ((t (:foreground ,fg2       :background ,bg1 :bold nil :italic nil))))
   `(diff-hl-margin-unknown ((t (:foreground ,fg2       :background ,bg1 :bold nil :italic nil))))
   
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
   `(org-code                  ((t (:foreground ,mixtur-highlight-color :distant-foreground ,bg1 :background ,mixtur-highlight-color))))
   `(org-date                  ((t (:foreground ,mixtur-highlight-color :bold t))))
   `(org-link                  ((t (:foreground ,mixtur-highlight-color :bold t :underline t)))) ;; BUG foreground not respected
   `(org-block                 ((t (:foreground ,fg1 :background ,bg3 :extend t))))
   `(org-block-begin-line      ((t (:foreground ,fg3 :background ,bg3 :bold t :extend t)))) ; could be a better fg
   `(org-block-end-line        ((t (:foreground ,fg3 :background ,bg3 :bold t :extend t))))
   `(org-drawer                ((t (:foreground ,fg3 :bold t))))
   `(org-document-info         ((t (:foreground ,fg1 :background ,bg1 :italic t)))) ;; BUG does not seem to correctly color fg
   `(org-document-info-keyword ((t (:foreground ,fg2 :background ,bg1))))
   `(org-document-title        ((t (:foreground ,fg1 :weight bold))))
   `(org-done                  ((t (:foreground ,ok  :bold t :strike-through t))))
   `(org-ellipsis              ((t (:foreground ,fg2))))
   `(org-footnote              ((t (:foreground ,mixtur-highlight-color))))
   `(org-formula               ((t (:foreground ,fg2))))
   `(org-headline-done         ((t (:foreground ,fg2 :weight normal :strike-through t))))
   `(org-hide                  ((t (:foreground ,bg1 :background ,bg1))))
   `(org-list-dt               ((t (:foreground ,fg2 :bold t))))
   `(org-priority              ((t (:foreground ,mixtur-highlight-color))))
   `(org-scheduled             ((t (:foreground ,attention))))
   `(org-scheduled-previously  ((t (:foreground ,mixtur-highlight-color))))
   `(org-scheduled-today       ((t (:foreground ,ok))))
   `(org-sexp-date             ((t (:foreground ,mixtur-highlight-color))))
   `(org-special-keyword       ((t (:foreground ,mixtur-highlight-color))))
   `(org-table                 ((t (:foreground ,fg2))))
   `(org-tag                   ((t (:foreground ,fg2       :background ,bg1 :bold t))))
   `(org-todo                  ((t (:foreground ,attention   :bold t))))
   `(org-warning               ((t (:foreground ,attention   :bold t))))
   `(org-upcoming-deadline     ((t (:foreground ,attention))))
   
   ;; markdown mode
   `(markdown-header-face             ((t (:foreground ,fg1  :bold t))))
   `(markdown-list-face               ((t (:foreground ,fg3 :bold t))))
   `(markdown-bold-face               ((t (:foreground ,fg1  :bold t))))
   `(markdown-blockquote-face         ((t (:foreground ,fg2 :italic t))))
   `(markdown-italic-face             ((t (:foreground ,fg1  :italic t))))
   `(markdown-link-face               ((t (:foreground ,fg2 :underline t))))
   `(markdown-url-face                ((t (:foreground ,fg2 :underline t))))
   `(markdown-header-delimiter-face   ((t (:inherit 'markdown-header-face))))
   `(markdown-metadata-key-face       ((t (:foreground ,fg2))))
   `(markdown-markup-face             ((t (:foreground ,fg1))))
   `(markdown-pre-face                ((t (:foreground ,fg1))))
   `(markdown-code-face               ((t (:background ,fg3 :extend t))))
   `(markdown-reference-face          ((t (:foreground ,fg2))))
   `(markdown-html-attr-name-face     ((t (:inherit 'font-lock-variable-name-face))))
   `(markdown-html-attr-value-face    ((t (:inherit 'font-lock-string-face))))
   `(markdown-html-entity-face        ((t (:inherit 'font-lock-variable-name-face))))
   `(markdown-html-tag-delimiter-face ((t (:inherit 'markdown-markup-face))))
   `(markdown-html-tag-name-face      ((t (:inherit 'font-lock-keyword-face))))
   `(markdown-inline-code-face        ((t (:inherit 'markdown-code-face :extend nil))))
   
   ;; show-paren
   `(show-paren-match-face       ((t (:background ,bg1       :foreground "red" :bold t :italic t))))
   `(show-paren-match            ((t (:background ,bg1       :foreground "red" :bold t :italic t))))
   `(show-paren-match-expression ((t (:background ,bg1       :foreground "red" :bold t :italic t))))
   `(show-paren-mismatch         ((t (:background ,attention :foreground ,bg1 :bold t :italic t))))
   
   ;; smartparens
   `(sp-show-pair-match-face    ((t (:inherit 'paren-matched))))
   `(sp-show-pair-mismatch-face ((t (:inherit 'paren-unmatched))))
   
   ;; LaTeX
   `(font-latex-sectioning-0-face ((t (:foreground ,mixtur-highlight-color :bold t))))
   `(font-latex-sectioning-1-face ((t (:foreground ,mixtur-highlight-color :bold t))))
   `(font-latex-sectioning-2-face ((t (:foreground ,mixtur-highlight-color :bold t))))
   `(font-latex-sectioning-3-face ((t (:foreground ,mixtur-highlight-color :bold t :italic t))))
   `(font-latex-sectioning-4-face ((t (:foreground ,mixtur-highlight-color :italic t))))
   `(font-latex-italic-face       ((t (:foreground ,fg1   :italic t))))
   `(font-latex-bold-face         ((t (:foreground ,fg1   :bold t))))
   `(font-latex-verbatim-face     ((t (:foreground ,mixtur-highlight-color :bold t))))
   `(font-latex-string-face       ((t (:foreground ,fg3))))
   `(font-latex-warning-face      ((t (:foreground ,attention))))
   `(font-latex-math-face         ((t (:foreground ,fg3))))
   `(font-latex-script-char-face  ((t (:foreground ,mixtur-highlight-color))))

   ;; re-builder
   `(reb-match-0 ((t (:foreground ,fg1 :inverse-video t :bold t))))
   `(reb-match-1 ((t (:foreground ,fg1 :inverse-video t :bold t))))
   `(reb-match-2 ((t (:foreground ,fg1 :inverse-video t :bold t))))
   `(reb-match-3 ((t (:foreground ,fg1 :inverse-video t :bold t))))
   
   ;; undo-tree
   `(undo-tree-visualizer-default-face       ((t (:foreground ,fg1))))
   `(undo-tree-visualizer-current-face       ((t (:foreground ,ok :bold t))))
   `(undo-tree-visualizer-unmodified-face    ((t (:foreground ,fg3 :italic t))))
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,fg1))))
   `(undo-tree-visualizer-register-face      ((t (:foreground ,fg1))))

   `(window-divider             ((t (:foreground ,bg1))))
   `(window-divider-first-pixel ((t (:foreground ,bg1))))
   `(window-divider-last-pixel  ((t (:foreground ,bg1))))
   
   ;; wo/man
   `(Man-overstrike ((t (:foreground ,fg2 :bold t))))
   `(Man-underline  ((t (:foreground ,fg2 :underline nil :italic t))))
   `(woman-bold     ((t (:inherit 'Man-overstrike))))
   `(woman-italic   ((t (:inherit 'Man-underline))))
   
   ;; web-mode
   `(web-mode-doctype-face           ((t (:foreground ,fg2))))
   `(web-mode-html-tag-face          ((t (:foreground ,fg1 :italic t))))
   `(web-mode-html-tag-bracket-face  ((t (:foreground ,fg1))))
   `(web-mode-html-attr-name-face    ((t (:foreground ,fg1 :bold t))))
   `(web-mode-html-entity-face       ((t (:foreground ,fg1 :italic t))))
   `(web-mode-block-control-face     ((t (:foreground ,fg3))))
   `(web-mode-html-tag-bracket-face  ((t (:foreground ,fg1 :bold t))))

   ;; visual-regexp
   `(vr/match-0 ((t (:background ,bg3 :foreground ,attention :bold t))))
   `(vr/match-1 ((t (:background ,bg2 :foreground ,attention))))
   `(vr/group-0 ((t (:background ,bg3 :foreground ,attention :bold t))))
   `(vr/group-1 ((t (:background ,bg2 :foreground ,attention))))
   `(vr/group-2 ((t (:background ,bg2 :foreground ,fg2))))

   ;; white-space
   `(whitespace-empty       ((t (:background ,bg3))))
   `(whitespace-space       ((t (:foreground ,fg3))))
   `(whitespace-newline     ((t (:foreground ,fg3))))
   `(whitespace-tab         ((t (:foreground ,fg3       :background ,bg1))))
   `(whitespace-indentation ((t (:foreground ,attention :background ,fg3))))
   `(whitespace-line        ((t (:foreground ,attention :background ,fg1 :weight bold))))
   `(nobreak-space          ((t (:inherit 'default      :underline nil))))
   `(whitespace-trailing    ((t (:foreground ,attention))))))

(custom-theme-set-variables
 'mixtur
 '(ansi-color-names-vector ["#1e1e1e" "#dddddd" "#dddddd" "#dddddd"
                            "#dddddd" "#dddddd" "#dddddd" "#dddddd"]))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'mixtur)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; mixtur-theme.el ends here
