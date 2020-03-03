;;; stimmung.el --- a theme tuned to inner harmonies. -*- lexical-binding: t -*-

;; Copyright © 2019

;; Author: Love Lagerkvist
;; URL: https://github.com/motform/stimmung
;; Package-Requires: ((emacs "24"))
;; Created: 2019-12-20
;; Keywords: color theme

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The idea behind this theme is to decrease fruit salad factor,
;; emphasize comments and mesh well with the colors used by MacOS dark mode.
;; Stimmung makes heavy use of typographic features to distinguish syntactic elements
;; instead of with colors.  Thus it requires a font with bold, italic and bold italic.
;;
;; The macros used to generate the theme is forked from Dracula.el:
;; https://github.com/dracula/emacs

;;; Code:

(require 'cl-lib)

(deftheme stimmung
  "A theme tuned to inner harmonies.")

(let ((colors '((s-bg      "#1e1e1e") ; background, a dark gray
                (s-fg      "#ffffff") ; foreground, very white
                (s-hline   "#2a2a2a")
                (s-comment "#a5a5a5") ; should the comments be more pronounced?

                (s-lgray   "#929296") ; contrasting foregrounds
                (s-gray    "#2e2e2e") ; contrasting backgrounds
                (s-beige   "#e0d2b9") ; kind of overpowers everything else, should it be lighter?
                (s-red     "#d19d95")
                (s-green   "#c9d195")

                (s-wtf     "#ff79c6")))

      (faces '((cursor              :background ,s-fg)
               (shadow              :background ,s-hline)
               (hl-line             :background ,s-hline :extend t)
               (secondary-selection :background ,s-gray  :foreground ,s-red  :bold t)
               (region              :background ,s-hline :bold t)
               (highlight           :foreground ,s-fg    :background ,s-gray :bold t)
               (lazy-highlight      :foreground ,s-lgray :background ,s-bg   :bold t)
               (default             :foreground ,s-fg    :background ,s-bg)
               (match               :foreground ,s-green :bold t)
               (fringe              :foreground ,s-fg    :background ,s-bg)
               (link                :foreground ,s-beige :underline t)
               (link-visited        :foreground ,s-beige :underline t :italic t)
               (button              :foreground ,s-beige :underline t)
               (header-line         :foreground ,s-beige :bold t)
               (tooltip             :foreground ,s-fg    :background ,s-gray)
               (vertical-border     :foreground ,s-hline :background ,s-hline)
               (info-string         :foreground ,s-beige)
               (default-italic      :slant italic)

               (error                       :foreground ,s-red)
               (warning                     :foreground ,s-red)
               (success                     :foreground ,s-green)
               (cancel                      :foreground ,s-red :strike-through t)

               (minibuffer-noticable-prompt :foreground ,s-lgray :bold t)
               (minibuffer-prompt           :foreground ,s-lgray :bold t)
               (isearch                     :foreground ,s-fg    :background ,s-hline)
               (isearch-highlight           :foreground ,s-fg    :background ,s-wtf)
               (isearch-fail                :foreground ,s-red   :background ,s-bg)
               (paren-matched               :foreground ,s-green :background ,s-bg)
               (paren-unmatched             :foreground ,s-red   :background ,s-bg)
               (escape-glyph                :foreground ,s-red   :bold t)
               (homoglyph                   :foreground ,s-red   :bold t)

               (line-number              :foreground ,s-lgray :background ,s-bg)
               (line-number-current-line :foreground ,s-fg    :background ,s-bg)
               (linum                    :inherit 'line-number)

               ;; syntax
               (font-lock-builtin-face              :foreground ,s-beige   :italic t)
               (font-lock-comment-delimiter-face    :foreground ,s-comment :italic t)
               (font-lock-comment-face              :foreground ,s-comment :italic t)
               (font-lock-doc-face                  :foreground ,s-comment :italic t)
               (font-lock-constant-face             :foreground ,s-beige   :bold t :italic t)
               (font-lock-function-name-face        :foreground ,s-fg      :bold t)
               (font-lock-keyword-face              :foreground ,s-lgray   :bold t)
               (font-lock-type-face                 :foreground ,s-beige   :bold t)
               (font-lock-variable-name-face        :foreground ,s-fg      :italic t)
               (font-lock-string-face               :foreground ,s-lgray)
               (font-lock-warning-face              :foreground ,s-red)
               (font-lock-negation-char-face        :foreground ,s-fg :bold t)
               (font-lock-preprocessor-face         :foreground ,s-fg :bold t)
               (font-lock-preprocessor-char-face    :foreground ,s-fg :bold t)
               (font-lock-regexp-grouping-backslash :foreground ,s-fg :bold t)
               (font-lock-regexp-grouping-construct :foreground ,s-fg :bold t)

               ;; eshell
               (eshell-prompt        :foreground ,s-lgray :bold t)
               (eshell-ls-directory  :foreground ,s-beige :bold t)
               (eshell-ls-executable :foreground ,s-gray  :bold t)
               (eshell-ls-symlink    :foreground ,s-fg    :italic t)
               (eshell-ls-readonly   :foreground ,s-red)
               (eshell-ls-unreadable :foreground ,s-gray) ; too dark?
               (eshell-ls-special    :foreground ,s-green :italic t)
               (eshell-ls-missing    :foreground ,s-red)
               (eshell-ls-product    :foreground ,s-fg)
               (eshell-ls-archive    :foreground ,s-lgray)
               (eshell-ls-entries    :foreground ,s-fg)
               (eshell-ls-backup     :foreground ,s-lgray :italic t)

               ;; avy
               (avy-lead-face   :background ,s-hline :foreground ,s-fg :distant-foreground ,s-comment :bold t)
               (avy-lead-face-0 :inherit 'avy-lead-face)
               (avy-lead-face-1 :inherit 'avy-lead-face)
               (avy-lead-face-2 :inherit 'avy-lead-face)

               ;; flyspell
               ;; TODO make these lines wavy!
               (flyspell-incorrect       :underline ,s-red)
               (flyspell-duplicate       :underline ,s-beige)
               (flycheck-error           :underline ,s-red)
               (flysheck-warning         :underline ,s-beige)
               (flysheck-warning-overlay :underline ,s-beige)
               (flycheck-note            :underline ,s-green)

               ;; hydra
               (hydra-face-red      :foreground ,s-fg    :bold t)
               (hydra-face-blue     :foreground ,s-lgray :bold t)
               (hydra-face-amaranth :foreground ,s-lgray :bold t)
               (hydra-face-pink     :foreground ,s-lgray :bold t)
               (hydra-face-teal     :foreground ,s-lgray :bold t)

               ;; cider

               ;; company
               (company-scrollbar-bg             :background ,s-lgray)
               (company-scrollbar-fg             :foreground ,s-fg)
               (company-echo-common              :background ,s-fg   :foreground ,s-bg)
               (company-preview                  :background ,s-bg   :foreground ,s-beige)
               (company-tooltip                  :background ,s-gray :foreground ,s-fg)
               (company-tooltip-annotation       :foreground ,s-beige)
               (company-tooltip-common           :foreground ,s-lgray)
               (company-tooltip-common-selection :foreground ,s-beige)
               (company-tooltip-selection        :background ,s-gray :foreground ,s-lgray)
               (company-tooltip-selection-       :background ,s-gray :foreground ,s-lgray)
               (company-tooltip-mouse            :inherit highlight)

               ;; compilation
               (compilation-line-number    :foreground ,s-beige :bold t)
               (compilation-column-number  :inherit 'font-lock-comment-face)
               (compilation-error          :inherit 'error   :bold t)
               (compilation-warning        :inherit 'warning :italic t)
               (compilation-info           :inherit 'success)
               (compilation-mode-line-exit :inherit 'compilation-info)
               (compilation-mode-line-fail :inherit 'compilation-error)

               ;; modeline
               (header-line         :inherit 'mode-line  :distant-foreground ,s-bg)
               (mode-line           :foreground ,s-fg    :background ,s-bg)
               (mode-line-inactive  :foreground ,s-lgray :background ,s-bg)
               (mode-line-buffer-id :foreground ,s-fg    :bold t :italic t)
               (mode-line-emphasis  :foreground ,s-fg    :bold t)
               (mode-line-highlight :foreground ,s-beige)

               ;; TODO custom

               ;; doom-modeline
               (doom-modeline-buffer-path        :foreground ,s-lgray)
               (doom-modeline-buffer-file        :foreground ,s-fg    :weight bold)
               (doom-modeline-buffer-modified    :foreground ,s-red   :weight bold)
               (doom-modeline-project-dir        :foreground ,s-fg    :weight bold)
               (doom-modeline-project-root-dir   :foreground ,s-lgray :weight normal)
               (doom-modeline-project-parent-dir :foreground ,s-lgray :weight normal)
               (doom-modeline-bar-inactive       :foreground ,s-fg    :background ,s-bg)
               (doom-modeline-bar                :background ,s-bg) ; the leftmost bar
               (doom-modeline-evil-insert-state  :foreground ,s-fg)
               (doom-modeline-evil-visual-state  :foreground ,s-fg)
               (doom-modeline-evil-normal-state  :foreground ,s-lgray)
               (doom-modeline-evil-emacs-state   :foreground ,s-red :italic nil)

               ;; dired
               (dired-directory  :foreground ,s-lgray :bold t)
               (dired-ignored    :foreground ,s-comment)
               (dired-flagged    :foreground ,s-green)
               (dired-header     :foreground ,s-fg  :bold t)
               (dired-mark       :foreground ,s-red :bold t)
               (dired-marked     :foreground ,s-red :bold t :italic t)
               (dired-perm-write :foreground ,s-fg  :underline t)
               (dired-symlink    :foreground ,s-fg  :italic t)
               (dired-warning    :foreground ,s-red)

               ;; evil
               (evil-ex-info                   :foreground ,s-red :italic t)
               (evil-ex-search                 :background ,s-hline :foreground ,s-fg :bold t)
               ;; (evil-ex-substitute-matches     :background base0 :foreground red   :strike-through t :weight 'bold)
               ;; (evil-ex-substitute-replacement :background base0 :foreground green :weight 'bold)
               (evil-search-highlight-persist-highlight-face :inherit 'lazy-highlight)

               ;; info
               (Info-quoted    :foreground ,s-beige :inherit 'default :bold t)
               (info-menu-star :foreground ,s-fg :bold t) ;; NOTE this should maybe have another color
               ;; link-styling is inherited from ~link~

               ;; ivy
               (ivy-current-match              :foreground ,s-beige :background ,s-bg :bold t)
               (ivy-minibuffer-match-highlight :foreground ,s-green)
               (ivy-minibuffer-match-face-1    :foreground ,s-comment :bold t :italic t)
               (ivy-minibuffer-match-face-2    :foreground ,s-comment :bold t :italic t)
               (ivy-minibuffer-match-face-3    :foreground ,s-comment :bold t :italic t)
               (ivy-minibuffer-match-face-4    :foreground ,s-comment :bold t :italic t)
               (ivy-confirm-face               :foreground ,s-green)
               (ivy-required-face              :foreground ,s-red)
               (ivy-subdir                     :foreground ,s-comment)
               (ivy-modified-buffer            :foreground ,s-red :bold t)
               (ivy-modified-outside-buffer    :foreground ,s-red)
               (ivy-remote                     :foreground ,s-comment)
               (ivy-virtual                    :foreground ,s-comment :italic t)
               (ivy-prompt                     :foreground ,s-red)
               (ivy-prompt-match               :foreground ,s-red)
               (ivy-separator                  :foreground ,s-beige)
               (ivy-highlight-face             :foreground ,s-red)
               (ivy-grep-info                  :foreground ,s-red)
               (ivy-completions-annotations    :foreground ,s-red)

               ;; TODO magit
               (magit-branch                 :foreground ,s-beige :bold t)
               (magit-diff-context-highlight :foreground ,s-lgray :background ,s-gray)
               (magit-diff-file-header       :foreground ,s-lgray :background ,s-gray)
               (magit-diffstat-added         :foreground ,s-green)
               (magit-diffstat-removed       :foreground ,s-red)
               (magit-hash                   :foreground ,s-lgray)
               (magit-hunk-heading           :background ,s-gray)
               (magit-hunk-heading-highlight :background ,s-gray)
               (magit-item-highlight         :background ,s-gray)
               (magit-log-author             :foreground ,s-lgray)
               (magit-process-ng             :foreground ,s-beige :bold t)
               (magit-process-ok             :foreground ,s-green :bold t)
               (magit-section-heading        :foreground ,s-lgray :bold t)
               (magit-section-highlight      :background ,s-gray)

               ;; diff-hl
               (diff-hl-insert         :foreground ,s-green   :background ,s-bg)
               (diff-hl-delete         :foreground ,s-red     :background ,s-bg)
               (diff-hl-change         :foreground ,s-beige   :background ,s-bg)
               (diff-hl-ignore         :foreground ,s-comment :background ,s-bg)
               (diff-hl-margin-ignore  :foreground ,s-comment :background ,s-bg)
               (diff-hl-margin-unknown :foreground ,s-comment :background ,s-bg)

               ;; outline, extends org-outline
               (outline-1 :foreground ,s-fg :bold t :extend t)
               (outline-2 :foreground ,s-fg :bold t :extend t)
               (outline-3 :foreground ,s-fg :bold t :extend t)
               (outline-4 :foreground ,s-fg :bold t :extend t)
               (outline-5 :foreground ,s-fg :bold t :extend t)
               (outline-6 :foreground ,s-fg :bold t :extend t)
               (outline-7 :foreground ,s-fg :bold t :extend t)
               (outline-8 :foreground ,s-fg :bold t :extend t)

               ;; org-agenda
               ;; (org-agenda-done :inherit 'org-done)
               ;; (org-agenda-dimmed-todo-face :foreground comments)
               ;; (org-agenda-date          :foreground violet :weight 'ultra-bold)
               ;; (org-agenda-date-today    :foreground (doom-lighten violet 0.4)   :weight 'ultra-bold)
               ;; (org-agenda-date-weekend  :foreground (doom-darken violet 0.4)  :weight 'ultra-bold)
               ;; (org-agenda-structure     :foreground fg :weight 'ultra-bold)
               ;; (org-agenda-clocking      :background (doom-blend blue bg 0.2))
               ;; (org-upcoming-deadline         :foreground (doom-blend fg bg 0.8))
               ;; (org-upcoming-distant-deadline :foreground (doom-blend fg bg 0.5))
               ;; (org-scheduled            :foreground fg)
               ;; (org-scheduled-today      :foreground base7)
               ;; (org-scheduled-previously :foreground base8)
               ;; (org-time-grid            :foreground comments)
               ;; (org-sexp-date            :foreground fg)

               ;; org
               (org-code                  :foreground ,s-beige :distant-foreground ,s-bg :background ,s-beige)
               (org-block                 :foreground ,s-fg    :background ,s-gray)
               (org-block-begin-line      :foreground ,s-lgray :background ,s-gray :bold t) ; could be a better fg
               (org-block-end-line        :foreground ,s-lgray :background ,s-gray :bold t)
               (org-date                  :foreground ,s-beige   :bold t)
               (org-drawer                :foreground ,s-lgray   :bold t)
               (org-document-info         :foreground ,s-fg      :background ,s-bg :italic t) ;; BUG  does not seem to correctly color fg
               (org-document-info-keyword :foreground ,s-comment :background ,s-bg)
               (org-document-title        :foreground ,s-fg      :weight bold)
               (org-done                  :foreground ,s-green)
               (org-ellipsis              :foreground ,s-comment)
               (org-footnote              :foreground ,s-beige)
               (org-formula               :foreground ,s-comment)
               (org-headline-done         :foreground ,s-comment :weight normal :strike-through t)
               (org-hide                  :foreground ,s-bg :background ,s-bg)
               (org-link                  :foreground ,s-comment :bold t :underline t) ;; BUG  foreground not respected
               (org-list-dt               :foreground ,s-comment :bold t)
               (org-priority              :foreground ,s-beige)
               (org-scheduled             :foreground ,s-red)
               (org-scheduled-previously  :foreground ,s-beige)
               (org-scheduled-today       :foreground ,s-green)
               (org-sexp-date             :foreground ,s-beige)
               (org-special-keyword       :foreground ,s-beige)
               (org-table                 :foreground ,s-comment)
               (org-tag                   :foreground ,s-comment :background ,s-bg :bold t)
               (org-todo                  :foreground ,s-red :bold t)
               (org-warning               :foreground ,s-red :bold t)
               (org-upcoming-deadline     :foreground ,s-red)

               ;; markdown mode
               ;; TODO parity with doom-themes
               (markdown-header-face             :foreground ,s-fg      :bold t)
               (markdown-list-face               :foreground ,s-lgray   :bold t)
               (markdown-bold-face               :foreground ,s-fg      :bold t)
               (markdown-blockquote-face         :foreground ,s-comment :italic t)
               (markdown-italic-face             :foreground ,s-fg      :italic t)
               (markdown-link-face               :foreground ,s-comment :underline t)
               (markdown-url-face                :foreground ,s-comment :underline t)
               (markdown-header-delimiter-face   :inherit 'markdown-header-face)
               (markdown-metadata-key-face       :foreground ,s-comment)
               (markdown-markup-face             :foreground ,s-fg)
               (markdown-pre-face                :foreground ,s-fg)
               (markdown-code-face               :background ,s-lgray :extend t)
               (markdown-reference-face          :foreground ,s-comment)
               (markdown-html-attr-name-face     :inherit 'font-lock-variable-name-face)
               (markdown-html-attr-value-face    :inherit 'font-lock-string-face)
               (markdown-html-entity-face        :inherit 'font-lock-variable-name-face)
               (markdown-html-tag-delimiter-face :inherit 'markdown-markup-face)
               (markdown-html-tag-name-face      :inherit 'font-lock-keyword-face)
               (markdown-inline-code-face        :inherit 'markdown-code-face :extend nil)

               ;; show-paren
               ;; NOTE the green/red is a bit faint right now, but the italic sort of balances that, not sure
               (show-paren-match-face       :background ,s-bg :foreground ,s-red :bold t :italic t)
               (show-paren-match            :background ,s-bg :foreground ,s-red :bold t :italic t)
               (show-paren-match-expression :background ,s-bg :foreground ,s-red :bold t :italic t)
               (show-paren-mismatch         :background ,s-red :foreground ,s-bg :bold t :italic t)

               ;; smartparens
               (sp-show-pair-match-face    :inherit 'paren-matched)
               (sp-show-pair-mismatch-face :inherit 'paren-unmatched)

               ;; re-builder
               (reb-match-0 :foreground ,s-fg :inverse-video t :bold t)
               (reb-match-1 :foreground ,s-fg :inverse-video t :bold t)
               (reb-match-2 :foreground ,s-fg :inverse-video t :bold t)
               (reb-match-3 :foreground ,s-fg :inverse-video t :bold t)

               ;; undo-tree
               (undo-tree-visualizer-default-face       :foreground ,s-fg)
               (undo-tree-visualizer-current-face       :foreground ,s-green :bold t)
               (undo-tree-visualizer-unmodified-face    :foreground ,s-lgray :italic t)
               (undo-tree-visualizer-active-branch-face :foreground ,s-fg)
               (undo-tree-visualizer-register-face      :foreground ,s-fg)

               ;; wo/man
               (Man-overstrike :foreground ,s-comment :bold t)
               (Man-underline  :foreground ,s-comment :underline nil :italic t)
               (woman-bold     :inherit 'Man-overstrike)
               (woman-italic   :inherit 'Man-underline)

               ;; TODO prism

               ;; web-mode
               (web-mode-doctype-face           :foreground ,s-comment)
               (web-mode-html-tag-face          :foreground ,s-fg :italic t)
               (web-mode-html-tag-bracket-face  :foreground ,s-fg)
               (web-mode-html-attr-name-face    :foreground ,s-fg :bold t)
               (web-mode-html-entity-face       :foreground ,s-fg :italic t)
               (web-mode-block-control-face     :foreground ,s-lgray)
               (web-mode-html-tag-bracket-face  :foreground ,s-fg :bold t)

               (whitespace-empty       :background ,s-gray)
               (trailing-whitespace    :foreground ,s-red)
               (whitespace-space       :foreground ,s-lgray)
               (whitespace-newline     :foreground ,s-lgray)
               (whitespace-tab         :foreground ,s-lgray :background ,s-bg)
               (whitespace-indentation :foreground ,s-red   :background ,s-lgray)
               (whitespace-line        :foreground ,s-red   :background ,s-fg :weight bold)
               (nobreak-space          :inherit 'default    :underline nil)
               (whitespace-trailing    :inherit 'trailing-whitespace))))

  (apply #'custom-theme-set-faces
         'stimmung
         (let ((color-names (mapcar #'car colors))
               (graphic-colors (mapcar #'cadr colors))
               (tty-colors (mapcar #'car (mapcar #'last colors))))
           (cl-flet* ((expand-for-tty (spec) (cl-progv color-names tty-colors
                                               (eval `(backquote ,spec))))
                      (expand-for-graphic (spec) (cl-progv color-names graphic-colors
                                                   (eval `(backquote ,spec)))))
             (cl-loop for (face . spec) in faces
                      collect `(,face
                                ((((min-colors 16777216))
                                  ,(expand-for-graphic spec))
                                 (t
                                  ,(expand-for-tty spec)))))))))

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
