;;; evil-case.el --- Evil operators to change text case -*- lexical-binding: t; -*-

;; Author: Yad Tahir (yad at ieee.org)
;; URL: https://github.com/yad-tahir/evil-case
;; Inspired by: https://github.com/waymondo/transform-symbol-at-point
;; Version: 0.0.1
;; Package-Requires: ((emacs "24") (s "1.12.0") (evil "1.14.0"))
;; License: GNU General Public License version 3, or (at your option) any later
;; version
;; Keywords: convenience, tools

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Provides Evil operators to change text case using s.el transformations.
;;
;; Available operators:
;; * `evil-case-camel'    (lowerCamelCase)
;; * `evil-case-pascal'   (UpperCamelCase)
;; * `evil-case-snake'    (snake_case)
;; * `evil-case-kebab'    (kebab-case)
;; * `evil-case-sentence' (Sentence case)
;; * `evil-case-title'    (Title Case)
;; * `evil-case-lower'    (downcase)
;; * `evil-case-upper'    (UPCASE)

;;; Code:
(require 'evil)
(require 's)
(require 'cl-lib)

(defgroup evil-case nil
  "Evil operators to change text case."
  :group 'evil
  :prefix "evil-case")

(defcustom evil-case-cycle-sequence
  '(evil-case-upper
    evil-case-lower
    evil-case-snake
    evil-case-screaming-snake
    evil-case-kebab
    evil-case-screaming-kebab
    evil-case-camel
    evil-case-pascal
    evil-case-title
    evil-case-sentence)
  "The sequence of Evil case operators to cycle through."
  :type '(repeat function)
  :group 'evil-case)

(defvar evil-case--last-command nil
  "The last executed evil-case operator. Used to determine the next step in the cycle.")

(defvar-local evil-case--state nil
  "Vector holding [start-marker end-marker original-string] for the current sequence.")

;;;
;;; Helpers
;;;

(defun evil-case--is-valid-repeat (beg)
  "Return t if we are continuing a sequence of evil-case operations."
  (and evil-case--state
       evil-case--last-command
       ;; Make sure the marker belongs to the current buffer
       (eq (current-buffer) (marker-buffer (aref evil-case--state 0)))
       ;; Finally, ensure we are at the exact same start position
       (= beg (marker-position (aref evil-case--state 0)))))

(defun evil-case--exec-sfunc (fn beg end)
  "Apply s-func FN to region BEG/END.

If this is a repeated operation, apply FN to the ORIGINAL text found in
`evil-case--state`, not the current buffer content."
  (let* ((is-repeat (evil-case--is-valid-repeat beg))
         (original-text (if is-repeat
                            (aref evil-case--state 2)
                          (buffer-substring-no-properties beg end)))
         ;; If repeating, delete up to the marker (which handles length changes)
         (delete-end (if is-repeat
                         (marker-position (aref evil-case--state 1))
                       end)))

    (unless is-repeat
      ;; Marker Cleanup
      ;; Explicitly detach previous state's markers. If we don't, they stay in
      ;; the buffer's marker-chain and slow down edits until GC runs.
      (when evil-case--state
        (set-marker (aref evil-case--state 0) nil)
        (set-marker (aref evil-case--state 1) nil))

      (setq evil-case--state (vector (copy-marker beg)
                                     (copy-marker end t) ;; t = insertion moves marker
                                     original-text)))

    (unless (string-blank-p original-text)
      (let* ((len (length original-text))
             (prefix-end (save-match-data
                           (if (string-match "\\`[^[:alnum:]]*" original-text)
                               (match-end 0) 0)))
             (suffix-start (save-match-data
                             (if (string-match "[^[:alnum:]]*\\'" original-text)
                                 (match-beginning 0) len))))
        (when (< prefix-end suffix-start)
          (let* ((prefix (substring original-text 0 prefix-end))
                 (suffix (substring original-text suffix-start))
                 (body (substring original-text prefix-end suffix-start))
                 (modified-body (funcall fn body))
                 (final-text (concat prefix modified-body suffix)))

            (goto-char beg)
            (delete-region beg delete-end)
            (insert final-text)

            ;; Update state marker to the new end of the word
            (set-marker (aref evil-case--state 1) (point))))))))

(defun evil-case--exec (fn beg end &optional type)
  "Execute transformation FN from BEG to END with evil-operator properties."
  (evil-with-single-undo
    (let ((start-pos beg))
      (cond
       ((eq type 'line)
        (let ((end-marker (copy-marker end)))
          (goto-char beg)
          (while (< (point) end-marker)
            (let ((l-beg (line-beginning-position))
                  (l-end (line-end-position)))
              (evil-case--exec-sfunc fn l-beg l-end)
              (forward-line 1)))
          (set-marker end-marker nil)))
       ((eq type 'block)
        (evil-apply-on-block
         (lambda (b-beg b-end &rest _args)
           (evil-case--exec-sfunc fn b-beg b-end))
         beg end nil))
       ((memq type '(exclusive inclusive))
        (let* ((end (if (eq type 'exclusive) (1+ end) end))
               (end-marker (copy-marker end)))
          (goto-char beg)
          (while (< (point) end-marker)
            (let* ((limit (min (line-end-position) end-marker)))
              (evil-case--exec-sfunc fn (point) limit)
              (if (< limit end-marker)
                  (forward-line 1)
                (goto-char end-marker))))
          (set-marker end-marker nil)))
       (t
        (evil-case--exec-sfunc fn beg end)))
      (goto-char start-pos))))

(defmacro evil-case--define-operator (name func doc)
  "Define an evil operator NAME."
  `(progn
     (evil-define-operator ,name (beg end &optional type)
       ,doc
       :move-point nil
       (setq evil-case--last-command ',name)
       (evil-case--exec #',func beg end type))))

;;
;; Public symbols
;;

;;;###autoload
(evil-case--define-operator evil-case-screaming-snake
                            (lambda (s) (upcase (s-snake-case s)))
                            "Convert text to SCREAMING_SNAKE_CASE.")
;;;###autoload
(evil-case--define-operator evil-case-snake s-snake-case
                            "Convert text to snake_case.")
;;;###autoload
(evil-case--define-operator evil-case-screaming-kebab
                            (lambda (s) (upcase (s-dashed-words s)))
                            "Convert text to SCREAMING-KEBAB-CASE.")
;;;###autoload
(evil-case--define-operator evil-case-kebab s-dashed-words
                            "Convert text to kebab-case.")
;;;###autoload
(evil-case--define-operator evil-case-camel s-lower-camel-case
                            "Convert text to lowerCamelCase.")
;;;###autoload
(evil-case--define-operator evil-case-pascal s-upper-camel-case
                            "Convert text to PascalCase (UpperCamelCase).")
;;;###autoload
(evil-case--define-operator evil-case-sentence s-capitalized-words
                            "Convert text to Sentence case.")
;;;###autoload
(evil-case--define-operator evil-case-title s-titleized-words
                            "Convert text to Title Case.")
;;;###autoload
(evil-case--define-operator evil-case-lower s-downcase
                            "Convert text to downcase.")
;;;###autoload
(evil-case--define-operator evil-case-upper s-upcase
                            "Convert text to UPCASE.")

;;;###autoload
(evil-define-operator evil-case-cycle (beg end type)
  "Cycle to the next operator in `evil-case-cycle-sequence`."
  :move-point nil
  (let* ((current-cmd (or evil-case--last-command
                          (car (last evil-case-cycle-sequence))))
         (current-idx (cl-position current-cmd evil-case-cycle-sequence))
         (next-idx (if current-idx
                       (mod (1+ current-idx) (length evil-case-cycle-sequence))
                     0))
         (next-cmd (or (nth next-idx evil-case-cycle-sequence)
                       (car evil-case--last-command))))

    (setq evil-case--last-command next-cmd)

    (funcall next-cmd beg end type)
    (message "Cycled to: %s" next-cmd)))

(defvar evil-case-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "~") 'evil-case-cycle)
    (define-key map (kbd "c") 'evil-case-camel)
    (define-key map (kbd "p") 'evil-case-pascal)
    (define-key map (kbd "_") 'evil-case-snake)
    (define-key map (kbd "$") 'evil-case-screaming-snake)
    (define-key map (kbd "-") 'evil-case-kebab)
    (define-key map (kbd "#") 'evil-case-screaming-kebab)
    (define-key map (kbd "s") 'evil-case-sentence)
    (define-key map (kbd "t") 'evil-case-title)
    (define-key map (kbd "u") 'evil-case-lower)
    (define-key map (kbd "U") 'evil-case-upper)
    map)
  "Keymap for `evil-case'.")

(fset 'evil-case-map evil-case-map)

(with-eval-after-load 'which-key
  (let ((prefix-name "Evil Case Operators"))
    (which-key-add-keymap-based-replacements evil-case-map
      "~" '("cycle" . evil-case-cycle)
      "c" '("camelCase" . evil-case-camel)
      "p" '("PascalCase" . evil-case-pascal)
      "_" '("snake_case" . evil-case-snake)
      "$" '("SCREAMING_sneak" . evil-case-screaming-snake)
      "-" '("kebab-case" . evil-case-kebab)
      "#" '("SCREAMING-KEBAB" . evil-case-screaming-kebab)
      "s" '("Sentence case" . evil-case-sentence)
      "t" '("Title Case" . evil-case-title)
      "u" '("downcase" . evil-case-lower)
      "U" '("UPCASE" . evil-case-upper))))

(provide 'evil-case)
