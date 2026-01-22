;;; evil-case.el --- Evil operators to transform text case -*- lexical-binding: t; -*-

;;         _____         _  _          ____     _     ____   _____
;;        | ____|__   __(_)| |        / ___|   / \   / ___| | ____|
;;        |  _|  \ \ / /| || | _____ | |      / _ \  \___ \ |  _|
;;        | |___  \ V / | || ||_____|| |___  / ___ \  ___) || |___
;;        |_____|  \_/  |_||_|        \____|/_/   \_\|____/ |_____|
;;

;; Author: Yad Tahir (yad at ieee.org)
;; URL: https://github.com/yad-tahir/evil-case
;; Inspired by: https://github.com/waymondo/transform-symbol-at-point
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (s "1.12.0") (evil "1.14.0"))
;; Keywords: convenience, tools, evil, refactoring

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provides a comprehensive set of Evil operators to transform text
;; casing. Because these are defined as proper Evil operators, they can be
;; combined with any motion or text object (e.g., change a word, a specific
;; region, inside quotes, or a visual selection).
;;
;; It utilizes the string manipulation library `s.el` to handle the offered
;; transformations.
;;
;; Features:
;; - Convert between many formats: snake_case, camelCase, PascalCase, kebab-case,
;;   SCREAMING_SNAKE_CASE, Title Case, Sentence case, etc.
;; - Full Visual Support: Works correctly with standard Visual mode (`v`),
;;   Visual Line mode (`V`), and Visual Block mode (`C-v`). In these settings,
;;   the text transformation is applied to each individual line within the
;;   selection - avoiding unwanted concatenation.
;; - A smart "Cycle" operator to rotate the text through the case list.
;; - Persistence: Repeating the operator (`.`) works correctly.
;; - Easy Integration: Provides a pre-made keybinding map `evil-case-map' with
;; built-in `which-key` descriptions.
;;
;; Recommended usage:
;;   (define-key evil-normal-state-map (kbd "g c") 'evil-case-map)
;;   (define-key evil-visual-state-map (kbd "g c") 'evil-case-map)
;;
;;   Then type `g c s iw` to snake_case the inner word.

;;; Code:
(require 'evil)
(require 's)
(require 'cl-lib)

;; Silence byte-compiler and linter regarding which-key
;; We are using `fboundp' as safety.
(declare-function which-key-add-keymap-based-replacements
                  "which-key" (keymap &rest replacements))

(defgroup evil-case nil
  "Evil operators to change text case."
  :group 'evil
  :prefix "evil-case")

(defvar evil-case-cycle-sequence nil
  "The sequence of Evil case operators to cycle through.

Populated automatically by `evil-case--define-operator'.")

(defvar evil-case--last-command nil
  "The last executed evil-case operator. Used to determine the next step in the cycle.")

(defvar-local evil-case--state nil
  "Vector holding [start-marker end-marker original-string] for the current sequence.")

;; Keymap
(define-prefix-command 'evil-case-map)
(put 'evil-case-map 'variable-documentation
     "Keymap for `evil-case'.

A new case operator introduced by calling `evil-case--define-operator'
will automatically be added to this key map.")

;;;
;;; Helpers
;;;

(defun evil-case--is-valid-repeat (beg)
  "Return t if we are continuing a sequence of evil-case operations at BEG."
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
  "Execute transformation FN from BEG to END based on the Evil motion TYPE.

Handles 'line', 'block', and standard character-wise motions."
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

(defmacro evil-case--define-operator (name key desc func doc)
  "Define an evil operator NAME to execute FUNC text manipulator.

Also binds it to KEY in `evil-case-map`, adds DESC to which-key,
and appends it to `evil-case-cycle-sequence'."
  (declare (indent defun))
  `(progn
     (evil-define-operator ,name (beg end &optional type)
       ,doc
       :move-point nil
       (setq evil-case--last-command ',name)
       (evil-case--exec #',func beg end type))

     (add-to-list 'evil-case-cycle-sequence ',name t)

     (define-key evil-case-map (kbd ,key) ',name)

     (when (fboundp 'which-key-add-keymap-based-replacements)
       (which-key-add-keymap-based-replacements evil-case-map
         ,key (cons ,desc ',name)))))

;;
;; Public functions
;;

;;;###autoload
(evil-case--define-operator evil-case-camel "c" "camelCase"
  s-lower-camel-case
  "Convert text to lowerCamelCase.")

;;;###autoload
(evil-case--define-operator evil-case-pascal "C" "PascalCase"
  s-upper-camel-case
  "Convert text to PascalCase (UpperCamelCase).")

;;;###autoload
(evil-case--define-operator evil-case-snake "_" "snake_case"
  s-snake-case
  "Convert text to snake_case.")

;;;###autoload
(evil-case--define-operator evil-case-screaming-snake "S" "SCREAMING_SNAKE"
  (lambda (s) (upcase (s-snake-case s)))
  "Convert text to SCREAMING_SNAKE_CASE.")

;;;###autoload
(evil-case--define-operator evil-case-kebab "-" "kebab-case"
  s-dashed-words
  "Convert text to kebab-case.")

;;;###autoload
(evil-case--define-operator evil-case-screaming-kebab "K" "SCREAMING-KEBAB"
  (lambda (s) (upcase (s-dashed-words s)))
  "Convert text to SCREAMING-KEBAB-CASE.")

;;;###autoload
(evil-case--define-operator evil-case-sentence "s" "Sentence case"
  s-capitalized-words
  "Convert text to Sentence case.")

;;;###autoload
(evil-case--define-operator evil-case-title "t" "Title Case"
  s-titleized-words
  "Convert text to Title Case.")

;;;###autoload
(evil-case--define-operator evil-case-lower "u" "downcase"
  s-downcase
  "Convert text to downcase.")

;;;###autoload
(evil-case--define-operator evil-case-upper "U" "UPCASE"
  s-upcase
  "Convert text to UPCASE.")

;;; Cycle Operator (Manually defined as it behaves differently)
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

;; Manually add cycle binding
(define-key evil-case-map (kbd "~") 'evil-case-cycle)
(when (fboundp 'which-key-add-keymap-based-replacements)
  (which-key-add-keymap-based-replacements evil-case-map
    "~" '("cycle" . evil-case-cycle)))

(provide 'evil-case)
;;; evil-case.el ends here
