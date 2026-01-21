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

(defun evil-case--exec-sfunc (fn beg end)
  "Apply s-func FN to region BEG/END, preserving leading/trailing delimiters."
  (let ((original-text (buffer-substring-no-properties beg end)))
    ;; Check if the text is purely whitespace/empty; skip then!
    (unless (string-blank-p original-text)
      (let* ((len (length original-text))
             ;; Identify the end of the leading non-alphanumeric chars
             (prefix-end (save-match-data
                           (if (string-match "\\`[^[:alnum:]]*" original-text)
                               (match-end 0)
                             0)))
             ;; Identify the start of the trailing non-alphanumeric chars
             (suffix-start (save-match-data
                             (if (string-match "[^[:alnum:]]*\\'" original-text)
                                 (match-beginning 0)
                               len))))
        ;; Safety check: if prefix overlaps suffix (e.g. text is just "---")
        ;; we do nothing, as there is no alphanumeric 'body' to transform.
        (when (< prefix-end suffix-start)
          (let* ((prefix (substring original-text 0 prefix-end))
                 (suffix (substring original-text suffix-start))
                 (body (substring original-text prefix-end suffix-start))
                 ;; Apply the transformation ONLY to the core body
                 (modified-body (funcall fn body))
                 (final-text (concat prefix modified-body suffix)))
            (goto-char beg)
            (delete-region beg end)
            (insert final-text)))))))

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
        ;; Treat exclusive motion as inclusive for better behavior with
        ;; motions like 'evil-forward-word-begin'
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
  "Define an evil operator NAME that applies s-func FUNC."
  `(evil-define-operator ,name (beg end &optional type)
     ,doc
     :move-point nil
     (evil-case--exec #',func beg end type)))

;;;###autoload (autoload 'evil-case-snake "evil-case" nil t)
(evil-case--define-operator evil-case-snake s-snake-case
                            "Convert text to snake_case.")

;;;###autoload (autoload 'evil-case-kebab "evil-case" nil t)
(evil-case--define-operator evil-case-kebab s-dashed-words
                            "Convert text to kebab-case.")

;;;###autoload (autoload 'evil-case-camel "evil-case" nil t)
(evil-case--define-operator evil-case-camel s-lower-camel-case
                            "Convert text to lowerCamelCase.")

;;;###autoload (autoload 'evil-case-pascal "evil-case" nil t)
(evil-case--define-operator evil-case-pascal s-upper-camel-case
                            "Convert text to PascalCase (UpperCamelCase).")

;;;###autoload (autoload 'evil-case-sentence "evil-case" nil t)
(evil-case--define-operator evil-case-sentence s-capitalized-words
                            "Convert text to Sentence case.")

;;;###autoload (autoload 'evil-case-title "evil-case" nil t)
(evil-case--define-operator evil-case-title s-titleized-words
                            "Convert text to Title Case.")

;;;###autoload (autoload 'evil-case-lower "evil-case" nil t)
(evil-case--define-operator evil-case-lower s-downcase
                            "Convert text to downcase.")

;;;###autoload (autoload 'evil-case-upper "evil-case" nil t)
(evil-case--define-operator evil-case-upper s-upcase
                            "Convert text to UPCASE.")

(defvar evil-case-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") 'evil-case-camel)
    (define-key map (kbd "p") 'evil-case-pascal)
    (define-key map (kbd "_") 'evil-case-snake)
    (define-key map (kbd "-") 'evil-case-kebab)
    (define-key map (kbd "s") 'evil-case-sentence)
    (define-key map (kbd "t") 'evil-case-title)
    (define-key map (kbd "u") 'evil-case-lower)
    (define-key map (kbd "U") 'evil-case-upper)
    map)
  "Keymap for `evil-case'.")

(fset 'evil-case-map evil-case-map)

;; Optional Which-Key integration
(with-eval-after-load 'which-key
  (let ((prefix-name "Evil Case Transform"))
    (which-key-add-keymap-based-replacements evil-case-map
      "c" '("camelCase" . evil-case-camel)
      "p" '("PascalCase" . evil-case-pascal)
      "_" '("snake_case" . evil-case-snake)
      "-" '("kebab-case" . evil-case-kebab)
      "s" '("Sentence case" . evil-case-sentence)
      "t" '("Title Case" . evil-case-title)
      "u" '("downcase" . evil-case-lower)
      "U" '("UPCASE" . evil-case-upper))))

(provide 'evil-case)

;;; evil-case.el ends here
