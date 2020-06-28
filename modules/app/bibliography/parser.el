;;; app/bibliography/parser.el -*- lexical-binding: t; -*-

;; Parsebib library adapted to the bibliography module.
;; 

;; Copyright (c) 2014-2017 Joost Kremers
;; All rights reserved.

;; Author: Joost Kremers <joostkremers@fastmail.fm>
;; URL: https://github.com/joostkremers/parsebib

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the author may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES ; LOSS OF USE,
;; DATA, OR PROFITS ; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(require 'bibtex)
(require 'cl-lib)

(defconst +bibliography-bibtex-identifier "[^\"@\\#%',={}() \t\n\f]+"
  "Regexp describing a licit BibTeX identifier.")
(defconst +bibliography-key-regexp "[^\"@\\#%',={} \t\n\f]+"
  "Regexp describing a licit key.")
(defconst +bibliography-entry-start "^[ \t]*@"
  "Regexp describing the start of an entry.")

(defun +bibliography-find-next-item (&optional pos)
  "Find the first (potential) BibTeX item following POS.
This function simply searches for an @ at the start of a line,
possibly preceded by spaces or tabs, followed by a string of
characters as defined by `+bibliography--bibtex-identifier'.  When
successful, point is placed right after the item's type, i.e.,
generally on the opening brace or parenthesis following the entry
type, \"@Comment\", \"@Preamble\" or \"@String\".
The return value is the name of the item as a string, either
\"Comment\", \"Preamble\" or \"String\", or the entry
type (without the @). If an item name is found that includes an
illegal character, an error of type `+bibliography-entry-type-error'
is raised. If no item is found, nil is returned and point is left
at the end of the buffer.
POS can be a number or a marker and defaults to point."
  (when pos (goto-char pos))
  (when (re-search-forward +bibliography-entry-start nil 0)
    (if (+bibliography-looking-at-goto-end
         (concat "\\(" +bibliography-bibtex-identifier "\\)" "[[:space:]]*[\(\{]?") 1)
        (match-string-no-properties 1)
      (signal '+bibliography-entry-type-error (list (point))))))

(defun +bibliography-read-entry (type &optional pos strings)
  "Read a BibTeX entry of type TYPE at the line POS is on.
TYPE should be a string and should not contain the @ sign. The
return value is the entry as an alist of (<field> . <contents>)
cons pairs, or nil if no entry was found. In this alist, the
entry key is provided in the field \"=key=\", the entry type in
the field \"=type=\", and the full entry's source in the field
\"=source=\". POS can be a number or a marker. It does not have
to be at the beginning of a line, but the entry must start at the
beginning of the line POS is on. If POS is nil, it defaults to
point. ENTRY should not be \"Comment\", \"Preamble\" or
\"String\", but is otherwise not limited to any set of possible
entry types. If so required, the calling function has to ensure
that the entry type is valid. If STRINGS is provided, it should
be a hash table with string abbreviations, which are used to
expand abbrevs in the entry's fields."
  (unless (member-ignore-case type '("comment" "preamble" "string"))
    (when pos (goto-char pos))
    (beginning-of-line)
    (when (+bibliography-looking-at-goto-end
           (concat +bibliography-entry-start type "[[:space:]]*[\(\{]"))
      ;; find the end of the entry and the beginning of the entry key
      (let* ((start (save-excursion
                      (search-backward "@")
                      (point)))
             (limit (save-excursion
                      (backward-char)
                      (+bibliography-match-paren-forward)
                      (point)))
             (beg (progn
                    (skip-chars-forward " \n\t\f") ; note the space!
                    (point)))
             (key (when (+bibliography-looking-at-goto-end
                         (concat "\\(" +bibliography-key-regexp "\\)[ \t\n\f]*,") 1)
                    (buffer-substring-no-properties beg (point)))))
        (or key (setq key "")) ; if no key was found, we pretend it's empty and try to read the entry anyway
        (skip-chars-forward "^," limit) ; move to the comma after the entry key
        (let ((fields (cl-loop for field = (+bibliography-find-bibtex-field limit strings)
                               while field collect field)))
          (push (cons "=type=" type) fields)
          (push (cons "=key=" key) fields)
          (push (cons "=source=" (buffer-substring-no-properties start limit)) fields)
          (nreverse fields))))))

(defun +bibliography-read-string (&optional pos strings)
  "Read the @String definition beginning at the line POS is on.
If a proper abbreviation and expansion are found, they are
returned as a cons cell (<abbrev> . <expansion>).  Otherwise, nil
is returned.
POS can be a number or a marker.  It does not have to be at the
beginning of a line, but the @String entry must start at the
beginning of the line POS is on.  If POS is nil, it defaults to
point.
If STRINGS is provided it should be a hash table with string
abbreviations, which are used to expand abbrevs in the string's
expansion."
  (when pos (goto-char pos))
  (beginning-of-line)
  (when (+bibliography-looking-at-goto-end
         (concat parsebib--entry-start "\\(string[[:space:]]*\\)[\(\{]") 1)
    (let ((limit (save-excursion
                   (+bibliography-match-paren-forward)
                   (point))))
      (+bibliography-looking-at-goto-end
       (concat "[({]\\(" +bibliography-bibtex-identifier "\\)[[:space:]]*=[[:space:]]*"))
      (let ((abbr (match-string-no-properties 1)))
        (when (and abbr (> (length abbr) 0))            ; if we found an abbrev
          (let ((expansion (+bibliography-parse-value limit strings)))
            (goto-char limit)
            (cons abbr expansion)))))))

(defun +bibliography-looking-at-goto-end (str &optional match)
  "Like `looking-at' but move point to the end of the matching string STR.
MATCH acts just like the argument to MATCH-END, and defaults to
0. Comparison is done case-insensitively."
  (or match (setq match 0))
  (let ((case-fold-search t))
    (if (looking-at str)
        (goto-char (match-end match)))))

(defun +bibliography-match-paren-forward ()
  "Move forward to the closing paren matching the opening paren at point.
This function handles parentheses () and braces {}.  Return t if
a matching parenthesis was found.  This function puts point
immediately after the matching parenthesis."
  (cond
   ((eq (char-after) ?\{)
    (+bibliography-match-brace-forward))
   ((eq (char-after) ?\()
    (bibtex-end-of-entry))))

(defun +bibliography-match-brace-forward ()
  "Move forward to the closing brace matching the opening brace at point."
  (with-syntax-table bibtex-braced-string-syntax-table
    (forward-sexp 1)
    ;; if forward-sexp does not result in an error, we want to return t
    t))

(defun +bibliography-find-bibtex-field (limit &optional strings)
  "Find the field after point.
Do not search beyond LIMIT (a buffer position).  Return a
cons (FIELD . VALUE), or nil if no field was found.
If STRINGS is provided it should be a hash table with string
abbreviations, which are used to expand abbrevs in the field's
value."
  (skip-chars-forward "\"#%'(),={} \n\t\f" limit) ; move to the first char of the field name
  (unless (>= (point) limit)                      ; if we haven't reached the end of the entry
    (let ((beg (point)))
      (if (+bibliography-looking-at-goto-end
           (concat "\\(" +bibliography-bibtex-identifier "\\)[[:space:]]*=[[:space:]]*") 1)
          (let ((field-type (buffer-substring-no-properties beg (point))))
            (let ((field-contents (+bibliography-parse-value limit strings)))
              (cons field-type field-contents)))))))

(defun +bibliography-parse-value (limit &optional strings)
  "Parse value at point.
A value is either a field value or a @String expansion.  Return
the value as a string.  No parsing is done beyond LIMIT, but note
that parsing may stop well before LIMIT.
STRINGS, if non-nil, is a hash table of @String definitions.
@String abbrevs in the value to be parsed are then replaced with
their expansions.  Additionally, newlines in field values are
removed, white space is reduced to a single space and braces or
double quotes around field values are removed."
  (let (res)
    (while (and (< (point) limit)
                (not (looking-at-p ",")))
      (cond
       ((looking-at-p "[{\"]")
        (let ((beg (point)))
          (+bibliography-match-delim-forward)
          (push (buffer-substring-no-properties beg (point)) res)))
       ((looking-at +bibliography-bibtex-identifier)
        (push (buffer-substring-no-properties (point) (match-end 0)) res)
        (goto-char (match-end 0)))
       ((looking-at "[[:space:]]*#[[:space:]]*")
        (goto-char (match-end 0)))
       (t (forward-char 1)))) ; so as not to get stuck in an infinite loop.
    (if strings
        (string-join (+bibliography-expand-strings (nreverse res) strings))
      (string-join (nreverse res) " # "))))

(defun +bibliography-match-delim-forward ()
  "Move forward to the closing delimiter matching the delimiter at point.
This function handles braces {} and double quotes \"\". Return t
if a matching delimiter was found."
  (let ((result (cond
                 ((eq (char-after) ?\{)
                  (+bibliography-match-brace-forward))
                 ((eq (char-after) ?\")
                  (+bibliography-match-quote-forward)))))
    result))

(defun +bibliography-match-quote-forward ()
  "Move to the closing double quote matching the quote at point."
  (with-syntax-table bibtex-quoted-string-syntax-table
    (forward-sexp 1)
    ;; if forward-sexp does not result in an error, we want to return t
    t))

(defun +bibliography-expand-strings (strings abbrevs)
  "Expand strings in STRINGS using expansions in ABBREVS.
STRINGS is a list of strings.  If a string in STRINGS has an
expansion in hash table ABBREVS, replace it with its expansion.
Otherwise, if the string is enclosed in braces {} or double
quotes \"\", remove the delimiters.  In addition, newlines and
multiple spaces in the string are replaced with a single space."
  (mapcar (lambda (str)
            (setq str (replace-regexp-in-string "[ \t\n\f]+" " " str))
            (cond
             ((gethash str abbrevs))
             ((string-match "\\`[\"{]\\(.*?\\)[\"}]\\'" str)
              (match-string 1 str))
             (t str)))
          strings))
