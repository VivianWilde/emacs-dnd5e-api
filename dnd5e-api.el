;;; dnd5e-api.el --- An interface to the 5e SRD API using completing-read
;;; -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Rohan Goyal
;;
;; Author: Rohan Goyal  <https://github.com/rohan>
;; Maintainer: Rohan Goyal  <goyal.rohan.03@gmail.com>
;; Created: January 01, 2022
;; Modified: January 01, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/rohan/dnd5e-api
;; Package-Requires: ((emacs "25.1") (seq "0.0") ('plz "0.0") ('ht "0.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'plz)
(require 'seq)
(require 'ht)

                                        ; Variable Declarations
(defvar dnd5e-api-base-url "http://localhost:3000")

(defvar dnd5e-api-endpoint-field-hash
  ;; Hash which links certain endpoints to a list of symbols, where each symbol is a field we care about
  #s(hash-table
     test equal
     data (
           "/api/skills" (name desc)
           "/api/languages" (name typical_speakers desc)
           "/api/classes" (name hit_die saving_throws)
           "/api/features" (name desc level class subclass)
           "/api/races" (name subraces ability_bonuses alignment language_desc)
           "/api/traits" (name desc races subraces)
           "/api/equipment" (name desc cost damage armor_class category_range range contents)
           "/api/magic-items" (name desc)
           "/api/spells" () ;DONE How to handle all? Solution - if we want all fields, leave this empty. Then our func checks if fields is nil, and gets all fields if that is so
           "/api/monsters" (name type subtype speed strength dexterity constitution intelligence wisdom charisma challenge_rating special-abilities)
           "/api/conditions" (name desc)
           "/api/rules" (name desc)
           "/api/rule-sections" (name desc)
           )))


                                        ; String processing/input/output
(defun dnd5e-api-make-readable (str)
  "Turn STR from '/api/whatever' to 'Whatever'."
  (capitalize (car (last (split-string str "/api/")))))

(defun dnd5e-api-make-unreadable (str)
  "Turn STR from 'Whatever' to '/api/whatever'."
  (concat "/api/" (downcase str)))


(defun dnd5e-api-make-prompt (str)
  "Clean STR a bit to make it work as a 'completing-read prompt."
  (let* (
         (singular (lambda (noun) noun))
         (make-prompt (lambda (str) (format "%s: " (dnd5e-api-make-readable (singular str)))))
         )
    (funcall make-prompt str))
  )

(defun dnd5e-api-output (result)
  "Format and output the alist RESULT."
  (message "%s" (pp-to-string result))
  )


                                        ; API Utilities
(defun dnd5e-api-get-response (endpoint)
  "One-liner to go from ENDPOINT to a URL"
  (plz 'get (concat dnd5e-api-base-url endpoint) :as #'json-read))

(defun dnd5e-api-generate-vec (endpoint)
  "Return a vector of the results at ENDPOINT. Each element is an alist with keys (index name url)."
  (let* (
         (response (dnd5e-api-get-response endpoint))
         (results (alist-get 'results response))
         )
    results
    ))
(defun dnd5e-api-ask-for-item (endpoint)
  "Let user choose an item from ENDPOINT via 'completing-read, and return a new endpoint pointing to that item."
  (let* (
         (results (dnd5e-api-generate-vec endpoint))
         (returned (dnd5e-api-name-url-hash results)) ; New-assoc is of the form ((name url)
         (hashtable (car returned))
         (keylist (cadr returned))
         (choice (completing-read (dnd5e-api-make-prompt endpoint) keylist nil t))

         )
    (gethash choice hashtable)
    ))

(defun dnd5e-api-name-url-hash (seq)
  "Take in a sequence SEQ of alists. Return a list, where the first elem is a hash of the form {name : url} and the second is a list of the names."
  (let* (( hashtable (make-hash-table :test #'equal))
         (keylist '())
         (converter (lambda (alist) "Add a transformed assoclist to the hashtable and keylist "
                      (progn
                        (puthash ( alist-get 'name alist) (alist-get 'url alist) hashtable)
                        (push (alist-get 'name alist) keylist)))))
    (mapc converter seq)
    (list hashtable keylist)
    )
  )
(defun dnd5e-api-get-keys (endpoint keys)
  "Given an ENDPOINT return an alist containing data from that ENDPOINT at specified KEYS. If key does not exist, it is ignored."
  (let* (
         (response (dnd5e-api-get-response endpoint))
         (get-keys (lambda (alist keys) (seq-filter (lambda (x) x) (seq-map (lambda (key) (assoc key alist)) keys))))
         )
    (funcall get-keys response keys))
  )


                                        ; Wrapper Functions
(defun dnd5e-api-get (endpoint &optional keys)
  "If KEYS exists and is non-nil pass ENDPOINT to 'dnd5e-api-get-keys, otherwise call 'dnd5e-api-response."
  (cond
   (keys (dnd5e-api-get-keys endpoint keys))
   ((dnd5e-api-get-response endpoint)))
  )
;; Assoc returns nil if nothing is found, rather than raising an error. Which is nice.

(defun dnd5e-api-handle (endpoint)
  "Prompt user with a list of all items at ENDPOINT, and then get data for the selected item."
  (let* (
         (fields (gethash endpoint dnd5e-api-endpoint-field-hash '(name desc)))
         (new-endpoint (dnd5e-api-ask-for-item endpoint))
         )
    (dnd5e-api-get new-endpoint fields)))

                                        ; Interactive Components
;;;###autoload
(defun dnd5e-api-search()
  "Let user pick an endpoint, select an item from the resulting list, and display that."
  (interactive)
  (let* (
         (choice (completing-read "Search For: " (seq-map 'dnd5e-api-make-readable (ht-keys dnd5e-api-endpoint-field-hash))))
         (result (dnd5e-api-handle (dnd5e-api-make-unreadable choice)))
         )
    (dnd5e-api-output result)
    )
  )

                                        ; Put off until tomorrow TODO
(defun dnd5e-api-get-list (endpoint)
  "Instead of using completing read, simply return the list of items at ENDPOINT.")

(defun dnd5e-api-custom ()
  "Let users pick a custom endpoint and do stuff there."
  (interactive)
  (let* (
         (endpoint ())))

  )


(defun dnd5e-api-process-response (alist)
  "Do simple cleanup on ALIST like turn desc from a vector into a string. Only called on filtered alists, for efficiency and convenience."
  (seq-map #'dnd5e-api-process-pair alist
           )
  )
;;  Define simple processors for things like cost, other common models. Also turning desc from a vector into a string
;; Once we have the cleaned up assoc-list, we basically need to format it to text and then display it.


(provide 'dnd5e-api)
;;; dnd5e-api.el ends here
