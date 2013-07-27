;;;; Last modified : 2013-07-27 14:36:06 tkych

;; quicksearch/quicksearch.asd


;;====================================================================
;; QUICKSEARCH: searches CL Library, Quickly
;;====================================================================
;; quicksearch/
;;   quicksearch.asd
;;   quicksearch.lisp
;;   README.md
;;   LICENSE
;;   CHANGELOG


;;====================================================================
;; System for QUICKSEARCH
;;====================================================================


(asdf:defsystem #:quicksearch
  :name        "Quicksearch"
  :description "Quicksearch searches CL library, and outputs results at REPL."
  :version     "0.1.01"
  :licence     "MIT License"
  :author      "Takaya OCHIAI <tkych.repl@gmail.com>"
  :depends-on  (#:iterate #:anaphora #:alexandria #:cl-ppcre #:drakma
                #:yason #:flexi-streams #:do-urlencode #:html-entities
                #:bordeaux-threads)
  :components  ((:file "quicksearch"))
  )


;;====================================================================
