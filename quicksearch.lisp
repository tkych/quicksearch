;;;; Last modified : 2013-06-21 21:17:48 tkych

;; quicksearch/quicksearch.lisp


;;====================================================================
;; Quicksearch
;;====================================================================

(in-package :cl-user)

(defpackage #:quicksearch
  (:nicknames #:qs)
  (:use :cl :iterate)
  (:import-from #:anaphora
                #:aif #:awhen #:it)
  (:export #:quicksearch
           #:?
           #:config))

(in-package #:quicksearch)

(declaim (inline strip remove-tags))


;;--------------------------------------------------------------------
;; Utilities
;;--------------------------------------------------------------------

(defun str (&rest strings)
  "Return appended string."
  (apply #'concatenate 'string strings))


(defun map-reduce (reducer mapper sequence)
  "
MAP-REDUCE is suitable for only the case that MAPPER's cost is much expensive.
 (QUICKSEARCH uses this function for drakma:http-request)
For each element in SEQUENCE (as an argument), MAPPER is computed with each thread,
then the results are collected by REDUCER.

Examples: (inefficient but intuitive)

  (map-reduce #'+
              (lambda (x) (expt x 2)) ;<- each thread computes this
              '(1 2 3 4))
   => 30

  (map-reduce #'append
              (lambda (x) (list (expt x 2)))
              '(1 2 3 4))
   => (1 4 9 16)

  (map-reduce (lambda (x y) (format nil \"~A~A\" x y))
              (lambda (c) (code-char (1- (char-code c))))
             \"IBM\")
   => \"HAL\"

Note:
  * Use only for expensive computation.
  * If MAPPER assigns a value for the same global variable, it will cause interleave.
  * Strictly, this MAP-REDUCE is not the MapReduce,
    but abstract model is the same if threads are equated with worker nodes.
"
  (reduce reducer
          (map 'vector
               (lambda (x)
                 (bordeaux-threads:make-thread
                  (lambda () (funcall mapper x))))
               sequence)
          :key #'bordeaux-threads:join-thread))


;;--------------------------------------------------------------------
;; Main
;;--------------------------------------------------------------------
;; <word>  ::= <string>
;; <space> ::= <symbol>, s.t. {quicklisp | github | cliki | bitbucket}
;; <repo>  ::= (<title> <url> <description>)
;; <repos> ::= (<repo>*)
;; <title> ::= <string>
;; <url>   ::= <string>
;; <description> ::= {<string> | NIL}

(defvar *threading?* t)
(defparameter *description-print?* nil)
(defparameter *url-print?* nil)
(defparameter *cut-off* 50)
(defparameter *print-search-results?* nil)

(defun quicksearch (search-word
                    &key (?web t) (?description nil) (?url nil) (?cut-off 50)
                         (?quicklisp t) (?cliki t) (?github t) (?bitbucket t))
  "
Search for CL projects with SEARCH-WORD in Quicklisp, Cliki, GitHub and BitBucket.
SEARCH-WORD must be a string, number or symbol (symbol will be automatically converted into downcase-string).

Keywords:
 * If ?WEB is NIL, it does not search in Cliki, GitHub and BitBucket.
 * If ?QUICKLISP is NIL, it does not search in Quicklisp (also ?CLIKI, ?GITHUB, ?BITBUCKET).
 * At least one search-space must be specified.
 * If ?DESCRIPTION is T, it displays project's descriptions (except for Quicklisp-search).
 * If ?URL is T, it display project's url.
 * ?CUT-OFF is the max number of printing repositories each space.

Note:
 * keyword ?CUT-OFF controls only printing results,
   nothing to do with the maximum number of fetching repositories (c.f. function CONFIG documentation).

 * About #\\Space in SEARCH-WORD:
   In case search-word contains #\\Space, Quicklisp-search is OR-search,
   whereas Cliki-search, GitHub-, BitBucket- is AND-search.
   e.g. (quicksearch \"foo bar\")
        Quicklisp-search for \"foo\" OR \"bar\",
        Cliki-search, GitHub-, BitBucket- for \"foo\" AND \"bar\".
"
  (check-type search-word (or string symbol))
  (check-type ?web boolean) (check-type ?description boolean) (check-type ?url boolean)
  (check-type ?cut-off (integer 1 *)) (check-type ?quicklisp boolean) (check-type ?cliki boolean)
  (check-type ?github boolean) (check-type ?bitbucket boolean)
  (let ((*url-print?* ?url)
        (*description-print?* ?description)
        (*cut-off* ?cut-off)
        (word (write-to-string search-word :case :downcase :escape nil))
        (found? nil)
        (*print-search-results?* nil)  ;no result, no print
        (threads '()))

    (when (and ?web *threading?*)
      ;; MAP Phase
      ;; (Strictly, the following is not the MapReduce,
      ;;  but abstract model is the same if threads are equated with worker nodes.)
      (let ((drakma:*drakma-default-external-format* :utf-8))
        ;; (print 'threading) ;for test
        (loop :for space   :in '(cliki github bitbucket)
              :for search? :in (list ?cliki ?github ?bitbucket) :do
           (when (and search? (not (in-cache-p word space)))
             ;; Search word in the web, and Store result into cache.
             ;; Since each space has its own cache, lock isn't need.
             (push (search-web-by-thread word space) threads)))))

    #+quicklisp  ;for build
    (when ?quicklisp
      ;; quicklisp-search is OR-search
      (dolist (w (ppcre:split " " word))
        (awhen (search-quicklisp w)
          (print-results word it 'quicklisp)
          (setf found? t))))

    (when ?web
      (if *threading?*
          (progn  ;REDUCE Phase
            ;; (print 'threading) ;for test
            (dolist (th threads) (bordeaux-threads:join-thread th))
            (loop
               :for space   :in '(cliki github bitbucket)
               :for search? :in (list ?cliki ?github ?bitbucket) :do
               (awhen (and search? (search-cache word space))
                 (print-results word it space)
                 (setf found? t))))
          (loop  ;not using threads
             :for space   :in '(cliki github bitbucket)
             :for search? :in (list ?cliki ?github ?bitbucket) :do
             (when search?
               ;; (print 'non-threading) ;for test
               (multiple-value-bind (repos stored?) (search-cache word space)
                 (if stored?
                     (progn
                       (print-results word repos space)
                       (setf found? t))
                     (awhen (search-web word space)
                       (print-results word it space)
                       (setf found? t))))))))

    found?))


;;--------------------------------------------------------------------
;; Search Spaces
;;--------------------------------------------------------------------

;; for print search-space name
(setf (get 'quicklisp :name) "Quicklisp"
      (get 'cliki     :name) "Cliki"
      (get 'github    :name) "GitHub"
      (get 'bitbucket :name) "BitBucket")

;; for print url
(setf (get 'quicklisp :host) "~A"
      (get 'cliki     :host) "http://www.cliki.net~A"
      (get 'github    :host) "https://github.com/~A"
      (get 'bitbucket :host) "https://bitbucket.org~A")

;; for generate query
(setf (get 'cliki :query-format)
      "http://www.cliki.net/site/search?query=~A")

;; github api-v3 search
(setf (get 'github :query-format)
      "https://api.github.com/legacy/repos/search/~A~
       ?language=Common%20Lisp")

;; github advanced search
;; (setf (get 'github :query-format)
;;       "https://github.com/search?q=~A~
;;        &type=Repositories&ref=advsearch&l=Common+Lisp")

(setf (get 'bitbucket :query-format)
      "https://bitbucket.org/repo/all/relevance?name=~A~
       &language=common+lisp")

;; drakma options
(setf (get 'cliki     :drakma-options) nil
      (get 'github    :drakma-options) '(:preserve-uri t)
      (get 'bitbucket :drakma-options) '(:preserve-uri t))


;;--------------------------------------------------------------------
;; Search-Quicklisp
;;--------------------------------------------------------------------

(defvar *quicklisp-verbose?* nil)

#+quicklisp  ;for build
(progn

  (defparameter *installed-prefix* "!")
  
  (defun search-quicklisp (word)
    (loop
       :for sys :in (ql-dist:provided-systems t)
       :when (or (search word (ql-dist:name sys))
                 (search word (ql-dist:name (ql-dist:release sys))))
       :collect (list (get-title sys)
                      (when *url-print?* (get-url sys))
                      nil)            ;(ql-dist:short-description sys)
                                      ; <=> (ql-dist:name sys)
       :into results
       :finally (progn
                  (when *quicklisp-verbose?* (set-version sys))
                  (return results))))
  
  (defun get-title (sys)
    (if (and *quicklisp-verbose?* (installed-p sys))
        (str *installed-prefix* (ql-dist:name sys))
        (ql-dist:name sys)))

  (defun installed-p (sys)
    (handler-case (not (ql-dist:check-local-archive-file
                        (ql-dist:release sys)))
      (ql-dist:missing-local-archive () nil)))
   
  (defun get-url (sys)
    (aif (ql:where-is-system (ql-dist:name sys))
         (str (format nil "~A" it)
              #.(format nil "~%      ")
              (slot-value (ql-dist:release sys)
                          'ql-dist::archive-url))
         (slot-value (ql-dist:release sys)
                     'ql-dist::archive-url)))

  (defun set-version (sys)
    (setf (get 'quicklisp :name)
          (str "Quicklisp: " (ql-dist:version (ql-dist:dist sys)))))

  )  ;end of #+quicklisp


;;--------------------------------------------------------------------
;; Search-Cache
;;--------------------------------------------------------------------
;; <cache> ::= <hashtable> stored previous search results.
;;             For non-lock threads, each space has its own cache
;;             in symbol-plist.
;;             max-size = *cache-size* + 1
;;             [key, val] = {[<word>, <repos>] | [:history, <history>]}
;; <histroy> ::= (<word>*), max-length = *cache-size*, using as queue.
;;               The front one is the most old, last one is the most new.

(defparameter *cache-size* 4)

(defun make-cache ()                    ;for config
  (setf (get 'cliki      :cache)
        (make-hash-table :test #'equal :size (1+ *cache-size*)))
  (setf (get 'github     :cache)
        (make-hash-table :test #'equal :size (1+ *cache-size*)))
  (setf (get 'bitbucket  :cache)
        (make-hash-table :test #'equal :size (1+ *cache-size*))))

(make-cache)

(defun clear-cache ()
  (dolist (space '(cliki github bitbucket))
    (clrhash (get space :cache)))
  t)

(defun search-cache (word space)
  (gethash word (get space :cache) nil))

(defun in-cache-p (word space)
  (nth-value 1 (gethash word (get space :cache) nil)))

(defun store-cache (word repos space)
  (let* ((cache   (get space :cache))
         (history (gethash :history cache '())))
    ;; If cache is full, then remove the most old one
    (when (<= *cache-size* (length history))
      (remhash (pop history) cache))
    (setf (gethash word (get space :cache))
          repos)
    (setf (gethash :history (get space :cache))
          (append history (list word)))))


;;--------------------------------------------------------------------
;; Search-Web
;;--------------------------------------------------------------------
;; 1. FETCH word from space
;; 2. EXTRACT repos from response
;; 3. GOTO 1 if next page exists (word <-- next-url)
;; 4. STORE repos in cache
;; 5. RETURN repos

(defun search-web (word space)
  (let* ((response (fetch (gen-query word space) space))
         (repos    (extract-repos response space)))
    (awhen (extract-next-page-url response space)
      (if *threading?*
          (alexandria:appendf repos
            (map-reduce #'append
                        (lambda (url)
                          (extract-repos (fetch url space) space))
                        it))
          (loop
             :for url :in it
             :for res := (fetch url space)
             :do (alexandria:appendf
                  repos (extract-repos res space)))))
    (store-cache word repos space)
    repos))

(defun search-web-by-thread (word space)
  (bordeaux-threads:make-thread
   (lambda () (search-web word space))
   :name (format nil "~(~A~)-search" space)))


;;--------------------------------------
;; github api-v3 search
(defun gen-query (word space)
  (format nil (get space :query-format)
          (if (eq space 'github)
              (do-urlencode:urlencode word)
              (nsubstitute #\+ #\Space word :test #'char=))))

;; github advanced search
;; (defun gen-query (word space)
;;   (format nil (get space :query-format)
;;           (nsubstitute #\+ #\Space word :test #'char=)))

(defun fetch (query space)
  (apply #'drakma:http-request query (get space :drakma-options)))


;;--------------------------------------
;; dispatch for extract repositories
(defun extract-repos (response space)
  (case space
    (cliki     (extract-cliki-repos response))
    (github    (extract-github-repos response))
    (bitbucket (extract-bitbucket-repos response))))

(defun strip (string)
  (string-trim '(#\Space #\Return #\Newline) string))

(defun remove-tags (string)
  (ppcre:regex-replace-all "(<.+?>)" string ""))

(defun extract-cliki-repos (response)
  (let* ((results (ppcre:scan-to-strings
                   "(?s)<ol start=.+?>(.+?)</ol>" response))
         (repos (ppcre:all-matches-as-strings
                 "(?s)<li>(.+?)</li>" results)))
    (when repos
      (iter (for repo :in repos)
            (ppcre:register-groups-bind (url title description)
                ("(?s)<li><a href=\"(.+?)\" class=\"internal\">(.+?)</a>\\s?<br\\s?/?>(.+?)</li>"
                 repo)
              (collect (list title url
                             (let ((desc (strip (remove-tags description))))
                               (when (string/= "" desc) desc)))))))))

;; github api-v3 search
(defun extract-github-repos (response)
  (let* ((jason (yason:parse (flexi-streams:octets-to-string
                              response :external-format :utf-8))))
    (loop :for repo :in (gethash "repositories" jason)
          :unless (gethash "fork" repo)   ;only master is displayed
          :collect (list (gethash "name" repo)
                         (gethash "url" repo)
                         (gethash "description" repo)))))

;; github advanced search
;; (defun extract-github-repos (response)
;;   (let* ((results (ppcre:scan-to-strings
;;                    "(?s)<ul class=\"repolist js-repo-list\">(.+<!-- /.body -->)"
;;                    response))
;;          (repos (ppcre:all-matches-as-strings
;;                  "(?s)<h3>(.+?)</p>"
;;                  results)))
;;     (when repos
;;       (iter (for repo :in repos)
;;             (ppcre:register-groups-bind (url title)
;;                 ("(?s)<h3>.+?<a href=\"/(.+?)\">.+?/(.+?)</a>" repo)
;;               (collect
;;                   (list (ppcre:regex-replace-all "</?em>" title "")
;;                         url
;;                         (ppcre:register-groups-bind (description)
;;                             ("(?s)<p class=\"description\">(.+?)</p>" repo)
;;                           (strip (remove-tags description))))))))))

(defun extract-bitbucket-repos (response)
  (let* ((results (ppcre:scan-to-strings
                   "(?s)<section id=\"repo-list\">(.+?)</section>"
                   response))
         (repos (ppcre:all-matches-as-strings
                 "(?s)<article class=\"repo-summary\">(.+?)</article>"
                 results)))
    (when repos
      (iter (for repo :in repos)
            (ppcre:register-groups-bind (url title)
                ("(?s)<a class=\"repo-link\" href=\"(.+?)\">.+? / (.+?)</a>"
                 repo)
              (collect (list title url
                             (ppcre:register-groups-bind (description)
                                 ("(?s)<p>(.+?)</p>" repo)
                               (strip (remove-tags description))))))))))


;;--------------------------------------
;; dispatch for extracting next url
(defun extract-next-page-url (response space)
  (case space
    (cliki     (extract-cliki-next-page-url response))
    (github    (extract-github-next-page-url response))
    (bitbucket (extract-bitbucket-next-page-url response))))

(defvar *max-num-web-search-results* 50)
(defvar *num-results-per-page* 10)

(defun max-num-next-pages ()
  (ceiling (- *max-num-web-search-results* *num-results-per-page*)
           *num-results-per-page*))

(defun extract-cliki-next-page-url (response)
  (let ((urls nil)
        (paginator (ppcre:scan-to-strings
                    "(?s)<div id=\"paginator\">(.+?)</div>" response)))
    (ppcre:do-register-groups (query)
        ("<a href=\"\\\?query=(.+?)\">" paginator)
      (push (gen-query query 'cliki) urls))
    (let ((rest-urls (nreverse (rest urls)))) ;first and last is the same.
      (subseq rest-urls
              0 (min (max-num-next-pages) (length rest-urls))))))

;; github api-v3 search
(defun extract-github-next-page-url (response)
  (declare (ignore response))
  nil)

;; for github advanced search
;; (defun extract-github-next-page-url (response)
;;   (let ((urls nil)
;;         (pagination
;;          (ppcre:scan-to-strings
;;           "(?s)<div class=\"pagination\"(.+?)</div>" response)))
;;     (ppcre:do-register-groups (next-url)
;;         ("<a href=\"(.+?)\"( (class|rel).+?)?>" pagination)
;;       (push (format nil "https://github.com~A"
;;                     (html-entities:decode-entities next-url))
;;             urls))
;;     (let ((rest-urls (nreverse (rest urls)))) ;first and last is the same.
;;       ;; (print rest-urls) ;for dbg
;;       (subseq rest-urls
;;               0 (min (max-num-next-pages) (length rest-urls))))))

(defun extract-bitbucket-next-page-url (response)
  (let ((urls nil)
        (paginator (ppcre:scan-to-strings
                    "(?s)<ol class=\"paginator\">(.+?)</ol>" response)))
    (ppcre:do-register-groups (next-url)
        ("<a href=\"(.+?)\">" paginator)
      (push next-url urls))
    (let ((rest-urls (nreverse (rest urls)))) ;first and last is the same.
      (subseq rest-urls
              0 (min (max-num-next-pages) (length rest-urls))))))


;;--------------------------------------------------------------------
;; Print-Results
;;--------------------------------------------------------------------

(defparameter *max-num-description-columns* 80)
(defparameter *description-indent-num* 6)

(defun print-line (n char)
  (format t (format nil "~~~D,,,'~AA" n char) char))

(defun print-search-results (word)
  (format t "~%SEARCH-RESULTS: ~S~%" word)
  (when (or *description-print?* *url-print?*)
    (print-line
     (+ #.(length "SEARCH-RESULTS: \"\"") (length word))
     #\=)
    (terpri)))

(defun gen-url (url space)
  (format nil (get space :host) url))

(defun print-space (space)
  (format t "~% ~A~%" (get space :name))
  (when (or *description-print?* *url-print?*)
    (format t " ") (print-line (length (get space :name)) #\-)))

(defun print-results (word repos space)
  (when repos
    ;; only when search-result has not printed yet, print it.
    (unless *print-search-results?*
      (print-search-results word)
      (setf *print-search-results?* t))
    (print-space space)
    (loop :for (title url desc) :in repos
          :repeat *cut-off* :do
       (format t "~&  ~A" (html-entities:decode-entities title))
       (when *url-print?*
         (format t "~%      ~A" (gen-url url space)))
       (when *description-print?*
         (pprint-description
          (html-entities:decode-entities desc))))
    (when (< *cut-off* (length repos))
      (format t "~&  .......> ~D" (- (length repos) *cut-off*)))
    (terpri)
    t))

(defun pprint-description (desc)
  (when desc
    (let ((len (length desc))
          (max-nchars (- *max-num-description-columns*
                         *description-indent-num*)))
      (if (<= len max-nchars)
          (format t "~%      ~A" desc)
          (let ((space-pos
                 (loop :for i :downfrom max-nchars :to 0
                       :until (char= #\Space (char desc i))
                       :finally (return (1+ i)))))
            (if (zerop space-pos)
                (progn
                  (format t "~%      ~A-" (subseq desc 0 (1- max-nchars)))
                  (pprint-description (subseq desc (1- max-nchars))))
                (progn
                  (format t "~%      ~A" (subseq desc 0 (1- space-pos)))
                  (pprint-description (subseq desc space-pos)))))))))


;;--------------------------------------------------------------------
;; Configuration for Quicksearch
;;--------------------------------------------------------------------

(defun config (&key ((:maximum-columns-of-description max-cols)
                                        80  max-cols-supplied?)
                    ((:maximum-number-of-fetching-repositories max-repos)
                                        50  max-repos-supplied?)
                    (cache-size         4   cache-size-supplied?)
                    (clear-cache?       nil clear-cache-supplied?)
                    (threading?         t   threading-supplied?)
                    (quicklisp-verbose? nil quicklisp-verbose-supplied?))
  "
Function CONFIG customizes printing, fetching or caching.

Keywords:
 * :MAXIMUM-COLUMNS-OF-DESCRIPTION (default 80)
   The value must be a plus integer bigger than 5.
   If the length of description-string is bigger than this value,
   then output of description is inserted newline for easy to see.

 * :MAXIMUM-NUMBER-OF-FETCHING-REPOSITORIES (default 50)
   This value controls the number of fetching repositories.
   The value must be a plus integer.
   Increasing this value, the number of fetching repositories increases, but also space & time does.

 * :CACHE-SIZE
   The value must be a plus integer (default 4).
   This value is the number of stored previous search-results.
   Increasing this value, the number of caching results increases, but also space does.

 * :CLEAR-CACHE?
   The value must be a boolean (default NIL).
   If value is T, then clear all caches.

 * :THREADING?
   The value must be a boolean (default T).
   If value is NIL, then QUICKSEARCH becomes not to use threads for searching.

   Note:
     Currently in SBCL (1.1.8), threads are part of the default build on x86[-64] Linux only.
     Other platforms (x86[-64] Darwin (Mac OS X), x86[-64] FreeBSD, x86 SunOS (Solaris),
     and PPC Linux) experimentally supports threads and must be explicitly enabled at build-time.
     For more details, please see [SBCL manual](http://www.sbcl.org/manual/index.html#Threading).

 * :QUICKLISP-VERBOSE?
   The value must be a boolean (default NIL).
   If value is T, then outputs version of quicklisp and whether library had installed your local.

   Example:
     CL-REPL> (qs:config :QUICKLISP-VERBOSE? t)
     CL-REPL> (qs:? \"json\" :q)

     SEARCH-RESULTS: \"json\"

      Quicklisp: 2013-04-20   ;<- quicklisp version
       !cl-json               ;<- if library has been installed via quicklisp, print prefix \"!\".
       !cl-json.test
       com.gigamonkeys.json   ;<- if not, none.
       json-template
       st-json
     T

Note:
 * If you would prefer permanent configuration,
   for example, add codes something like the following in the CL init file.

   In `.sbclrc` for SBCL, `ccl-init.lisp` for CCL:

   (ql:quickload :quicksearch)
   (qs:config :maximum-columns-of-description 50
              :maximum-number-of-fetching-repositories 20
              :cache-size 2
              :threading? nil
              :quicklisp-verbose? t)
"
  (if (not (or max-cols-supplied?  max-repos-supplied?
               cache-size-supplied? clear-cache-supplied?
               threading-supplied? quicklisp-verbose-supplied?))
      (error "At most one keyword must be supplied.")
      (progn
        (when max-cols-supplied?
          (if (and (integerp max-cols)
                   (< *description-indent-num* max-cols))
              (progn
                (setf *max-num-description-columns* max-cols)
                (format t "~&Current maximum columns of description: ~D"
                        *max-num-description-columns*)
                (clear-cache)
                t)
              (error "~S is not plus integer above ~D."
                     max-cols *description-indent-num*)))
        (when max-repos-supplied?
          (if (and (integerp max-repos)
                   (plusp max-repos))
              (progn
                (setf *max-num-web-search-results* max-repos)
                (format t "~&Current maximum number of fetching repositories: ~D"
                        *max-num-web-search-results*)
                t)
              (error "~S is not plus integer." max-repos)))
        (when cache-size-supplied?
          (if (and (integerp cache-size)
                   (<= 0 cache-size))
              (progn
                (setf *cache-size* cache-size)
                (make-cache)
                (format t "~&Current cache size: ~D"  *cache-size*)
                t)
              (error "~S is not plus integer." cache-size)))
        (when clear-cache-supplied?
          (if (and clear-cache? (typep clear-cache? 'boolean))
              (progn
                (clear-cache)
                (format t "All caches cleaned.")
                t)
              (error "~S is not boolean." clear-cache?)))
        (when threading-supplied?
          (if (typep threading? 'boolean)
              (progn
                (setf *threading?* threading?)
                (format t "~&Using threads for searching: ~S" threading?)
                t)
              (error "~S is not boolean." threading?)))
        (when quicklisp-verbose-supplied?
          (if (typep quicklisp-verbose? 'boolean)
              (if quicklisp-verbose?
                  (progn
                    (setf *quicklisp-verbose?* t)
                    (format t "~&Quicklisp verbose: T")
                    t)
                  (progn
                    (setf *quicklisp-verbose?*  nil
                          (get 'quicklisp :name) "Quicklisp")
                    (format t "~&Quicklisp verbose: NIL")
                    t))
              (error "~S is not boolean." quicklisp-verbose?)))
        )))


;;--------------------------------------------------------------------
;; Abbreviation for Quicksearch
;;--------------------------------------------------------------------

(defun ? (search-word &rest options)
  "
? is abbreviation wrapper for function QUICKSEARCH.
SEARCH-WORD must be a string, number or symbol.
OPTIONS must be a non-negative integer (as Cut-Off) and/or some keywords which consists of some Option-Chars.

Options:
 * Cut-Off:
   * The max number of printing results (default is 50).
 * Option-Chars:
   * d, D -- output Description
   * u, U -- output URL
   * q, Q -- search in Quicklisp
   * c, C -- search in Cliki
   * g, G -- search in GitHub
   * b, B -- search in Bitbucket

Note:
 * Option-Char is idempotent (e.g. :dd <=> :d).
 * If OPTIONS contains more than 2 Cut-Offs, only last one is applyed.
 * The order of Option-Chars is nothing to do with output. (e.g. :du <=> :ud)
 * The order of OPTIONS is nothing to do with output (except for some Cut-Offs).
 * If no search-space is specified, all spaces are specified (e.g. :d <=> :dqcgb)
 * If at most one search-space is specified, then others are not specified.

Examples:
  (? \"crypt\")
  <=>
  (quicksearch \"crypt\" :?description nil :?url nil :?cut-off 50
                       :?quicklisp t :?cliki t :?github t :?bitbucket t)

  (? \"crypt\" :du 10)
  <=>
  (quicksearch \"crypt\" :?description T :?url T :?cut-off 10
                       :?quicklisp t :?cliki t :?github t :?bitbucket t)

  (? \"crypt\" 20 :g :d)
  <=>
  (quicksearch \"crypt\" :?description T :?url nil :?cut-off 20
                       :?quicklisp nil :?cliki nil :?github T :?bitbucket nil)
"
  (let ((cut-off 50)
        (d nil) (u nil) (q nil) (c nil) (g nil) (b nil))
    (dolist (opt options)
      (cond ((keywordp opt)
             (loop :for char :across (symbol-name opt) :do
                (case char
                  ((#\D #\d) (setf d t))
                  ((#\U #\u) (setf u t))
                  ((#\Q #\q) (setf q t))
                  ((#\C #\c) (setf c t))
                  ((#\G #\g) (setf g t))
                  ((#\B #\b) (setf b t))
                  (t         (error "~A is unknown option." char)))))
            ((and (integerp opt) (not (minusp opt)))
             (setf cut-off opt))
            (t
             (error "~S is neither keyword nor non-negative-integer." opt))))
    (if (or q c g b)
        (quicksearch search-word
                     :?description d :?url u :?cut-off cut-off
                     :?quicklisp q :?cliki c :?github g :?bitbucket b)
        (quicksearch search-word
                     :?description d :?url u :?cut-off cut-off
                     :?quicklisp T :?cliki T :?github T :?bitbucket T))))


;;====================================================================
