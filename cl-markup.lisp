(in-package #:cl-markup)

(defun %markup-from-regexp (regexp string callback &optional plain-string-markup-fn)
  (flet ((markup-string (s)
           (if plain-string-markup-fn
               (funcall plain-string-markup-fn s)
               (list s))))
    (loop
       with length = (length string)
       with start = 0
       while (< start length)
       append (multiple-value-bind (match-start match-end reg-starts reg-ends)
                  (cl-ppcre:scan regexp string :start start)
                (if match-start
                    ;; Some highlighted text was found
                    (let* ((highlight (funcall callback reg-starts reg-ends))
                           (old-start start))
                      (setq start match-end)
                      (if (> match-start old-start)
                          ;; There is some unmatched text before the match
                          (append (markup-string (subseq string old-start match-start))
                                  (list highlight))
                          ;; ELSE: The match is at the beginning of the string
                          (list highlight)))
                    ;; ELSE: No match, copy the last part of the text and finish the loop
                    (progn
                      (let ((old-start start))
                        (setq start length)
                        (markup-string (subseq string old-start)))))))))

(defmacro markup-from-regexp (regexp string callback &optional plain-string-markup-fn &environment env)
  `(%markup-from-regexp ,(if (constantp regexp env) `(load-time-value (cl-ppcre:create-scanner ,regexp)) regexp)
                        ,string ,callback ,@(if plain-string-markup-fn (list plain-string-markup-fn))))

(defun markup-highlight (string)
  (markup-from-regexp "([*_`])(.+?)\\1" string
                      #'(lambda (reg-starts reg-ends)
                          (let ((type (subseq string (aref reg-starts 0) (aref reg-ends 0))))
                            (cons (string-case:string-case (type)
                                    ("*" :bold)
                                    ("_" :italics))
                                  (subseq string (aref reg-starts 1) (aref reg-ends 1)))))))

(defun markup-maths (string)
  ;; Maths needs to be extracted before anything else, since it can
  ;; contain a mix of pretty much every other character, and we don't
  ;; want that to mess up any other highlighting.
  (markup-from-regexp "((?:\\$\\$.+?\\$\\$)|(?:\\\\\\(.+?\\\\\\))|(?:`.+?`))" string
                      #'(lambda (reg-starts reg-ends)
                          (let* ((start (aref reg-starts 0))
                                 (end (aref reg-ends 0))
                                 (c1 (aref string start)))
                            (cond ((eql c1 #\$)
                                   (cons :math (subseq string (+ start 2) (- end 2))))
                                  ((eql c1 #\\)
                                   (cons :inline-math (subseq string (+ start 2) (- end 2))))
                                  ((eql c1 #\`)
                                   (cons :code (subseq string (+ start 1) (- end 1))))
                                  (t
                                   (error "Internal error, unexpected match. Probably broken regexp.")))))
                      #'markup-highlight))

(defun add-protocol-name-to-url (name)
  (if (cl-ppcre:scan "^https?://" name)
      name
      (format nil "http://~a" name)))

(defun markup-string (string &key allow-nl)
  (let ((result nil))
    (labels ((process-string (s)
               (process-regex-parts *url-pattern* s
                             #'(lambda (reg-starts reg-ends)
                                 (let* ((name (subseq s (aref reg-starts 0) (aref reg-ends 0)))
                                        (url (add-protocol-name-to-url name)))
                                   (push (list :url url name) result)))
                             #'(lambda (start end)
                                 (dolist (v (markup-maths (subseq s start end)))
                                   (push v result))))))
      (if allow-nl
          (loop
             for line in (split-sequence:split-sequence #\Newline string)
             for first = t then nil
             unless first
             do (push '(:newline) result)
             do (process-string line))
          (process-string string))
      (reverse result))))

(defun markup-paragraphs-inner (string &key allow-nl)
  (loop
     for v in (cl-ppcre:split "\\n{2,}" string)
     when (plusp (length v))
     collect (cons :paragraph (markup-string v :allow-nl allow-nl))))

(defun trim-blanks-and-newlines (s)
  (string-trim #.(format nil " ~c~c" #\Newline #\Return) s))

(defmacro when-trimmed-not-empty ((sym string) &body body)
  (let ((trimmed (gensym)))
    `(let ((,trimmed (trim-blanks-and-newlines ,string)))
       (when (plusp (length ,trimmed))
         (let ((,sym ,trimmed))
           ,@body)))))

(defun markup-codeblocks (string)
  (loop
     with result = nil
     with state = :normal
     with current-language = nil
     with length = (length string)
     with pos = 0
     while (< pos length)
     if (eq state :normal)
     do (multiple-value-bind (start end reg-starts reg-ends)
            (cl-ppcre:scan "(?ms)^```[ ]*([^\\n\\r ]*)[ ]*$" string :start pos)
          (if start
              (progn
                (when (> start pos)
                  (when-trimmed-not-empty (trimmed (subseq string pos start))
                    (push trimmed result)))
                (setq state :code)
                (let ((s (aref reg-starts 0))
                      (e (aref reg-ends 0)))
                  (setq current-language (if (> e s) (subseq string s e) nil)))
                (setq pos end))
              (progn
                (when-trimmed-not-empty (trimmed (subseq string pos length))
                  (push trimmed result))
                (setq pos length))))
     else if (eq state :code)
     do (multiple-value-bind (start end)
            (cl-ppcre:scan "(?ms)^```[ ]*$" string :start pos)
          (if start
              (progn
                (when (> start pos)
                  (when-trimmed-not-empty (trimmed (subseq string pos start))
                    (push (list (list :code-block current-language trimmed)) result)))
                (setq state :normal)
                (setq current-language nil)
                (setq pos end))
              (progn
                (when-trimmed-not-empty (trimmed (subseq string pos length))
                  (push trimmed result))
                (setq pos length))))
     finally (return (reverse result))))

(defun markup-paragraphs (string &key allow-nl)
  (loop
     for v in (markup-codeblocks string)
     append (if (stringp v)
                (markup-paragraphs-inner v :allow-nl allow-nl)
                v)))

(defun escape-string (string stream)
  (loop
     for c across string
     do (case c
          (#\& (write-string "&amp;" stream))
          (#\< (write-string "&lt;" stream))
          (#\> (write-string "&gt;" stream))
          (t   (write-char c stream)))))

(defun render-element-with-content (tag element stream)
  (write-string "<" stream)
  (write-string tag stream)
  (write-string ">" stream)
  (%render-markup-to-stream element stream)
  (write-string "</" stream)
  (write-string tag stream)
  (write-string ">" stream))

(defun render-math (element stream)
  (format stream "$$~a$$" element))

(defun render-inline-math (element stream)
  (format stream "\\(~a\\)" element))

(defun render-url (element stream)
  (let* ((url (first element))
         (description (or (second element) url)))
    (write-string "<a href=\"" stream)
    (escape-string-minimal-plus-quotes url stream)
    (write-string "\">" stream)
    (escape-string-minimal-plus-quotes description stream)
    (write-string "</a>" stream)))

(defun render-codeblock (element stream)
  (let ((type (if (first element)
                  (string-case:string-case ((string-downcase (first element)))
                                           ("lisp" :lisp)
                                           ("scheme" :scheme)
                                           ("clojure" :clojure)
                                           ("elisp" :elisp)
                                           ("common-lisp" :common-lisp)
                                           ("c" :c)
                                           ("c++" :c++)
                                           ("java" :java)
                                           ("objective-c" :objective-c)
                                           ("objc" :objective-c)
                                           ("erlang" :erlang)
                                           ("python" :python)
                                           ("haskell" :haskell)
                                           ("diff" :diff)
                                           ("webkit" :webkit)
                                           (t nil)))))
          (write-string "<pre class=\"highlighted-code\">" stream)
          (if type
              (write-string (colorize:html-colorization type (second element)) stream)
              (write-string (second element) stream))
          (write-string "</pre>" stream)))

(defun render-newline (stream)
  (write-string "<br>" stream))

(defun %render-markup-to-stream (content stream)
  (if (stringp content)
      (escape-string content stream)
      ;; ELSE: Not a string, so it will be treated as a list
      (dolist (element content)
        (etypecase element
          (string (escape-string element stream))
          (cons (let ((v (cdr element)))
                  (ecase (car element)
                    (:paragraph   (render-element-with-content "p" v stream))
                    (:bold        (render-element-with-content "b" v stream))
                    (:italics     (render-element-with-content "i" v stream))
                    (:code        (render-element-with-content "code" v stream))
                    (:math        (render-math v stream))
                    (:inline-math (render-inline-math v stream))
                    (:url         (render-url v stream))
                    (:code-block  (render-codeblock v stream))
                    (:newline     (render-newline stream)))))))))

(defun render-markup-to-stream (content stream)
  (%render-markup-to-stream content stream))

(defun render-markup (content)
  (with-output-to-string (s)
    (%render-markup-to-stream content s)))
