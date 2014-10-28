(in-package #:cl-markup)

(defun process-regex-parts (regexp string match-fn no-match-fn)
  (loop
     with length = (length string)
     with start = 0
     while (< start length)
     do (multiple-value-bind (match-start match-end reg-starts reg-ends)
                (cl-ppcre:scan regexp string :start start)
          (if match-start
              ;; Match found, possibly call the no-match function for the segment before the match
              (progn
                (when (> match-start start)
                  (funcall no-match-fn start match-start))
                (funcall match-fn reg-starts reg-ends)
                (setq start match-end))
              ;; ELSE: No match, call the no-match function for the last segment
              (progn
                (funcall no-match-fn start length)
                (setq start length))))))

(defun html-escape-string (string s test)
  (declare (type string string))
  (let ((first-pos (position-if test string)))
    (if (not first-pos)
        ;; nothing to do, just return STRING
        (write-sequence string s)
        ;; otherwise, process the string
        (loop with len = (length string)
           for old-pos = 0 then (1+ pos)
           for pos = first-pos
           then (position-if test string :start old-pos)
           ;; now the characters from OLD-POS to (excluding) POS
           ;; don't have to be escaped while the next character has to
           for char = (and pos (char string pos))
           while pos
           do (write-sequence string s :start old-pos :end pos)
             (case char
               ((#\<)
                (write-sequence "&lt;" s))
               ((#\>)
                (write-sequence "&gt;" s))
               ((#\&)
                (write-sequence "&amp;" s))
               ((#\')
                (write-sequence "&#039;" s))
               ((#\")
                (write-sequence "&quot;" s))
               (otherwise
                (format s "&#~d;" (char-code char))))
           while (< (1+ pos) len)
           finally (unless pos
                     (write-sequence string s :start old-pos))))))

(defun escape-string-minimal (string s)
  "Escape only #\<, #\>, and #\& in STRING."
  (html-escape-string string s #'(lambda (char) (find char "<>&"))))

(defun escape-string-minimal-plus-quotes (string s)
  "Like ESCAPE-STRING-MINIMAL but also escapes quotes."
  (html-escape-string string s #'(lambda (char) (find char "<>&'\""))))

(defun escape-string-all (string s)
  "Escapes all characters in STRING which aren't in the 7-bit ASCII
character set."
  (html-escape-string string s #'(lambda (char)
                                   (or (find char "<>&'\"")
                                       (> (char-code char) 127)))))
