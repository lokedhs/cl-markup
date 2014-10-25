(in-package #:cl-markup)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defparameter *math-render-fn* nil)
(defparameter *inline-math-render-fn* nil)

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
                                    ("_" :italics)
                                    ("`" :code))
                                  (subseq string (aref reg-starts 1) (aref reg-ends 1)))))))

(defun markup-maths (string)
  ;; Maths needs to be extracted before anything else, since it can
  ;; contain a mix of pretty much every other character, and we don't
  ;; want that to mess up any other highlighting.
  (markup-from-regexp "((?:\\$\\$.+?\\$\\$)|(?:\\\\\\(.+?\\\\\\)))" string
                      #'(lambda (reg-starts reg-ends)
                          (let* ((start (aref reg-starts 0))
                                 (end (aref reg-ends 0))
                                 (c1 (aref string start)))
                            (cond ((eql c1 #\$)
                                   (cons :math (subseq string (+ start 2) (- end 2))))
                                  ((eql c1 #\\)
                                   (cons :inline-math (subseq string (+ start 2) (- end 2))))
                                  (t
                                   (error "Internal error, unexpected match. Probably broken regexp.")))))
                      #'markup-highlight))

(defun markup-string (string)
  (markup-maths string))

(defun markup-paragraphs (string)
  (loop
     for v in (cl-ppcre:split "\\n{2,}" string)
     when (plusp (length v))
     collect (cons :paragraph (markup-string v))))

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
                    (:inline-math (render-inline-math v stream)))))))))

(defun render-markup-to-stream (content stream)
  (%render-markup-to-stream content stream))
