(in-package #:cl-markup)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defparameter *math-render-fn* nil)

(defun markup-string (string)
  (loop
     with length = (length string)
     with start = 0
     while (< start length)
     append (multiple-value-bind (match-start match-end reg-starts reg-ends)
                (cl-ppcre:scan "([*_`]|\\$\\$)(.+?)\\1" string :start start)
              (if match-start
                  ;; Some highlighted text was found
                  (let* ((type (subseq string (aref reg-starts 0) (aref reg-ends 0)))
                         (highlight (cons (string-case:string-case (type)
                                            ("*" :bold)
                                            ("_" :italics)
                                            ("`" :code)
                                            ("$$" :math))
                                          (subseq string (aref reg-starts 1) (aref reg-ends 1)))))
                    (let ((old-start start))
                      (setq start match-end)
                      (if (> match-start old-start)
                          ;; There is some unmatched text before the match
                          (list (subseq string old-start match-start)
                                highlight)
                          ;; ELSE: The match is at the beginning of the string
                          (list highlight))))
                  ;; ELSE: No match, copy the last part of the text and finish the loop
                  (progn
                    (let ((old-start start))
                      (setq start length)
                      (list (subseq string old-start))))))))

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

(defun %render-markup-to-stream (content stream)
  (if (stringp content)
      (escape-string content stream)
      ;; ELSE: Not a string, so it will be treated as a list
      (dolist (element content)
        (etypecase element
          (string (escape-string element stream))
          (cons (ecase (car element)
                  (:paragraph (render-element-with-content "p" (cdr element) stream))
                  (:bold (render-element-with-content "b" (cdr element) stream))
                  (:italics (render-element-with-content "i" (cdr element) stream))
                  (:code (render-element-with-content "code" (cdr element) stream))
                  (:math (funcall *math-render-fn* (cdr element) stream))))))))

(defun render-markup-to-stream (content stream &key (math-render-fn #'render-math))
  (let ((*math-render-fn* math-render-fn))
    (%render-markup-to-stream content stream)))
