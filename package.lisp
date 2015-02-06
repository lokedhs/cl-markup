(defpackage #:cl-markup
  (:use #:cl)
  (:export #:render-markup-to-stream
           #:render-markup
           #:markup-string
           #:markup-paragraphs
           #:*custom-parser-1*
           #:*custom-parser-2*
           #:*custom-html-renderer*))
