#lang racket

(require racket/string
         threading
         
         pollen/decode
         pollen/tag
         pollen/unstable/typography
         
         txexpr)

(provide root $ $$ piecewise quick-table)


(define (root . elements)
  (~> elements
      (decode-elements
       #:txexpr-elements-proc decode-paragraphs
       #:string-proc smart-dashes
       #:exclude-tags '($ $$))))


(define (apply-math-replaces s)
  (~> s
      (string-replace "\n\n" " \\newline ")
      #|(string-replace "\\" "\\\\")|#))

(define ($ . xs)
  (apply-math-replaces
   (format "$~a$" (string-trim (apply string-append xs)))))

(define ($$ . xs)
  (apply-math-replaces
   (format "$$~a$$" (string-trim (apply string-append xs)))))


(define (piecewise . xs)
  (apply string-append `("\\left\\{\\begin{array}{l}\n"
                         ,@xs
                         "\n\\end{array}\\right.")))


(define (quick-table . tx-elements)
  (define rows-of-text-cells
    (let ([text-rows (filter-not whitespace? tx-elements)])
      (for/list ([text-row (in-list text-rows)])
                (for/list ([text-cell (in-list (string-split text-row "|"))])
                          (string-trim text-cell)))))
 
  (match-define (list tr-tag td-tag th-tag) (map default-tag-function '(tr td th)))
 
  (define html-rows
    (match-let ([(cons header-row other-rows) rows-of-text-cells])
      (cons (map th-tag header-row)
            (for/list ([row (in-list other-rows)])
                      (map td-tag row)))))
 
  (cons 'table (for/list ([html-row (in-list html-rows)])
                         (apply tr-tag html-row))))
