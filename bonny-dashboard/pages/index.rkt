#lang racket/base

(provide
  index-page)

(require
  "_base.rkt")

(define (index-page)
  (base-page "Main desk" '()
    (lambda ()
      `(main
         (h2 "Pirates")
         ; ,(datagrid
         ;    #:resource "/pirate"
         ;    (column #:name "Name" #:value name)
         ;    (column #:name "URL" #:value url)
         ;    (column #:name "Status" #:value status)
         ;    (column #:name "Actions"
         ;      (button "Start")
         ;      (button "Restart")
         ;      (button "Stop")
         ;      (button "Trails")
         ;      (button "Logs")
         ;      (button "Upgrade")
         ;      (button "Delete"))
         ;    (footer
         ;      (form ([action "/pirate"])
         ;        (input ([type "text"] [name "new-pirate-name"]))
         ;        (input ([type "text"] [name "new-pirate-url"]))
         ;        (button ([type "submit"]) "Add a pirate"))))
         ; ,(datagrid
         ;    #:resource "/task"
         ;    (column #:name "Description" #:value 'description)
         ;    (column #:name "Status" #:value 'status)
         ;    (column #:name "Actions" (button "See details"))
         ;    (footer
         ;      (a ([href "/logs"]) "See Captain's logs")))
         ))))
