#lang racket/base

(provide
  index-page)

(require
  bonny/pirate
  "_base.rkt")

(define (index-page)
  (base-page "Main desk" '()
    (lambda ()
      `(main
         (h2 "Your crew")
         (bonny-datagrid ([id "piratesDatagrid"]
                          [class "w-100"]
                          [data-resource "http://localhost:8000/pirate"]
                          [data-page-size "5"]
                          [data-page-sizes "[5, 20, 50, 100, 300]"])
           (bonny-header "Pirates")
           (bonny-header
             (div ([class "flex flex-flow-wrap"])
               (div
                 (label "Filter:")
                 (bonny-filter-adder))
               (div ([class "flex1 right"])
                 (label "Showing at least")
                 (bonny-page-size-selector) "out of " (strong (bonny-result-count)) " results.")))
           (bonny-header
             (label "Current filters:")
             (bonny-filter-list))
           (bonny-placeholder "It's rather empty 'round here...")
           (bonny-loader
             (div ([class "center mt-5 mb-5"])
               "... Trying to find pirates ..."))
           (bonny-column ([data-name "Name"] [data-value "name"] [data-sortable ""] [data-filterable ""]))
           (bonny-column ([data-name "URL"] [data-value "url"] [data-sortable ""] [data-filterable ""]))
           (bonny-column ([data-name "Status"]
                          [data-value "status"]
                          [data-sortable ""]
                          [data-filterable ""]
                          [data-type "['STOPPED', 'STARTING', 'STARTED', 'STOPPING']"])
             (span ([class "chip chip-{status}"]) "{status}"))
           (bonny-column ([data-name "Actions"])
            (a ([class "btn"] [href "/trails/{id}"]) "Trails")
            (a ([class "btn"] [href "/logs/{id}"]) "Logs"))
           (bonny-footer
             (form ([action "http://localhost:8000/pirate"] [method "POST"])
               (input ([type "text"] [name "name"] [placeholder "Name"]))
               (input ([type "text"] [name "url"] [placeholder "URL"]))
               (button ([type "submit"]) "Add a pirate")
             ))
           (bonny-footer (bonny-pagination)))
         (h2 "What's goin' on?")
         (bonny-datagrid ([id "tasksDatagrid"]
                          [class "w-100"]
                          [data-resource "http://localhost:8000/task"]
                          [data-page-size "20"])
           (bonny-header "Tasks")
           (bonny-placeholder "It's rather empty 'round here...")
           (bonny-loader
             (div ([class "center mt-5 mb-5"])
               "... Trying to find tasks ..."))
           (bonny-column ([data-name "Date"] [data-value "date"] [data-sortable ""]))
           (bonny-column ([data-name "Description"] [data-value "description"] [data-sortable ""]))
           (bonny-column ([data-name "Status"]
                          [data-value "status"]
                          [data-sortable ""]
                          [data-type "['STOPPED', 'STARTING', 'STARTED', 'STOPPING']"])
             (span ([class "chip chip-green"]) "{status}"))
           (bonny-column ([data-name "Actions"])
            (a ([class "btn"] [href "/task/{id}"]) "Details"))
           (bonny-footer (a ([href "/logs"]) "See Captain's logs"))
           (bonny-footer (bonny-pagination)))
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
