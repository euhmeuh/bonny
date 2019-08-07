#lang racket/base

(provide
  base-page)

(require
  racket/string
  web-galaxy/entities)

(define basic-links
  (list
    (link "/" "Main desk")
    (link "/logs" "Captain's logbook")))

(define (render-navigation links)
  `(nav ([role "navigation"])
        (ul ,@(map (lambda (link)
                     `(li ,(render-element link)))
                   links))))

(define (render-title title)
  `(title ,(string-append title " | Bonny")))

(define (base-page title links renderer)
  `(html ([lang "en"])
     (head
       (meta ([charset "utf-8"]))
       (link ([rel "stylesheet"] [type "text/css"] [href "/common.css"]))
       (link ([rel "stylesheet"] [type "text/css"] [href "/datagrid.css"]))
       ,(render-title title))
     (body
       (header
         (h1 "☠ Bonny ☠")
         (p
           (small
             "Yarr Captain! The sun shines over this blessed "
             (strong (time ([datetime "2018-04-04"]) "Wednesday, the 4th of April in the year 2018"))
             ".")
           (br)
           (small
             "I may add, according to the sky, that it is approximately "
             (strong "14 o'clock")
             ", and about "
             (strong "18 minutes")
             " past."))
         ,(render-navigation (append basic-links links)))
       ,(renderer)
       (footer
         (p (small "Bonny is Free and Open Source software, "
                   "provided under the GNU General Public License v3.")
            (br)
            (small "Copyright © Jérôme Martin, All Rights Reserved")))
       (script ([src "_.js"]))
       (script ([src "page.js"]))
       (script ([src "request.js"]))
       (script ([src "datagrid.js"]))
       (script ([type "text/javascript"])
         #<<'''
         Page.onload(function() {
           const piratesDg = new Datagrid("piratesDatagrid");
           const tasksDg = new Datagrid("tasksDatagrid");
         });
'''
       ))))
