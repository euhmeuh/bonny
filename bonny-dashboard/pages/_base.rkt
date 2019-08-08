#lang racket/base

(provide
  base-page)

(require
  racket/string
  (only-in racket/date current-date)
  (only-in srfi/19 date->string)
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

(define (render-pirate-date)
  (define datetime (current-date))
  (define day (number->string (date-day datetime)))
  `((p
      "Yarr Captain! The sun shines over this blessed "
      (strong (time ([datetime ,(date->string datetime "~1")])
                    ,(date->string datetime
                                   (string-append
                                     "~A, the "
                                     day
                                     (cond [(string=? day "11") "th"]
                                           [(string=? day "12") "th"]
                                           [(string=? day "13") "th"]
                                           [(string-suffix? day "1") "st"]
                                           [(string-suffix? day "2") "nd"]
                                           [(string-suffix? day "3") "rd"]
                                           [else "th"])
                                     " of ~B in the year ~Y"))))
      ".")
    (p
      "I may add, according to the sky, that it is approximately "
      (strong ,(date->string datetime "~l ~p"))
      ", about "
      (strong ,(date->string datetime "~M minutes"))
      " past.")))

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
         ,@(render-pirate-date)
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
