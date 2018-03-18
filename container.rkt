#lang racket/base

(provide
  create-base-container
  clone-container)

(require
  "cascade.rkt"
  "machine.rkt"
  "pirate.rkt")

(define (get-image-dir name)
  (build-path "/var/lib/machines/" name))

(define (create-base-container [name "archlinux-base"])
  (define image-dir (get-image-dir name))
  (cascade
    (create-directory "/etc/systemd/nspawn")
    (create-directory image-dir)
    (install-base #:ignore '(linux)
                  #:add '(racket-minimal git)
                  image-dir)
    (delete-directory #:recursive? #t
                      (build-path image-dir "usr/share/locale"))
    (with-machine 'archlinux-base
      (add-user 'racket)
      (install-racket-pkg 'web-server-lib 'command-tree)
      (enable-service 'systemd-networkd)
      (clean-pacman-cache))))

(define (clone-container name [model "archlinux-base"])
  (define image-dir (get-image-dir name))
  (define pirate (find-pirate name))
  (define template-vars
    `([project ,name]
      [port ,(pirate-port pirate)]))
  (cascade
    (clone-machine model name)
    (setup-machine-id name)
    (deploy-template "{project}.service"
                     (build-path image-dir "usr/lib/systemd/system/")
                     template-vars)
    (deploy-template "{project}.nspawn" "/etc/systemd/nspawn/" template-vars)
    (enable-machine name)
    (start-machine name)
    (with-machine name #:user 'racket
      (git-clone (pirate-repo-url pirate))
      (make-install (pirate-repo-name pirate))
      (enable-service name)
      (start-service name))))
