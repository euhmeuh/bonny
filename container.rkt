#lang racket/base

(provide
  create-base-container
  clone-container)

(require
  "cascade.rkt"
  "machine.rkt")

(define (create-base-container [name "archlinux-base"])
  (define image-folder (build-path "/var/lib/machines/" name))
  (cascade
    (create-directory "/etc/systemd/nspawn")
    (create-directory image-folder)
    (install-base #:ignore '(linux)
                  #:add '(racket-minimal git)
                  image-folder)
    (delete-directory #:recursive? #t
                      (build-path image-folder "/usr/share/locale"))
    (with-machine 'archlinux-base
      (add-user 'racket)
      (install-racket-pkg 'web-server-lib 'command-tree)
      (enable-service 'systemd-networkd)
      (clean-pacman-cache))))

(define (clone-container name [model "archlinux-base"])
  (call "machinectl clone ~a ~a" model name))
