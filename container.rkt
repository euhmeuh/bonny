#lang racket/base

(provide
  create-base-container)

(require
  "cascade.rkt")

(define (create-base-container)
  (define image-folder "/var/lib/machines/archlinux-base")
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

(define-cascader (create-directory dir)
  #:unless (folder-exists? dir)
  (call "mkdir ~a" dir))
