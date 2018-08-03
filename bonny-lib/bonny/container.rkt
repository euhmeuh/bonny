#lang racket/base

(provide
  install-and-configure-bonny
  create-base-container
  clone-container)

(require
  bonny/pirate
  bonny/cascade
  bonny/machine)

(define (find-pirate name)
  (pirate 'rilouw-website "git://github.com/euhmeuh/rilouw-website" "rilouw-website" 8001))

(define (install-and-configure-bonny dependencies)
  (cascade
    (apply install-racket-packages dependencies)
    (create-user "bonny")

    (create-directory machine-dir)
    (own-directory #:user "bonny"
                   #:group "bonny"
                   machine-dir)
    (reset-directory-grants #:user 'rw
                            #:group 'r
                            machine-dir)

    (create-directory machine-config-dir)
    (own-directory #:user "bonny"
                   #:group "bonny"
                   machine-config-dir)
    (reset-directory-grants #:user 'rw
                            #:group 'r
                            machine-config-dir)

    (grant-machinectl-rights "bonny")))

(define (create-base-container [name "archlinux-base"])
  (define machine-path (build-machine-path name))
  (cascade
    (create-directory machine-path)
    (install-base #:ignore '(linux)
                  #:add '(racket-minimal git)
                  machine-path)
    (delete-directory #:recursive? #t
                      (build-path machine-path "usr/share/locale"))
    (with-machine 'archlinux-base
      (create-user "racket")
      (install-racket-packages 'web-server-lib 'command-tree)
      (enable-service 'systemd-networkd)
      (clean-pacman-cache))))

(define (clone-container name [model "archlinux-base"])
  (define machine-path (build-machine-path name))
  (define pirate (or (find-pirate name)
                     (raise-user-error 'pirate-not-found
                                       "The given pirate '~a' was not found" name)))
  (define template-vars
    `([project ,name]
      [port ,(pirate-port pirate)]))
  (cascade
    (clone-machine model name)
    (setup-machine-id name)
    (deploy-template "{project}.service"
                     (build-path machine-path "usr/lib/systemd/system/")
                     template-vars)
    (deploy-template "{project}.nspawn" machine-config-dir template-vars)
    (enable-machine name)
    (start-machine name)
    (with-machine name #:user "racket"
      (git-clone (pirate-repo-url pirate))
      (make-install (pirate-repo-name pirate))
      (enable-service name)
      (start-service name))))
