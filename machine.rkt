#lang racket/base

(provide
  current-call-shell
  call
  machine-exists?
  open-machine-shell
  
  ;; cascaders
  create-directory
  delete-directory
  install-base
  clean-pacman-cache
  with-machine
  add-user
  install-racket-pkg
  enable-service
  disable-service
  start-service
  stop-service)

(require
  racket/system
  "cascade.rkt")

(define current-call-shell (make-parameter #f))

(define (call #:dir [dir #f]
              command . args)
  (if (current-call-shell)
      (send (current-call-shell) call #:dir dir command args)
      (parameterize ([current-directory (or dir (current-directory))])
        (define command (apply format (cons command args)))
        (displayln command)
        (system command #:set-pwd? #t))))

(define (machine-exists? name)
  'todo)

(define (open-machine-shell name)
  'todo)

(define-cascader (create-directory dir)
  #:description (format "Create directory '~a'" dir)
  #:unless (folder-exists? dir)
  (call "mkdir ~a" dir))

(define-cascader (delete-directory #:recursive? [recursive #f]
                                   dir)
  #:description (format "Delete directory '~a'" dir)
  #:unless (not (folder-exists? dir))
  (if recursive
      (call "rm -rf ~a" dir)
      (call "rmdir ~a" dir)))

(define-cascader (install-base #:ignore [ignored-pkgs '()]
                               #:add [added-pkgs '()]
                               dir)
  #:description (format "Install base ArchLinux packages to '~a', ignoring ~a and adding ~a"
                        dir ignored-pkgs added-pkgs)
  #:fail (not (folder-empty? dir))
  #:fail-reason (format "The given folder '~a' should be empty." dir)

  (define commands '("pacman -Sgq base"))
  (when (pair? ignored-pkgs)
    (set! commands
          (append commands
                  (format "grep -Fvx ~a" (pkgs->string ignored-pkgs)))))
  (call
    (string-join
      (append commands
              (format "pacstrap -c -G -M ~a ~a -"
                      dir
                      (pkgs->string added-pkgs))
      " | "))))

(define-cascader (clean-pacman-cache)
  #:description "Clean pacman cache"
  (call "yes | pacman -Scc"))

(define-cascader (with-machine name . cascaders)
  #:description (format "Run commands inside machine '~a'" name)
  #:fail (not (machine-exists? name))
  #:fail-reason (format "The given machine '~a' does not exist." name)
  (parameterize ([current-call-shell (open-machine-shell name)])
    (if (not (current-call-shell))
        (cascade-fail "Unable to connect to machine '~a'" name)
        (apply cascade cascaders))))

(define-cascader (add-user name)
  #:description (format "Add user '~a'" name)
  #:unless (user-exists? name)
  (call "useradd -m -U -s /usr/bin/nologin ~a" name))

(define-cascader (install-racket-pkg . pkgs)
  #:description (format "Install Racket packages ~a" pkgs)
  (call "raco pkg install -i --auto --skip-installed --binary-lib ~a" (pkgs->string pkgs)))

(define-cascader (enable-service name)
  #:description (format "Enable systemd service '~a'" name)
  (call "systemctl enable ~a" name))

(define-cascader (disable-service name)
  #:description (format "Disable systemd service '~a'" name)
  (call "systemctl disable ~a" name))

(define-cascader (start-service name)
  #:description (format "Start systemd service '~a'" name)
  (call "systemctl start ~a" name))

(define-cascader (stop-service name)
  #:description (format "Stop systemd service '~a'" name)
  (call "systemctl stop ~a" name))

(define (user-exists? name)
  (call "user ~a" name))

(define (pkgs->string pkgs)
    (string-join (map symbol->string pkgs)))
