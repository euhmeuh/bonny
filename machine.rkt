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
  stop-service
  enable-machine
  disable-machine
  start-machine
  stop-machine
  clone-machine
  setup-machine-id
  deploy-template
  git-clone
  make-install)

(require
  racket/system
  racket/string
  racket/function
  racket/list
  racket/format
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
  (call "machinectl status ~a" name))

(define (open-machine-shell name user)
  'todo)

(define (user-exists? name)
  (call "id -u ~a" name))

(define (file-exists? filepath)
  (call "test -f ~a" filepath))

(define (directory-exists? dir)
  (call "test -d ~a" dir))

(define (directory-empty? dir)
  (call "test -n \"$(find \"~a\" -maxdepth 0 -empty)\"" dir))

(define (apply-template string vars)
  (let loop ([str string]
             [vars vars])
    (if (pair? vars)
        (let* ([var (car vars)]
               [target (first var)]
               [replacement (second var)])
          (loop (string-replace str
                                (format "{~a}" target)
                                (~a replacement))
                (cdr vars)))
        str)))

(define (pkgs->string pkgs)
  (string-join (map symbol->string pkgs)))

(define-cascader (create-directory dir)
  #:description (format "Create directory '~a'" dir)
  #:unless (directory-exists? dir)
  (call "mkdir ~a" dir))

(define-cascader (delete-directory #:recursive? [recursive #f]
                                   dir)
  #:description (format "Delete directory '~a'" dir)
  #:unless (not (directory-exists? dir))
  (if recursive
      (call "rm -rf ~a" dir)
      (call "rmdir ~a" dir)))

(define-cascader (copy-file file dest)
  (call "cp ~a ~a" file dest))

(define-cascader (apply-template-to-file filepath vars)
  #:description (format "Apply template to file '~a'" filepath)
  #:fail (not (file-exists? filepath))
  #:fail-reason (format "The given file '~a' does not exist." filepath)
  (define sed-commands
    (string-join (map (lambda (var)
                        (format "s/~a/~a/g" (first var) (second var)))
                      vars)
                 "; "))
  (call "sed -i -- '~a' ~a" sed-commands filepath))

(define-cascader (install-base #:ignore [ignored-pkgs '()]
                               #:add [added-pkgs '()]
                               dir)
  #:description (format "Install base ArchLinux packages to '~a', ignoring ~a and adding ~a"
                        dir ignored-pkgs added-pkgs)
  #:fail (not (directory-empty? dir))
  #:fail-reason (format "The given directory '~a' should be empty." dir)

  (define commands (list "pacman -Sgq base"))
  (when (pair? ignored-pkgs)
    (set! commands
          (append commands
                  (list (format "grep -Fvx ~a" (pkgs->string ignored-pkgs))))))
  (call
    (string-join
      (append commands
              (list (format "pacstrap -c -G -M ~a ~a -"
                            dir
                            (pkgs->string added-pkgs))))
      " | ")))

(define-cascader (clean-pacman-cache)
  #:description "Clean pacman cache"
  (call "yes | pacman -Scc"))

(define-cascader (with-machine name #:user [user 'root] . cascaders)
  #:description (format "Run commands inside machine '~a'" name)
  #:fail (not (machine-exists? name))
  #:fail-reason (format "The given machine '~a' does not exist." name)
  (parameterize ([current-call-shell (open-machine-shell name user)])
    (if (not (current-call-shell))
        (cascade-fail "Unable to connect to machine '~a' with user '~a'" name user)
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

(define-cascader (enable-machine name)
  #:description (format "Enable systemd machine '~a'" name)
  #:fail (not (machine-exists? name))
  #:fail-reason (format "The given machine '~a' does not exist." name)
  (call "machinectl enable ~a" name))

(define-cascader (disable-machine name)
  #:description (format "Disable systemd machine '~a'" name)
  #:fail (not (machine-exists? name))
  #:fail-reason (format "The given machine '~a' does not exist." name)
  (call "machinectl disable ~a" name))

(define-cascader (start-machine name)
  #:description (format "Start systemd machine '~a'" name)
  #:fail (not (machine-exists? name))
  #:fail-reason (format "The given machine '~a' does not exist." name)
  (call "machinectl start ~a" name))

(define-cascader (stop-machine name)
  #:description (format "Stop systemd machine '~a'" name)
  #:fail (not (machine-exists? name))
  #:fail-reason (format "The given machine '~a' does not exist." name)
  (call "machinectl stop ~a" name))

(define-cascader (clone-machine model name)
  #:description (format "Clone machine '~a' to the new machine '~a'" model name)
  #:fail (or (machine-exists? name)
             (not (machine-exists? model)))
  #:fail-reason (format "The given machine '~a' already exists or model '~a' does not exist." name model)
  (call "machinectl clone ~a ~a" model name))

(define-cascader (setup-machine-id name)
  'todo)

(define-cascader (deploy-template filepath dest-dir vars)
  (define destination (build-path dest-dir (apply-template filepath vars)))
  (cascade
    (copy-file filepath destination)
    (apply-template-to-file destination vars)))

(define-cascader (git-clone url)
  (call "git clone ~a" url))

(define-cascader (make-install dir)
  (call #:dir dir "./make install"))
