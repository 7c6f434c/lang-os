(defpackage :lisp-os-helpers/subuser
  (:use :common-lisp
	:lisp-os-helpers/shell :lisp-os-helpers/global-sqlite
        :lisp-os-helpers/timestamp :lisp-os-helpers/util)
  (:export
    #:subuser-uid
    #:select-subuser
    #:drop-subuser
    #:slay-subuser
    #:chown-subuser
    #:run-as-subuser
    #:nsjail-mount-allowed-p
    #:add-command-numeric-su
    #:add-command-simulate-setuid
    ))
(in-package :lisp-os-helpers/subuser)

(defvar *subuser-uid-shift* 100000)
(defvar *nogroup* 65534)
(defvar *numeric-su-helper* "/run/current-system/sw/bin/numeric-su")
(defvar *simulate-setuid-helper* "/run/current-system/sw/bin/simulate-setuid")
(defvar *nsjail-helper* "/run/current-system/sw/bin/nsjail")
(defvar *socat-helper* "/run/current-system/sw/bin/socat")
(defvar *unshare-helper* "/run/current-system/sw/bin/unshare")

(defun select-subuser (user &key uid name)
  (unless (or uid name) (error "Subuser selection requires either name or uid"))
  (unless user (error "Subuser selection requires owner"))
  (with-global-sqlite
    ()
    (complex-global-value
      :subusers "" 
      `(:owner :home :uid :gid)
      `(:varchar :varchar :integer :integer)
      ))
  (let*
    ((name (or (and name (format nil "~a.~a" user name))
	       (and uid
		    (first
		      (with-global-sqlite
			()
			(complex-global-value-by-id
			  :subusers (- uid *subuser-uid-shift*) '(:key)))))
	       (iolib/syscalls:getpwuid uid)))
     (name-uid
       (with-global-sqlite
	 ()
	 (first
	   (complex-global-value :subusers name '(:uid) nil))))
     (uid (or
	    uid
	    (third (multiple-value-list (iolib/syscalls:getpwnam name)))
	    name-uid))
     (name-suffix (and name (subseq name (1+ (length user)))))
     (owner (with-global-sqlite 
	      ()
	      (first
		(complex-global-value
		  :subusers name '(:owner) nil))))
     (passwd-line (and uid (multiple-value-list (iolib/syscalls:getpwuid uid)))))
    (when (and uid name)
      (unless
	(equal user owner)
	(error "Subuser owner is ~a instead of ~a" owner user))
      (unless
	(equal name (format nil "~a.~a" user name-suffix))
	(error "Subuser name mismatch: ~a is not ~a.~a"
	       name user name-suffix))
      (when
	(and name-uid (not (equal uid name-uid)))
	(error "Subuser name mismatch for ~a: ~a is not ~a"
	       name uid name-uid))
      (assert
	(equal
	  passwd-line
	  (multiple-value-list (iolib/syscalls:getpwnam name))))
      (values
	uid name passwd-line name-suffix owner))))

(defun subuser-uid (user &key name home gid group passwd-entry)
  (let*
    ((name-suffix (or name (timestamp-usec-recent-base36)))
     (name (format nil "~a.~a" user name-suffix))
     (home (or home "/var/empty"))
     (gid (or
            gid 
            (and
              group
              (third (multiple-value-list (iolib/syscalls:getgrnam group))))
            *nogroup*))
     (uid
       (or
         (select-subuser user :name name)
         (if passwd-entry
           (progn
             (uiop:run-program
               `("/run/current-system/bin/system-useradd"
                 "-d" ,home
                 "-g" ,(format nil "~a" gid) ,name))
             (select-subuser user :name name-suffix))
           (with-global-sqlite
             ()
             (complex-global-value 
               :subusers name
               `(:owner :home :uid :gid)
               `(:varchar :varchar :integer :integer)
               (list user home nil gid) t)
             (+ *subuser-uid-shift*
                (first (complex-global-value :subusers name `(:id) nil)))
             ))))
     )
    (with-global-sqlite
      ()
      (complex-global-value 
        :subusers name
        `(:owner :home :uid :gid) `(:varchar :varchar :integer :integer)
        (list user home uid gid))
      (values uid name-suffix))))

(defun slay-subuser (user &key uid name)
  (multiple-value-bind
    (uid)
    (select-subuser user :uid uid :name name)
    (when uid
      (uiop:run-program
        `("pkill" "-U" ,(format nil "~a" uid) "-KILL")
        :ignore-error-status t)
      (uiop:run-program
        `("pkill" "-u" ,(format nil "~a" uid) "-KILL")
        :ignore-error-status t)
      )))

(defun drop-subuser (user &key uid name slay)
  (multiple-value-bind
    (uid name passwd-line)
    (select-subuser user :uid uid :name name)
    (and slay uid (slay-subuser user :uid uid))
    (when passwd-line
      (uiop:run-program
	`("/run/current-system/bin/system-userdel" ,name)))))

(defun-weak extra-owned-locations (user) (declare (ignorable user)) nil)

(defun chown-subuser(user file &key uid name recursive)
  (let*
    ((self (not (or uid name)))
     (subuser-uid (unless self (select-subuser user :uid uid :name name)))
     (passwd-line (multiple-value-list (iolib/syscalls:getpwnam user)))
     (home (sixth passwd-line))
     (in-home-p
       (find-if
         (lambda (d) (alexandria:starts-with-subseq
                       (cl-ppcre:regex-replace "/*$" d "/") file))
         (cons home (extra-owned-locations user))))
     (gid (fourth passwd-line))
     (user-uid (third passwd-line)))
    (unless in-home-p (error "File ~s is not in home of user ~s" file user))
    (unless (or self subuser-uid)
      (error "Could not select subuser by uid ~s and name ~s for user ~s"
             uid name user))
    (uiop:run-program
      `("chown" "-h" ,@(when recursive `("-R"))
        ,(format nil "~a:~a" (if self user-uid subuser-uid) gid)
        ,file))))

(defun add-command-numeric-su (command uid &key (gid 65534))
  `(,*numeric-su-helper*
     ,(format nil "~a" uid)
     ,(format nil "~a" gid)
     ,@ command))

(defun add-command-simulate-setuid (command uid &key gid)
  (assert uid)
  `(,*simulate-setuid-helper*
     ,(format nil "~a" uid)
     ,(format nil "~a"
              (or gid
                  (fourth (multiple-value-list
                            (iolib/syscalls:getpwuid
                              (parse-integer (format nil "~a" uid)))))))
     ,@ command))

(defun-weak
  nsjail-mount-allowed-p (from to type)
  (declare (ignorable from to type))
  (or
    (and
      (or
        (alexandria:starts-with-subseq "/home/" from)
        (alexandria:starts-with-subseq "/tmp/" from)
        )
      (or
        (alexandria:starts-with-subseq "/home/" to)
        (alexandria:starts-with-subseq "/tmp/" to)
        ))
    (and
      (alexandria:starts-with-subseq "/dev/" from)
      (find type '("B" "R") :test 'equal)
      (equal from to))
    ))

(defun add-command-nsjail
  (command uid &key
           verbose
	   (gid 65534) (network nil) hostname
	   mounts skip-default-mounts
	   (proc-rw t)
	   (internal-uid uid) (internal-gid gid)
           fake-passwd fake-groups fake-usernames
           skip-mount-check
	   full-dev writeable-dev dev-log-socket
           (home (format nil "/tmp/home.~a" uid) homep)
	   (rlimit-as "max") (rlimit-core "0") (rlimit-cpu "max")
	   (rlimit-fsize "max") (rlimit-nofile "max")
	   (rlimit-nproc "max") (rlimit-stack "16384")
           keep-namespaces
           enable-newprivs
           (directory "/")
           (resolv-conf "/etc/resolv.conf")
           (machine-id "/etc/machine-id")
	   )
  `(,*nsjail-helper*
     ,@(unless verbose `("-q"))
     "-u" , (format nil "~a:~a" internal-uid uid)
     "--rlimit_as"     ,rlimit-as
     "--rlimit_core"   ,rlimit-core
     "--rlimit_cpu"    ,rlimit-cpu
     "--rlimit_fsize"  ,rlimit-fsize
     "--rlimit_nofile" ,rlimit-nofile
     "--rlimit_nproc"  ,rlimit-nproc
     "--rlimit_stack"  ,rlimit-stack
     "-D" ,directory
     ,@(loop for ns in keep-namespaces
             do (assert (cl-ppcre:scan "^[a-z_]+$" ns))
             collect
             (format nil "--disable_clone_new~a" ns))
     ,@(when enable-newprivs `("--disable_no_new_privs"))
     ,@(when gid `("-g" ,(format nil "~a:~a" internal-gid gid)))
     ,@(unless skip-default-mounts
	 `(
	   "-R" "/etc/ssl"
           "-R" ,(progn
                   (uiop:run-program (add-command-numeric-su
                                       (list "stat" resolv-conf) uid))
                   (format nil "~a:/etc/resolv.conf" resolv-conf))
           "-R" ,(format nil "~a:/etc/machine-id" machine-id)
	   "-T" "/tmp"
           ,@(when writeable-dev `("-T" "/dev"))
           ,@(when dev-log-socket
               (when (equal dev-log-socket "")
                 (let* ((tmpdir
                          (uiop:run-program
                            "mktemp -d -p /tmp/subuser-homes/"
                            :output '(:string :stripped t)))
                        (socket (format nil "~a/log-socket" tmpdir)))
                   (uiop:run-program
                     (list "chown" (format nil "~a" uid) tmpdir))
                   (uiop:launch-program
                     (add-command-numeric-su
                       `("socat"
                         ,(format nil "unix-listen:~a,mode=0666,fork" socket)
                         "stdio")
                       uid))
                   (setf dev-log-socket socket)))
               (uiop:run-program
                 (add-command-numeric-su
                   (list "test" "-w" dev-log-socket) uid))
               `("-B" ,(format nil "~a:/dev/log" dev-log-socket)))
	   ,@(unless full-dev
	       `("-B" "/dev/null" "-B" "/dev/full" "-B" "/dev/zero"
		 "-B" "/dev/random" "-B" "/dev/urandom"
		 "-B" "/dev/shm"
		 ))
	   "-R" "/bin" "-R" "/usr" "-R" "/nix/store"
	   "-R" "/var/current-system" "-R" "/run/current-system"
	   "-R" "/etc/fonts"
           "-T" ,(format nil "/run/user/~a/" internal-uid)
           ,@(when home `(,(if homep "-B" "-T") ,home))
	   ))
     ,@(unless (or skip-default-mounts
                   (not (probe-file "/run/opengl-driver/"))
                   (not (probe-file "/run/opengl-driver-32/")))
         `(
           "-R" "/run/opengl-driver/"
           "-R" "/run/opengl-driver-32/"
           ))
     ,@(when full-dev `("-B" "/dev/" "-B" "/dev/shm"))
     ,@(when hostname `("-H" ,hostname))
     ,@(when fake-passwd
	 (ensure-directories-exist "/tmp/system-lisp/subuser-passwd/")
	 (with-open-file 
	   (f (format nil 
		      "/tmp/system-lisp/subuser-passwd/~a" uid)
	      :direction :output :if-exists :supersede)
	   (format f "root:x:0:0::/:/bin/sh~%")
	   (when home
             (format f ".~a:x:~a:~a::~a:/bin/sh~%" internal-uid internal-uid gid home))
           (loop for u in fake-usernames do
                 (format f "~a:x:~a:~a::~a:/bin/sh~%" u internal-uid gid "/"))
	   (format f ".~a:x:~a:~a::/:/bin/sh~%" 65534 65534 65534)
	   )
	 (list "-R" (format nil "/tmp/system-lisp/subuser-passwd/~a:/etc/passwd" uid)))
     ,@(when fake-groups
	 (ensure-directories-exist "/tmp/system-lisp/subuser-passwd/")
	 (with-open-file 
	   (f (format nil 
		      "/tmp/system-lisp/subuser-passwd/~a.group" uid)
	      :direction :output :if-exists :supersede)
	   (format f "root:x:0:~%")
	   (format f ".~a:x:~a:~%" internal-uid gid)
	   (format f "~{~{~a:x:~a:~}~%~}" (if (listp fake-groups) 
                                    (loop for g in fake-groups
                                          if (listp g) collect g
                                          else collect (list g 65534))
                                    (list)))
	   )
	 (list "-R" (format nil "/tmp/system-lisp/subuser-passwd/~a.group:/etc/group" uid)))
     ,@(loop
	 for m in mounts
	 for type := (subseq (reverse (string-upcase (first m))) 0 1)
	 for target := (second m)
	 for internal-target := (or (third m) target)
	 for target-reference := (if (equalp type "T") target
				   (format nil "~a:~a" target internal-target))
	 for type-known := (find type '("B" "R" "T") :test 'equal)
         do (uiop:run-program (add-command-numeric-su (list "stat" target) uid))
	 unless (or
		  skip-mount-check 
		  (nsjail-mount-allowed-p target internal-target type))
	 do (error "Forbidden mount for nsjail: ~s on ~s with type ~s"
		   target internal-target type)
         unless type-known
         do (error "Unknown mount type: ~a" type)
	 collect (concatenate 'string "-" type)
	 collect target-reference)
     ,@(when proc-rw `("--proc_rw"))
     ,@(when network `("-N"))
     "-E" "PATH="
     ,@(when home `("-E" ,(format nil "HOME=~a" home)))
     "--"
     ,@ command))

(defun socat-passthrough-commands (ports tmpdir)
  (loop
    with listen-commands := nil
    with connect-commands := nil
    for p in ports
    for n upfrom 1
    for nt := (format nil "~6,'0d" n)
    for lp := (first p)
    for lhost := (or (second p) "127.0.0.1")
    for cp := (or (third p) lp)
    for chost := (or (fourth p) lhost)
    for lpn := (first lp)
    for lpp := (or (second lp) :tcp)
    for cpn := (first cp)
    for cpp := (or (second cp) lpp :tcp)
    for socket := (format nil "~a/~a" tmpdir nt)
    for listen := (cond 
                    ((equalp (string lpp) "tcp")
                     (format nil "tcp-listen:~a,forever,bind=~a,reuseaddr" lpn lhost))
                    ((equalp (string lpp) "udp")
                     (format nil "udp-listen:~a,bind=~a,reuseaddr" lpn lhost))
                    ((equalp (string lpp) "unix")
                     (format nil "unix-listen:~a,mode=~a,forever" lpn (or (third lp) "0666")))
                    (t (error "Unknown protocol: ~a" lpp)))
    for connect := (cond
                     ((equalp (string cpp) "tcp")
                      (format nil "tcp-connect:~a:~a~{,~a~}" chost cpn (third cp)))
                     ((equalp (string cpp) "udp")
                      (format nil "udp-sendto:~a:~a~{,~a~}" chost cpn (third cp)))
                     ((equalp (string cpp) "unix")
                      (format nil "unix-connect:~a~{,~a~}" cpn (third cp)))
                     (t (error "Unknown protocol: ~a" cpp)))
    for listen-command :=
    (list "socat" (format nil "~a,fork" listen)
          (format nil "unix-connect:~a" socket))
    for connect-command :=
    (list "socat" 
          (format nil "unix-listen:~a,forever,fork" socket)
          connect)
    do (push connect-command connect-commands)
    do (push listen-command listen-commands)
    finally (return (list connect-commands listen-commands))))

(defun add-command-netns (command &key ports-out ports-in
                                  uid gid (directory "/")
				  (path "/var/current-system/sw/bin")
                                  tuntap-devices
                                  hostname verbose)
  (ensure-directories-exist "/tmp/subuser-homes/")
  (let* 
    ((*print-right-margin* (expt 10 9))
     (tmpdir (uiop:run-program "mktemp -d -p /tmp/subuser-homes/"
                               :output '(:string :stripped t)))
     (mkdir-command (list "mkdir" "-p" tmpdir))
     (clean-dir-command (list "rm" "-rf" tmpdir))
     (socat-commands-out (socat-passthrough-commands ports-out tmpdir))
     (socat-commands-in (socat-passthrough-commands ports-in tmpdir))
     (tuntap-commands
       (append
         (loop for d in tuntap-devices
               for name = (first d)
               for address = (second d)
               for mode = (or (third d) :tap)
               for options = (fourth d)
               for version = (if (find "6" options :test 'equal)
                                 "-6" "-4")
               collect
               `("ip" "tuntap" "add"
                 "mode" ,(string-downcase mode)
                 "name" ,name
                 ,@(loop for o in options
                         when (find o '(:pi :vnet_hdr :one_queue :multi_queue)
                                    :key 'string :test 'equalp)
                         collect (string-downcase o)))
               collect
               `("sh" "-c" ,(format nil "while ! ip link show ~a > /dev/null; do sleep 0.1; done" name))
               collect
               `("ip" "link" "set" ,name "up")
               collect
               `("ip" ,version "addr" "add" ,address "dev" ,name
                 ,@(when (equal version "-6") `("nodad")))
               when verbose collect `("ip" "address" "show")
               when verbose collect `("ip" "link" "show")
               )))
     (outside-commands (append (first socat-commands-out) (second socat-commands-in)))
     (inside-commands (append (second socat-commands-out)
                              (first socat-commands-in)))
     (inside-commands-pre (append tuntap-commands))
     (inner-unshare `(, *nsjail-helper*
                       ,@(unless verbose `("-Q"))
                      "-e" "-c" "/"
                      "-u" ,(format nil "~a:0" uid)
                      "-g" ,(format nil "~a:0" gid)
                      "-D" ,directory
                      ,@(when hostname `("-H" ,hostname))
                      "--keep_caps"
                      "--proc_rw"
                      "--disable_clone_newnet"
                      "--disable_clone_newipc"
                      "--disable_clone_newuts"
                      "--rlimit_as"     "max"
                      "--rlimit_core"   "max"
                      "--rlimit_cpu"    "max"
                      "--rlimit_fsize"  "max"
                      "--rlimit_nofile" "max"
                      "--rlimit_nproc"  "max"
                      "--rlimit_stack"  "max"
                      "--disable_no_new_privs"
                      "--"
                      ,@ command))
     (inner-setup
       (list
	 "/bin/sh" "-c"
	 (format
	   nil "~{ ~a ; ~}~{ ~a & ~} sleep 0.3; mkdir -p \"$HOME\"; cd; ~a; exit_value=$?; pkill -INT -P $$; exit $exit_value"
	   (mapcar 'collapse-command inside-commands-pre)
	   (mapcar 'collapse-command inside-commands)
	   (collapse-command inner-unshare))))
     ;(outer-unshare `("unshare" "-U" "-r" "-n" ,@ inner-setup))
     (outer-unshare `(
                      , *nsjail-helper*
                      ,@(unless verbose `("-Q"))
                      "-e" "-c" "/"
                      "-u" ,(format nil "0:~a" uid)
                      "-g" ,(format nil "0:~a" gid)
                      "-D" ,directory
                      "--keep_caps"
                      "--disable_clone_newuts"
                      "--disable_clone_newipc"
                      "--disable_no_new_privs"
                      "--proc_rw"
                      "--rlimit_as"     "max"
                      "--rlimit_core"   "max"
                      "--rlimit_cpu"    "max"
                      "--rlimit_fsize"  "max"
                      "--rlimit_nofile" "max"
                      "--rlimit_nproc"  "max"
                      "--rlimit_stack"  "max"
                      "--"
                      ,@ inner-setup
                      ))
     (result
       (list
	 "/bin/sh" "-c"
	 (format nil "test -n \"$PATH\" || export PATH=~a; ~a; ~{~a & ~} ~a; exit_value=$?; ~a; pkill -INT -P $$; exit $exit_value"
		 (escape-for-shell path)
		 (collapse-command mkdir-command)
		 (mapcar 'collapse-command outside-commands)
		 (collapse-command outer-unshare)
		 (collapse-command clean-dir-command)))))
    (iolib/syscalls:lchown tmpdir uid gid)
    result))

(defun prepare-basic-chroot (&optional (target "/run/basic-chroot/"))
  (loop for dir in `("nix" "sys" "proc" "etc" "bin" "usr" "dev"
                     "run/current-system" "var/current-system"
                     "var/empty"
                     "tmp/.X11-unix")
        do (unless (directory (format nil "~a/~a/*.*" target dir))
             (ensure-directories-exist (format nil "~a/~a/" target dir))
             (uiop:run-program `("mount" "--rbind"
                                 ,(format nil "/~a/" dir)
                                 ,(format nil "~a/~a/" target dir)))))
  (uiop:run-program (list "chmod" "a+rwxt" (format nil "~a/tmp/" target)))
  target)

(defun add-command-chroot (command target)
  (list "/bin/sh" "-c" (format nil "chroot ~a ~a" target (collapse-command command))))

(defun add-command-masking-mounts (command target)
  (let* ((root-entries
           (cl-ppcre:split
             (string #\Newline)
             (uiop:run-program `("ls" "-a" "/")
                               :output '(string :stripped t))))
         (make-mount
           (lambda (name)
             (cond ((not (probe-file (format nil "/~a/." name)))
                    (list "mount" "--bind" "/dev/null" (format nil "/~a" name)))
                   ((find name `("dev" "var") :test 'equal) nil)
                   ((probe-file (format nil "~a/~a" target name))
                    (list "mount" "--rbind" (format nil "~a/~a" target name)
                          (format nil "/~a" name)))
                   (t (list "mount" "--bind" "/var/empty"
                            (format nil "/~a" name)))))))))

(defun add-command-mounts (command mounts)
  (let* ((flat-command (collapse-command command))
         (mount-commands 
           (loop 
             for (source target . options) in mounts
             for tmpfsp := (find :tmpfs options)
             for create-file-p := (find :create-file options)
             for create-directory-p := (find :create-directory options)
             for bind-switch := (if (find :recursive options) "--rbind" "--bind")
             when create-file-p collect (list "touch" target)
             when create-directory-p collect (list "mkdir" "-p" target)
             when tmpfsp 
             collect (list "mount" "-t" "tmpfs" source target)
             unless tmpfsp
             collect (list "mount" bind-switch source target)
             when (find :read-only options)
             collect (list "mount" "-o" "bind,remount,ro" target)))
         (flat-mount-commands (mapcar 'collapse-command mount-commands))
         (inner-command
           (list "/bin/sh" "-c" (format nil "~{~a ; ~} ~a"
                                   flat-mount-commands flat-command)))
         (unshare-command 
           `(,*unshare-helper* "-m" ,@ inner-command)))
    unshare-command))

(defun run-as-subuser (user command &key uid (gid 65534) name environment
			    stdin-fd stdout-fd stderr-fd
			    pty wait slurp-stdout slurp-stderr
			    feed-stdin slay
                            (directory "/")
			    netns netns-ports-out netns-ports-in netns-verbose
                            netns-tuntap-devices
			    nsjail nsjail-settings
                            chroot masking-mounts skip-masking-mounts-check
                            fake-passwd fake-groups fake-usernames
                            clear-env)
  (let*
    ((uid
       (cond
	 (uid (select-subuser user :uid uid :name name))
	 (name (or (select-subuser user :name name)
		   (subuser-uid user :name name)))
	 (t (subuser-uid user :name name))))
     (command-with-env 
       (add-command-env command environment
			:env-helper "/usr/bin/env"
                        :clear-env t))
     (command-to-wrap command-with-env)
     (command-to-wrap
       (if netns
	 (add-command-netns 
	   command-to-wrap
           :ports-out netns-ports-out
           :ports-in netns-ports-in
           :tuntap-devices netns-tuntap-devices
	   :uid uid :gid gid :verbose netns-verbose
           :directory directory)
	 command-to-wrap))
     (command-to-wrap
       (cond
	 (nsjail
	   (apply
	     'add-command-nsjail
	     command-to-wrap uid :gid gid
             :directory directory
	     nsjail-settings))
	 (t (add-command-numeric-su command-to-wrap uid :gid gid))))
     (command-to-wrap
       (cond
         ((equal chroot "")
          (add-command-chroot command-to-wrap (prepare-basic-chroot)))
         (chroot
           (add-command-chroot command-to-wrap 
                               (prepare-basic-chroot
                                 (format nil "/run/basic-chroots/~a/~a/"
                                         user chroot))))
         (t command-to-wrap)))
     (command-to-wrap
       (cond ((or nsjail
                  (and (null masking-mounts)
                       (not fake-passwd)
                       (null fake-groups)))
              command-to-wrap)
             (t
               (loop for (source target . options) in masking-mounts
                     for kind = (cond
                                  ((find :tmpfs options) "T")
                                  ((find :read-only options) "R")
                                  ( t"B"))
                     unless skip-masking-mounts-check
                     do (assert (nsjail-mount-allowed-p source target kind)))
               (add-command-mounts
                 command-to-wrap
                 (append
                   masking-mounts
                   (when fake-passwd
                     (ensure-directories-exist "/tmp/system-lisp/subuser-passwd/")
                     (with-open-file 
                       (f (format nil 
                                  "/tmp/system-lisp/subuser-passwd/~a" uid)
                          :direction :output :if-exists :supersede)
                       (format f "root:x:0:0::/:/bin/sh~%")
                       (format f ".~a:x:~a:~a::~a:/bin/sh~%" uid uid gid
                               (second 
                                 (or (find-if 
                                       (lambda (x)
                                         (and (listp x)
                                              (equal (first x) "HOME")))
                                       environment) '("HOME" "/"))))
                       (loop for u in fake-usernames do
                             (format f "~a:x:~a:~a::~a:/bin/sh~%" u uid gid "/"))
                       (format f ".~a:x:~a:~a::/:/bin/sh~%" 65534 65534 65534)
                       )
                     `((,(format nil "/tmp/system-lisp/subuser-passwd/~a" uid)
                         "/etc/passwd" :read-only)))
                   (when fake-groups
                     (ensure-directories-exist "/tmp/system-lisp/subuser-passwd/")
                     (with-open-file 
                       (f (format nil 
                                  "/tmp/system-lisp/subuser-passwd/~a.group" uid)
                          :direction :output :if-exists :supersede)
                       (format f "root:x:0:~%")
                       (format f ".~a:x:~a:~%" uid gid)
                       (format f "~{~{~a:x:~a:~}~%~}" (if (listp fake-groups) 
                                                        (loop for g in fake-groups
                                                              if (listp g) collect g
                                                              else collect (list g 65534))
                                                        (list))))
                     `((,(format nil "/tmp/system-lisp/subuser-passwd/~a.group" uid)
                        "/etc/group" :read-only))))))))
     (wrapped-command command-to-wrap)
     (process
       (iolib/os:create-process
	 wrapped-command
	 :stdin (or stdin-fd (when feed-stdin :pipe) :null)
	 :stdout (or stdout-fd (when slurp-stdout :pipe) :null)
	 :stderr (or stderr-fd (when slurp-stderr :pipe) :null)
	 :pty pty :current-directory (or directory "/")
         :new-session pty))
     (stdin-feeder
       (bordeaux-threads:make-thread
	 (lambda ()
	   (when feed-stdin
	     (let*
	       ((stream (iolib/os:process-stdin process)))
	       (ignore-errors
		 (unwind-protect
		   (progn
		     (format stream "~a" feed-stdin)
		     (finish-output stream))
		   (close stream))))))
	 :name "Process stdin feeder"))
     (return-value
       (cond
	 (slurp-stdout
	   (iolib/os:process-status process :wait t)
	   (iolib/os::slurp-char-stream
	     (iolib/os:process-stdout process)))
	 (slurp-stderr
	   (iolib/os:process-status process :wait t)
	   (iolib/os::slurp-char-stream
	     (iolib/os:process-stderr process)))
	 (wait (iolib/os:process-status process :wait t))
	 (t process))))
    (if (or wait slurp-stdout slurp-stderr)
      (progn
	(bordeaux-threads:join-thread stdin-feeder)
	(when slay (slay-subuser user :uid uid)))
      (bordeaux-threads:make-thread
	(lambda ()
	  (iolib/os:process-status process :wait t)
	  (bordeaux-threads:join-thread stdin-feeder)
	  (when slay (slay-subuser user :uid uid)))
	:name "Process reaper"))
    return-value))
