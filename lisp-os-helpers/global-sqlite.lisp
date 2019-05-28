(defpackage :lisp-os-helpers/global-sqlite
  (:use :common-lisp)
  (:export
    #:*global-sqlite-location*
    #:with-global-sqlite
    #:simple-global-value
    #:simple-global-value-id
    #:complex-global-value
    #:complex-global-value-by-id
    ))
(in-package :lisp-os-helpers/global-sqlite)

(defvar *global-sqlite-location* "/run/global-sqlite/global.sqlite")

(defvar *global-sqlite-lock* (bordeaux-threads:make-lock "global.sqlite"))

(defmacro with-global-sqlite ((&key (global-sqlite-location *global-sqlite-location*)) &body body)
  (let*
    ((db-var (gensym)))
    `(bordeaux-threads:with-lock-held (*global-sqlite-lock*)
       (let* ((*global-sqlite-location* ,global-sqlite-location))
         (ensure-directories-exist *global-sqlite-location*)
         (clsql:with-database
           (,db-var (list *global-sqlite-location*) :pool t :database-type :sqlite3)
           (clsql:with-default-database
             (,db-var)
             ,@body))))))

(defun ensure-field (table field field-metadata)
  (unless
    (ignore-errors
      (clsql:select
        (clsql:sql-expression :attribute field)
        :from
        (clsql:sql-expression :table table)
        :limit 0)
      t)
    (clsql:execute-command
      (format nil "alter table ~a add column ~a ~{~a~};"
              (clsql:sql (clsql:sql-expression :table table))
              (clsql:sql (clsql:sql-expression :attribute field))
              (loop
                for m in field-metadata collect
                (clsql:sql m))))
    t))


(defun ensure-table (name fields)
  (unless
    (multiple-value-bind (result error)
      (ignore-errors
        (clsql:select
          (clsql:sql-expression :attribute :id)
          :from
          (clsql:sql-expression :table name)
          :limit 0)
        t)
      (unless result
        (format *error-output* "Table ~a apparently doesn't exist:~%~a~%"
                name error))
      result)
    (clsql:create-table
      name
      (cons
        (list (clsql:sql-expression :attribute :id)
              "INTEGER PRIMARY KEY AUTOINCREMENT")
        fields))
    t))

(defun ensure-field-index (table attribute &key unique)
  (ignore-errors
    (clsql:create-index
      table :on attribute :unique unique)))

(defun simple-global-value (name &optional (new-value nil new-value-p))
  (when 
    (ensure-table
      :simple-values
      (list
        (list
          (clsql:sql-expression :attribute :key)
          "varchar")
        (list
          (clsql:sql-expression :attribute :value)
          "varchar")
        ))
    (ensure-field-index :simple-values :key :unique t))
  (if new-value-p
    (progn
      (clsql:with-transaction 
        ()
        (if
          (clsql:select
            (clsql:sql-expression :attribute :id)
            :from
            (clsql:sql-expression :table :simple-values)
            :where
            (clsql:sql-operation 
              '=
              (clsql:sql-expression :attribute :key)
              name))
          (clsql:update-records
            :simple-values
            :where
            (clsql:sql-operation 
              '=
              (clsql:sql-expression :attribute :key)
              name)
            :av-pairs (list (list (clsql:sql-expression 
                                    :attribute :value)
                                  new-value)))
          (clsql:insert-records
            :into :simple-values
            :av-pairs
            (list
              (list (clsql:sql-expression :attribute :value) new-value)
              (list (clsql:sql-expression :attribute :key) name)
              )))
        new-value))
    (caar
      (clsql:select
        (clsql:sql-expression :attribute :value)
        :from
        (clsql:sql-expression :table :simple-values)
        :where
        (clsql:sql-operation 
          '=
          (clsql:sql-expression :attribute :key)
          name)))))

(defun simple-global-value-id (name)
  (caar
    (clsql:select
      (clsql:sql-expression :attribute :id)
      :from
      (clsql:sql-expression :table :simple-values)
      :where
      (clsql:sql-operation 
        '=
        (clsql:sql-expression :attribute :key)
        name))))

(defun complex-global-value
  (kind name fields types &optional new-values no-update)
  (ensure-table 
    kind
    (cons
      (list
        (clsql:sql-expression :attribute :key)
        "varchar")
      (loop 
        for f in fields
        for tt in types
        collect
        (list
          (clsql:sql-expression :attribute f)
          (format nil "~a" tt)))))
  (ensure-field-index kind :key :unique t)
  (unless
    (ignore-errors
      (apply
        'clsql:select
        (append
          (loop
            for f in fields 
            collect (clsql:sql-expression :attribute f))
          (list :from (clsql:sql-expression :table kind)
                :limit 0)
          ))
      t)
    (loop
      for f in fields
      for tt in types
      do (ensure-field kind f (list (clsql:sql-expression :attribute tt)))))
  (clsql:with-transaction
    ()
    (let*
      ((old-values
         (apply
           'clsql:select
           (append
             (loop
               for f in fields 
               collect (clsql:sql-expression :attribute f))
             (list :from (clsql:sql-expression :table kind)
                   :where
                   (clsql:sql-operation
                     '=
                     (clsql:sql-expression :attribute :key)
                     name))
             ))))
      (if new-values
        (if old-values
          (unless no-update
            (clsql:update-records
              (clsql:sql-expression :table kind)
              :where
              (clsql:sql-operation '= (clsql:sql-expression :attribute :key) name)
              :attributes
              (loop
                for f in fields 
                collect (clsql:sql-expression :attribute f))
              :values new-values))
          (clsql:insert-records
            :into (clsql:sql-expression :table kind)
            :attributes
            (cons
              (clsql:sql-expression :attribute :key)
              (loop
                for f in fields 
                collect (clsql:sql-expression :attribute f)))
            :values (cons name new-values)))
        (car old-values)))))

(defun complex-global-value-by-id (kind id fields)
  (first
    (apply
      'clsql:select
      (append
	(loop
	  for f in fields 
	  collect (clsql:sql-expression :attribute f))
	(list :from (clsql:sql-expression :table kind)
	      :where
	      (clsql:sql-operation
		'=
		(clsql:sql-expression :attribute :id)
		id))))))
