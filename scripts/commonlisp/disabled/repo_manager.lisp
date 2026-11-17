#!/usr/bin/env -S sbcl_batteriful --script

(defclass repo ()
  ((name
    :accessor name
    :initarg :name)
   (default-path
    :accessor default-path
    :initarg :default-path)
   (remotes
    :accessor remotes
    :initarg :remotes)
   (locals
    :accessor locals
    :initarg :locals)
   (tags
    :accessor tags
    :initarg :tags
    )))

(defun repo-new (&rest args
                 &key
                   (tags nil)
                   remotes
                   locals
                 &allow-other-keys)
  (let ((args
          (alexandria:remove-from-plist
           args
           ;; @redundant The first occurences of keyword args override the later ones
           :tags
           :remotes
           :locals)))
    (let ((repo (apply #'make-instance 'repo
                       :tags tags
                       :remotes remotes
                       :locals locals
                       args)))
      (dolist (i (list remotes locals))
        (dolist (remote i)
          (setf (parent-repo remote) repo)))
      repo
      )))

(defclass remote-store ()
  ((parent-repo
    :accessor parent-repo
    :initarg :parent-repo
    )
   (name
    :accessor name
    :initarg :name
    )
   (url
    :accessor url
    :initarg :url
    )
   (conditions
    :accessor conditions
    :initarg :conditions
    )))

(defun remote-store-new (&key (url nil) (class 'remote-store) (name "unnamed") (conditions nil))
  (apply #'make-instance
         `(,class
           ,@(when name
               (list :name name))
           ,@(when url
               (list :url url))
           :conditions ,(listify-if-not conditions))))

(comment
  (describe (remote-store-new
             :url "https://github.com/NightMachinary/ddg2json.git"
             :name "gh"
             )))

(defclass local-store (remote-store)
  ((remotes
    ;; Used to selectivly sync with remotes. `nil' means use all available remotes.
    :accessor remotes
    :initarg :remotes)
   (backup-targets
    :accessor backup-targets
    :initarg :backup-targets)))

(defun local-store-new (&rest args
                        &key
                          (class 'local-store)
                          (backup-targets nil)
                          (remotes nil)
                        &allow-other-keys)
  (let* ((args
           (alexandria:remove-from-plist
            args
            :class
            :backup-targets
            :remotes))
         (i (apply #'remote-store-new :class class args)))
    (setf (backup-targets i) backup-targets)
    (setf (remotes i) remotes)
    i))

(comment
  (local-store-new ))
;;;

;;;
(defparameter ddg2json
    (repo-new
     :name "ddg2json"
     :tags '(Python JSON DuckDuckGo)
     :default-path (path-unabbrev "~cod/python/ddg2json")
     :remotes (list
               (remote-store-new
                :url "https://github.com/NightMachinary/ddg2json.git"
                :name "gh"))
     :locals (list
              (local-store-new
               :name "MBP"
               :conditions #'mbp-p))))
(comment
  (describe ddg2json))
