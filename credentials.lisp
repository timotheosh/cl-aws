(in-package #:cl-aws)

(defun my-getenv (name &optional default)
  #+CMU
  (let ((x (assoc name ext:*environment-list*
                  :test #'string=)))
    (if x (cdr x) default))
  #-CMU
  (or
   #+Allegro (sys:getenv name)
   #+CLISP (ext:getenv name)
   #+ECL (si:getenv name)
   #+SBCL (sb-unix::posix-getenv name)
   #+LISPWORKS (lispworks:environment-variable name)
   default))

(defun aws-config (profile &optional confile)
  "If confile is not provided, we try to find the aws config file from the
   AWS_CONFIG_FILE environment variable.
   To get a specific value:
   (cdr (assoc \"aws_session_token\" (aws-config \"dev\") :test #'string=))"
  (let ((confile-path confile)
        (config (py-configparser:make-config)))
    (when (equalp confile-path nil)
      (if (my-getenv "AWS_CONFIG_FILE")
          (setq confile-path (my-getenv "AWS_CONFIG_FILE"))
          (setq confile-path (format nil "~A/~A" (my-getenv "HOME") ".aws/config"))))
    (py-configparser:read-files config (list confile-path))
    (py-configparser:items config (format nil "profile ~A" profile))))

(defun get-aws-credentials (profile &optional confile)
  "Just returns the needed creds for the aws signature"
  (let ((creds (aws-config profile confile)))
    (setf access-key (cdr (assoc "aws_access_key_id" creds :test #'string=)))
    (setf secret (cdr (assoc "aws_secret_access_key" creds :test #'string=)))
    (lambda ()
      (values access-key secret))))

(defun get-sts-token (profile &optional confile)
  "Returns the sts token in the aws config file"
  (let ((creds (aws-config profile confile)))
    (cdr (assoc "aws_session_token" creds :test #'string=))))

(defun get-mfa-serial (profile &optional confile)
    "Returns the sts token in the aws config file"
  (let ((creds (aws-config profile confile)))
    (cdr (assoc "mfa_serial_number" creds :test #'string=))))

(defmacro with-aws-credentials (profile &body body)
  `(let ((*aws-credentials* (get-aws-credentials profile)))
     ,@body))
