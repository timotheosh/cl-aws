;;;; cl-aws.lisp

(in-package #:cl-aws)

;;; "cl-aws" goes here. Hacks and glory await!

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

(defun aws-config (profile)
  "To get a specific value:
   (cdr (assoc \"aws_session_token\" (aws-config \"dev\") :test #'string=))"
  (let ((confile-path (my-getenv "AWS_CONFIG_FILE"))
        (config (py-configparser:make-config)))
    (when (equalp confile-path nil)
      (setq confile-path (format nil "~A/~A" (my-getenv "HOME") ".aws/config")))
    (py-configparser:read-files config (list confile-path))
    (py-configparser:items config (format nil "profile ~A" profile))))

(defun get-aws-credentials (profile)
  "Just returns the needed creds for the aws signature"
  (let ((creds (aws-config profile)))
    (setf access-key (cdr (assoc "aws_access_key_id" creds :test #'string=)))
    (setf secret (cdr (assoc "aws_secret_access_key" creds :test #'string=)))
    (lambda ()
      (values access-key secret))))

(defun get-aws-credentials-dev ()
  (get-aws-credentials "dev"))

(defun get-sts-token (profile)
  "Returns the sts token in the aws config file"
  (let ((creds (aws-config profile)))
    (cdr (assoc "aws_session_token" creds :test #'string=))))

(defun swf-request (region sts-token action payload)
  (let ((host (format nil "swf.~(~A~).amazonaws.com" region)))
    (multiple-value-bind (authz date)
        (aws-sign4:aws-sign4 :region region
                   :service :swf
                   :method :post
                   :host host
                   :path "/"
                   :headers `((:x-amz-target . ,(format nil "SimpleWorkflowService.~A" action))
                              (:content-type .  "application/x-amz-json-1.0"))
                   :payload payload)
      (flex:octets-to-string
       (http-request (format nil "https://~A/" host)
                     :method :post
                     :additional-headers `((:x-amz-target . ,(format nil "SimpleWorkflowService.~A" action))
                                           (:x-amz-date . ,date)
                                           (:X-Amz-Security-Token . ,sts-token)
                                           (:authorization . ,authz))
                     :content-type "application/x-amz-json-1.0"
                     :content payload
                     :force-binary t)))))

(defmacro with-aws-credentials (profile &body body)
  `(let ((*aws-credentials* (get-aws-credentials profile)))
     ,@body))


(defun test (profile)
  (with-aws-credentials profile
      (swf-request :us-east-1
                   (get-sts-token profile)
                   "ListDomains" "{\"registrationStatus\":\"REGISTERED\"}")))

;;(drakma:http-request "http://common-lisp.net/")
;;(multiple-value-bind (access-key secret)
;;    (get-aws-credentials "dev")
;;  access-key)
;;(get-sts-token "dev")
;;(get-aws-credentials-dev)
;;(test "dev")

(defun credentials-from-file ()
  (let (access-key secret)
    (with-open-file (in (merge-pathnames ".aws/config" (user-homedir-pathname)))
      (setf access-key (read-line in))
      (setf secret (read-line in)))
    (lambda ()
      (values access-key secret))
    (values access-key secret)))
;;(credentials-from-file)
