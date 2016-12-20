;;;; cl-aws.lisp

(in-package #:cl-aws)

;;; "cl-aws" goes here. Hacks and glory await!

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
