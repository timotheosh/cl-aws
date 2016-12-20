(in-package :cl-aws)

(defun sts-request (region action token)
  (let ((host (format nil "sts.~(~A~).amazonaws.com" region)))
    (multiple-value-bind (authz date)
        (aws-sign4:aws-sign4 :region region
                   :service :sts
                   :method :post
                   :host host
                   :path "/"
                   :headers `((:x-amz-target . ,(format nil "SecurityTokenService.~A" action))
                              (:content-type .  "application/x-amz-json-1.0"))
                   :payload payload)
      (flex:octets-to-string
       (http-request (format nil "https://~A/" host)
                     :method :post
                     :additional-headers `((:x-amz-target . ,(format nil "SecurityTokenService.~A" action))
                                           (:x-amz-date . ,date)
                                           (:authorization . ,authz))
                     :content-type "application/x-amz-json-1.0"
                     :force-binary t)))))

(defmacro with-aws-customcreds (profile confile &body body)
  `(let ((*aws-credentials* (get-aws-credentials profile confile)))
     ,@body))

(defun sts-get-token (profile confile token)
  (with-aws-customcreds profile confile
    (sts-request :us-east-1
                 "GetSecurityToken"
                 "DurationSeconds" "3600"
                 (get-mfa-serial "althusius" confile)
                 token)))

(defun ststest (token)
  (sts-get-token "althusius" "~/.aws/mfa_althusius.conf" token))
