;; Functional Programming Paradigm - URL Safety Checker
;; Uses pure functions, higher-order functions, and function composition

;; Pure function: Check if URL starts with a prefix
(defun starts-with (string prefix)
  (and (>= (length string) (length prefix))
       (string= string prefix :end1 (length prefix))))

;; Pure function: Extract protocol from URL
(defun get-protocol (url)
  (cond
    ((starts-with url "https://") 'https)
    ((starts-with url "http://") 'http)
    (t nil)))

;; Pure function: Check if protocol is valid
(defun valid-protocol-p (url)
  (not (null (get-protocol url))))

;; Pure function: Determine if URL is safe (HTTPS)
(defun is-safe-p (url)
  (eq (get-protocol url) 'https))

;; Pure function: Get safety status as string
(defun safety-status (url)
  (if (is-safe-p url)
      "SAFE (HTTPS - Encrypted)"
      "NOT SAFE (HTTP - Unencrypted)"))

;; Function: Measure access time using system call
(defun measure-access-time (url)
  (let ((start-time (get-internal-real-time)))
    (handler-case
        (let* ((command (format nil "curl -s -o /dev/null -w '%%{http_code}' --max-time 10 --head ~A 2>&1 || echo '000'" url))
               (output (with-output-to-string (stream)
                         (ext:shell command))))
          (let ((end-time (get-internal-real-time))
                (success (and (> (length output) 0)
                              (not (search "000" output))
                              (not (search "curl:" output)))))
            (list success 
                  (/ (- end-time start-time) 
                     (float internal-time-units-per-second)))))
      (error (e)
        (let ((end-time (get-internal-real-time)))
          (list nil 
                (/ (- end-time start-time) 
                   (float internal-time-units-per-second))))))))

;; Higher-order function: Process a single URL and return result
(defun check-url (url)
  (if (valid-protocol-p url)
      (let* ((protocol (get-protocol url))
             (status (safety-status url))
             (result (measure-access-time url))
             (success (first result))
             (access-time (second result)))
        (list url protocol status access-time success))
      (list url nil "Invalid URL (must start with http:// or https://)" 0.0 nil)))

;; Pure function: Format result for display
(defun format-result (result)
  (destructuring-bind (url protocol status access-time success) result
    (format nil "~%~%URL: ~A~%Status: ~A~%Access Time: ~,3F seconds~A"
            url
            status
            access-time
            (if success "" " (Failed)"))))

;; Higher-order function: Map over list of URLs (functional style)
(defun process-urls (urls)
  (mapcar (lambda (url)
            (let ((result (check-url url)))
              (format t "~A" (format-result result))
              result))
          urls))

;; Function composition: Main entry point
(defun main (args)
  (if (null args)
      (format t "Usage: clisp main.ls <url1> [url2] ...~%")
      (process-urls args)))

;; Entry point - functional style
(if (>= (length *args*) 1)
    (main *args*)
    (main nil))


