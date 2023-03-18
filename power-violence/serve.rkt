#lang racket

(define (go)
  'what-it-do)


(define (serve port-no)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept-and-handle listener)
      (loop))
    (thread loop))
  (lambda () (custodian-shutdown-all main-cust)))

(define (accept-and-handle listener)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))

    ; handler thread
    (define hdlr
      (thread
       (lambda ()
         (handle in out)
         (close-input-port in)
         (close-output-port out))))
    
    ; watcher thread
    (thread
     (lambda ()
       (sleep 10)
       (custodian-shutdown-all cust)))))



(define (handle in out)
  (regexp-match #rx"(\r\n|^)\r\n" in)
  (display "HTTP/1.0 200 Okay\r\n" out)
  (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
  (display "<html><body>what it do</body></html>" out))
