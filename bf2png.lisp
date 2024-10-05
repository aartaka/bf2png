#!sbcl --script

(require "asdf")
(asdf:load-asd #p"zpng/zpng.asdf")
(asdf:load-system "zpng")

;; ((#\+ . 701) (#\- . 591) (#\< . 4366) (#\> . 4428) (#\, . 0) (#\. . 3) (#\[ . 686) (#\] . 686))
(defun command->pixel (command)
  "Turn COMMAND into a valid pixel value.
Groups COMMAND into:
- Looping ([])
- Movement (<>)
- Arithmetic (+-)
- I/O (.,)

Stats from Mandelbrot-extreme.b:
((#\+ . 701) (#\- . 591) (#\< . 4366) (#\> . 4428) (#\, . 0) (#\. . 3) (#\[ . 686) (#\] . 686))"
  (list
   (case command
     ((#\[ #\]) #x00)
     ((#\< #\>) #x44)
     ((#\+ #\-) #x88)
     ((#\. #\,) #xCC)
     (t #xFF))))

(defun main (bf-in png-out)
  (let* ((commands (remove-if
                    (complement
                     (lambda (c)
                       (find c "+-<>,.[]")))
                    (uiop:read-file-string bf-in)))
         (size (ceiling (sqrt (length commands))))
         (commands (uiop:strcat commands
                                (make-string (- (* size size) (length commands))
                                             :initial-element #\Space))))
    (with-open-file (file png-out :direction :output
                                  :if-does-not-exist :create
                                  :if-exists :supersede
                                  :element-type '(unsigned-byte 8))
      (let ((png (make-instance 'zpng:pixel-streamed-png
                                :height size :width size :color-type :grayscale)))
        (loop initially (zpng:start-png png file)
              for row below size
              do (loop for col below size
                       do (zpng:write-pixel (command->pixel (elt commands (+ col (* row size))))
                                            png))
              finally (zpng:finish-png png))))))

(apply #'main uiop:*command-line-arguments*)
