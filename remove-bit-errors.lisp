;;;; remove-bit-errors.lisp

(in-package #:remove-bit-errors)

(defun byte-with-bit-flipped (byte bit-offset)
  (let ((int-repr-of-bit-offset (expt 2 (- 7 bit-offset))))
    (logxor byte int-repr-of-bit-offset)))

(defun transform-data-from-in-to-out (in-stream out-stream bit-position)
  (multiple-value-bind (byte-offset bit-offset) (floor bit-position 8)
    (loop for byte = (read-byte in-stream nil)
          for index from 0
          while byte do
            (if (= index byte-offset)
                (write-byte (byte-with-bit-flipped byte bit-offset) out-stream)
                (write-byte byte out-stream)))))

(defun flip-bit (source-file destination-file bit-position)
  (with-open-file (in-stream source-file
                             :direction :input
                             :element-type '(unsigned-byte 8))
    (with-open-file (out-stream destination-file
                                :direction :output
                                :if-exists :supersede
                                :element-type '(unsigned-byte 8))
      (transform-data-from-in-to-out in-stream out-stream bit-position))))
