;;;
;;; gl.simple.image - simple image I/O
;;;  
;;;   Copyright (c) 2005-2014  Shiro Kawai  <shiro@acm.org>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  
;;;  $Id: simple-image.scm,v 1.2 2008-06-04 11:46:27 shirok Exp $
;;;

;; For serious image handling, I'd recommend you to use a serious
;; library (e.g. Gauche-gd).
;; This module provides a minimal routines to do some experiment
;; and testing with Gauche-gl alone.

(define-module gl.simple.image
  (export read-sgi-image read-sgi-image-from-port)
  (use gauche.uvector)
  (use gauche.sequence)
  (use util.match)
  (use binary.pack)
  (use srfi-42)
  )
(select-module gl.simple.image)

(define (read-sgi-image file)
  (call-with-input-file file read-sgi-image-from-port))

;; read-sgi-image-from-port :: port -> (width, height, nchannels, data)
(define (read-sgi-image-from-port port)
  (match-let1 (magic compr bpp dim x y z min max pad1 name cmap . pad2)
      (unpack "nccnnnnNNNA80Nc404" :input port)
    (and (= magic 474)
         (= bpp 1)  ;; we only support 8bit/channel for now.
         (= cmap 0) ;; we only support direct pixel values for now.
         (if (= compr 1)
           (read-sgi-rle port dim x y z)
           (read-sgi-raw port dim x y z)))))

(define (read-sgi-raw port dim x y z)
  (case dim
    [(1) (let1 v (make-u8vector x)
           (read-block! v port)
           (list x 1 1 v))]
    [(2) (let1 v (make-u8vector (* x y))
           (read-block! v port)
           (list x y 1 v))]
    [(3) (let ([planes (list-ec (: i z)
                                (rlet1 v (make-u8vector (* x y))
                                  (read-block! v port)))]
               [vec (make-u8vector (* x y z))])
           (do-ec (: i (* x y))
                  (: plane (index j) planes)
                  (u8vector-set! vec (+ (* i z) j) (u8vector-ref plane i)))
           (list x y z vec))]
    (else #f)))

(define (read-sgi-rle port dim x y z)
  (let ([starts (make-u32vector (* y z))] ; scan line start indexes
        [sizes  (make-u32vector (* y z))] ; compressed scan line sizes
        [compressed #f]
        [offset (+ 512 (* 2 4 y z))]      ; offset to the compressed data
        [vec (make-u8vector (* x y z))])  ; result vector
    (read-block! starts port 0 -1 'big-endian)
    (read-block! sizes  port 0 -1 'big-endian)
    (set! compressed
          (string->u8vector
           (call-with-output-string (cut copy-port port <>))))

    (do-ec [: zz z]
           [: yy y]
           (let ([start (- (u32vector-ref starts (+ (* zz y) yy)) offset)]
                 [size  (u32vector-ref sizes (+ (* zz y) yy))])
             (let1 line
                 (uvector-alias <u8vector> compressed start (+ start size))
               (do ([xx (+ (* x yy z) zz) xx]
                    [k 0 k])
                   [(>= k size)]
                 (let1 b (u8vector-ref line k)
                   (inc! k)
                   (cond
                    [(= b 0)]
                    [(< b 128) ;; repeat next byte to b times
                     (let1 bb (u8vector-ref line k)
                       (inc! k)
                       (dotimes (n b)
                         (u8vector-set! vec xx bb)
                         (inc! xx z)))]
                    [else      ;; copy (- b 128) bytes
                     (dotimes [n (- b 128)]
                       (u8vector-set! vec xx (u8vector-ref line k))
                       (inc! k)
                       (inc! xx z))]))
                 ))))

    (list x y z vec)))
