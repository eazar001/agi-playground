(defpackage #:view
  (:use :cl #:sdl2 #:file :bytes)
  (:export #:read-cel-data #:main))

(in-package #:view)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defvar *test-image*
  '(#x4F #x4A #x00 #x41 #x0F #x08 #x41 #x00 #x41 #x0F #x08 #x41 #x00 #x41 #x0F #x08 #x41 #x00
    #x41 #x0F #x08 #x41 #x00 #x41 #x0F #x08 #x41 #x00 #x41 #x0F #x08 #x41 #x00 #x41 #x0F #x08
    #x41 #x00 #x41 #x08 #x81 #x72 #x81 #x72 #x09 #x41 #x00 #x41 #x08 #x81 #x72 #x81 #x72 #x09
    #x41 #x00 #x41 #x08 #x81 #x71 #x82 #x71 #x0A #x41 #x00 #x41 #x08 #x81 #x72 #x81 #x72 #x09
    #x41 #x00 #x41 #x09 #x81 #x71 #x01 #x81 #x71 #x09 #x41 #x00 #x41 #x08 #x81 #x72 #x81 #x72
    #x09 #x41 #x00 #x41 #x08 #x81 #x71 #x82 #x71 #x0A #x41 #x00 #x41 #x08 #x81 #x72 #x81 #x72
    #x09 #x41 #x00 #x41 #x09 #x81 #x71 #x81 #x72 #x09 #x41 #x00 #x41 #x08 #x81 #x72 #x81 #x72
    #x09 #x41 #x00 #x41 #x07 #x82 #x75 #x09 #x41 #x00 #x41 #x06 #x82 #x77 #x08 #x41 #x00 #x41
    #x05 #x82 #x73 #x83 #x73 #x07 #x41 #x00 #x41 #x04 #x82 #x73 #x03 #x82 #x73 #x06 #x41 #x00
    #x41 #x03 #x82 #xF3 #x05 #x82 #xF3 #x05 #x41 #x00 #x41 #x03 #x81 #x73 #x07 #x82 #x73 #x04
    #x41 #x00 #x41 #x03 #x81 #x73 #x08 #x81 #x73 #x04 #x41 #x00 #x41 #x03 #x84 #x07 #x85 #x04
    #x41 #x00 #x41 #x04 #x81 #x73 #x05 #x82 #x73 #x05 #x41 #x00 #x41 #x05 #x81 #x73 #x03 #x82
    #x73 #x06 #x41 #x00 #x41 #x06 #x81 #x73 #x83 #x73 #x07 #x41 #x00 #x41 #x07 #x81 #x71 #xF1
    #x72 #x81 #x72 #x08 #x41 #x00 #x41 #x08 #x81 #xF1 #x72 #x81 #x71 #x09 #x41 #x00 #x41 #x0F
    #x08 #x41 #x00 #x41 #x0F #x08 #x41 #x00 #x41 #x0F #x08 #x41 #x00 #x41 #x0F #x08 #x41 #x00
    #x41 #x0F #x08 #x41 #x00 #x4F #x4A #x00))

(defvar *palette*
  '((#x00 #x00 #x00) (#x00 #x00 #xAA) (#x00 #xAA #x00) (#x00 #xAA #xAA) (#xAA #x00 #x00)
    (#xAA #x00 #xAA) (#xAA #x55 #x00) (#xAA #xAA #xAA) (#x55 #x55 #x55) (#x55 #x55 #xFF)
    (#x55 #xFF #x55) (#x55 #xFF #xFF) (#xFF #x55 #x55) (#xFF #x55 #xFF) (#xFF #xFF #x55)
    (#xFF #xFF #xFF)))

;; Extraction of VIEW resources


(defun draw-cel-data (renderer x y pixels)
  (if pixels
      (destructuring-bind ((color n0) . rest) pixels
        (if (and (= color 0) (= n0 0))
            (draw-cel-data renderer 0 (1+ y) rest)
            (let ((rgb (nth color *palette*))
                  (a #xFF)
                  (n (+ x (* 2 n0))))
              (destructuring-bind (r g b) rgb
                (sdl2:set-render-draw-color renderer r g b a))
              (do ((i x (1+ i)))
                  ((= i n) nil)
                (sdl2::render-draw-point renderer i y))
              (draw-cel-data renderer n y rest))))))

(defun read-cel-data ()
  (mapcar #'convert-cel-data *test-image*))

(defun convert-cel-data (byte)
  "Reads a list of bytes and transforming them into a list of pairs containing pixel RLE data."
  (let ((color (nibble byte :hi))
        (num-pixels (nibble byte :lo)))
    (list color num-pixels)))

(defmacro with-window-renderer ((window renderer) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "In-game image-rendering test"
                        :w *screen-width*
                        :h *screen-height*
                        :flags '(:shown))
       (sdl2:with-renderer (,renderer ,window :index -1 :flags '(:accelerated))
                           ,@body))))

(defun test ()
  (with-window-renderer (window renderer)
    (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
    (sdl2:render-clear renderer)
    (draw-cel-data renderer 0 0 (read-cel-data))
    (sdl2:render-present renderer)
    (sdl2::delay 5000)))
