(in-package "CCL")
(require "COCOA")

(defclass turtle-view (ns:ns-view)
  ()
  (:metaclass ns:+ns-object))

(defmacro with-focused-view (view &body forms)
  `(when (#/lockFocusIfCanDraw ,view)
     (unwind-protect
	  (progn ,@forms)
       (#/unlockFocus ,view)
       (#/flushGraphics (#/currentContext ns:ns-graphics-context))
       (#/flushWindow (#/window ,view)))))

(objc:defmethod (#/drawRect: :void) ((self turtle-view) (rect :<NSR>ect))
  (let* ((bounds (#/bounds self))
         (width (ns:ns-rect-width bounds))
         (height (ns:ns-rect-height bounds)))
    (#/set (#/whiteColor ns:ns-color))
    (#_NSRectFill bounds)))

(defun cocoa-setup ()
  (with-autorelease-pool
    (let* ((r (ns:make-ns-rect 100 350 800 800))
           (w (make-instance
                   'ns:ns-window
                   :with-content-rect r
                   :style-mask (logior #$NSTitledWindowMask
                                       #$NSClosableWindowMask
                                       #$NSMiniaturizableWindowMask)
                   :backing #$NSBackingStoreBuffered
                   :defer t)))
      (#/setTitle: w #@"The turtle")
      (let ((my-view (make-instance 'turtle-view :with-frame r)))
        (#/setContentView: w my-view)
        (#/setDelegate: w my-view))
      (#/performSelectorOnMainThread:withObject:waitUntilDone:
       w (objc:@selector "makeKeyAndOrderFront:") nil nil)
      w)))

(setf *window* (cocoa-setup))
(setf *view* (#/contentView *window*))
(setf *current-point* (list 0 0))

(defun draw (from to)
  (with-focused-view *view*
    (let* ((path (#/bezierPath ns:ns-bezier-path)))
      (#/set (#/blackColor ns:ns-color))
      (#/moveToPoint: path (apply #'ns:make-ns-point from))
      (#/lineToPoint: path (apply #'ns:make-ns-point to))
      (#/stroke path))))

(defun move (to)
  (draw *current-point* to)
  (setf *current-point* to))

(defun move-left (pixels)
  (move (list (- (car *current-point*) pixels) (cadr *current-point*))))

(defun move-right (pixels)
  (move-left (- pixels)))

(defun move-up (pixels)
  (move (list (car *current-point*) (+ (cadr *current-point*) pixels))))

(defun move-down (pixels)
  (move-up (- pixels)))

(defun clear ()
  (with-focused-view *view*
    (let* ((bounds (#/bounds *view*)))
      (#/set (#/whiteColor ns:ns-color))
      (#_NSRectFill bounds))))

(defun slow-move-left (pixels)
  (loop for x from 0 to pixels do (move-left 1) (sleep 0.1)))

(defun slow-move-right (pixels)
  (loop for x from 0 to pixels do (move-right 1) (sleep 0.1)))

(defun slow-move-up (pixels)
  (loop for x from 0 to pixels do (move-up 1) (sleep 0.1)))

(defun slow-move-down (pixels)
  (loop for x from 0 to pixels do (move-down 1) (sleep 0.1)))

(defun width-height ()
  (let* ((bounds (#/bounds *view*))
         (width (ns:ns-rect-width bounds))
         (height (ns:ns-rect-height bounds)))
    (list width height)))

(defun reset-middle ()
  (setf *current-point* (list (/ (car (width-height)) 2) (/ (cadr (width-height)) 2))))

(defparameter turtle-pane nil)
(defparameter turtle-x (car (width-height)))
(defparameter turtle-y (cadr (width-height)))
(defparameter turtle-theta 0)
(defparameter turtle-draw t)

(defun reset ()
  (setf turtle-x (/ (car (width-height)) 2))
  (setf turtle-y (/ (cadr (width-height)) 2))
  (setf turtle-theta 0)
  (setf turtle-draw t))

(defun forward (length)
  (let ((new-x (+ turtle-x (* length (cos turtle-theta))))
        (new-y (- turtle-y (* length (sin turtle-theta)))))
    (if turtle-draw
        (draw (list turtle-x turtle-y) (list new-x new-y)))
    (setf turtle-x new-x)
    (setf turtle-y new-y)))

(defun radians (angle)
  (* pi (/ angle 180)))

(defun right (angle)
  (setf turtle-theta (- turtle-theta (radians angle))))

(defun left (angle)
  (right (- angle)))

(defun back (length)
  (forward (- length)))

(defun penup ()
  (setf turtle-draw nil))

(defun pendown ()
  (setf turtle-draw t))

(defun inspi (side angle inc count)
  (forward side)
  (right angle)
  (if (> count 0)
      (inspi side (+ angle inc) inc (- count 1))))

(defun ldragon (size level)
  (if (= level 0)
      (forward size)
    (progn
      (ldragon size (- level 1))
      (left 90)
      (rdragon size (- level 1)))))

(defun rdragon (size level)
  (if (= level 0)
      (forward size)
    (progn
      (ldragon size (- level 1))
      (right 90)
      (rdragon size (- level 1)))))


