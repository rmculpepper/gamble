;; Copyright (c) 2014 Ryan Culpepper
;; Released under the terms of the 2-clause BSD license.
;; See the file COPYRIGHT for details.

#lang racket/base
(require racket/class
         racket/gui/base
         unstable/gui/notify
         data/gvector
         pict)
(provide multi-viz<%>
         multi-viz-widget%
         make-multi-viz-frame)

(define canvas/size-callback%
  (class canvas%
    (init-field size-callback)
    (super-new)
    (define/override (on-size w h)
      (size-callback w h))))

;; TODO: auto play, pause, framerate
;; TODO: scoll w/ scroll wheel (grr, complicated?!)

(define ZOOM-LEVELS
  '(["Auto fit" #f]
    ["10%"  1/10]
    ["25%"  1/4]
    ["50%"  1/2]
    ["100%" 1]
    ["150%" 3/2]
    ["200%" 2]
    ["300%" 3]
    ["400%" 4]
    ["500%" 5]
    ["10x"  10]))

(define DEFAULT-ZOOM "Auto fit")

;; ----

(define multi-viz<%>
  (interface ()
    ;; add-pict : Pict -> Void
    add-pict
    ))

(define multi-viz-widget%
  (class* object% (multi-viz<%>)
    (init-field parent)
    (super-new)

    ;; == Window elements ==

    (define-notify zoom-label (new notify-box% (value DEFAULT-ZOOM)))
    (define-notify zoom (new notify-box% (value #f)))
    (listen-zoom-label (lambda (zl) (set-zoom (cadr (assoc zl ZOOM-LEVELS)))))
    (listen-zoom (lambda _ (update-view)))

    (define top-bar
      (new horizontal-pane%
           (parent parent)
           (stretchable-height #f)
           (stretchable-width #t)))
    (define main-area
      (new horizontal-panel%
           (parent parent)
           (stretchable-height #t)
           (stretchable-width #t)
           (min-width 400)
           (min-height 400)))
    (define bottom-bar
      (new horizontal-pane%
           (parent parent)
           (alignment '(center center))
           (stretchable-height #f)
           (stretchable-width #t)))

    (define nav-bar
      (new horizontal-pane%
           (parent top-bar)
           (alignment '(center center))
           (stretchable-height #f)
           (stretchable-width #t)))
    (define zoom-choice
      (choice/notify-box bottom-bar
                         "Zoom: "
                         (map car ZOOM-LEVELS)
                         zoom-label))
    (define coords
      (new message%
           (parent top-bar)
           (label (make-string 10 #\space))
           (auto-resize #t)))

    (define start
      (new button%
           (parent nav-bar)
           (label "|<- Start")
           (callback (lambda (b ce) (set-index 0)))))
    (define prev
      (new button%
           (parent nav-bar)
           (label "<- Prev")
           (callback (lambda (b ce) (set-index (max 0 (sub1 (get-index))))))))
    (define next
      (new button%
           (parent nav-bar)
           (label "Next ->")
           (callback (lambda (b ce) (set-index (min (add1 (get-index)) (get-contents-length)))))))
    (define end
      (new button%
           (parent nav-bar)
           (label "End ->|")
           (callback (lambda (b ce) (set-index (sub1 (get-contents-length)))))))
    (define canvas
      (new canvas/size-callback%
           (parent main-area)
           (style '(hscroll vscroll))
           (paint-callback (lambda (c dc) (paint c dc)))
           (size-callback (lambda (w h) (on-canvas-resize)))))

    (define/private (on-canvas-resize)
      (when (eq? (get-zoom) #f) ;; Auto
        (update-view)))

    (define/private (navbar-update)
      (navbar-update* (> (get-index) 0)
                      (< (get-index) (get-contents-length))
                      (< (get-index) (sub1 (get-contents-length)))))

    (define/private (navbar-update* has-prev? has-current? has-next?)
      (send coords set-label
            (format "~s/~s"
                    (min (add1 (get-index)) (get-contents-length))
                    (get-contents-length)))
      (send start enable has-prev?)
      (send prev enable has-prev?)
      (send next enable has-next?)
      (send end enable has-next?))

    ;; == Contents ==

    ;; contents : (GVectorOf Pict)
    (define contents (make-gvector))

    ;; index, contents-length : (NotifyBoxOf Nat)
    (define-notify index (new notify-box% (value 0)))
    (define-notify contents-length (new notify-box% (value 0)))

    (let ([on-index-change
           (lambda _
             (navbar-update)
             (set-current-pict (gvector-ref contents (get-index) #f)))])
      (send index listen on-index-change)
      (send contents-length listen on-index-change))

    ;; == Zoom State ==

    (define/private (get-auto-zoom p)
      (define-values (cw ch) (send canvas get-client-size))
      (min (/ cw (pict-width p))
           (/ ch (pict-height p))))

    ;; == Pict State ==

    (define-notify current-pict (new notify-box% (value #f)))
    (listen-current-pict (lambda _ (update-view)))

    (define/public (add-pict p)
      (gvector-add! contents p)
      (set-contents-length (gvector-count contents)))

    ;; == Drawing ==

    (define/private (get-current-scaled-pict)
      (define p (get-current-pict))
      (and p (scale p (or (get-zoom) (get-auto-zoom p)))))

    (define/private (update-view)
      (define p (get-current-scaled-pict))
      (send canvas init-auto-scrollbars
            (ceiling (pict-width p)) (ceiling (pict-height p)) 0 0)
      (send canvas refresh))

    (define/private (paint _c dc)
      (define p (get-current-scaled-pict))
      (when p (draw-pict p dc 0 0)))

    ;; == Initialization ==
    (navbar-update)
    ))

;; ----

;; make-multi-viz-frame : ... -> multi-viz<%>
(define (make-multi-viz-frame #:label [label "Visualization"]
                              #:show? [show? #t])
  (define f (new frame% (label label)))
  (define c (new multi-viz-widget% (parent f)))
  (when show? (send f show #t))
  c)
