#lang racket

(define calc1 (list 'calc1 (list)))
(define calc2 (list 'calc2 (list calc1)))
(define linear1 (list 'linear1 (list)))
(define linear2 (list 'linear2 (list linear1)))
(define discrete1 (list 'discrete1 (list)))
(define advmath (list 'advmath (list calc2 linear2 discrete1)))

(define top1 (list calc1 calc2 linear1 linear2 discrete1 advmath))
(define top2 (list calc1 linear1 discrete1 calc2 linear2 advmath))
(define top3 (list linear1 calc1 calc2 discrete1 linear2 advmath))
(define top4 (list discrete1 calc1 calc2 linear1 linear2 advmath))
(define top5 (list calc1 calc2 discrete1 linear1 linear2 advmath))
(define top6 (list calc1 linear1 linear2 calc2 discrete1 advmath))

(define sorts (list top1 top2 top3 top4 top5 top6))

(provide (all-defined-out))
