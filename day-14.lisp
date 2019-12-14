;;;; Advent of code day 14

(in-package #:aoc2019)

(defparameter *input-day-14*
"1 QDKHC => 9 RFSZD
15 FHRN, 17 ZFSLM, 2 TQFKQ => 3 JCHF
4 KDPV => 4 TQFKQ
1 FSTRZ, 5 QNXWF, 2 RZSD => 3 FNJM
15 VQPC, 1 TXCJ => 3 WQTL
1 PQCQN, 6 HKXPJ, 16 ZFSLM, 6 SJBPT, 1 TKZNJ, 13 JBDF, 1 RZSD => 6 VPCP
1 LJGZP => 7 VNGD
1 CTVB, 1 HVGW => 1 LJGZP
6 HVGW, 1 HJWT => 2 VDKF
10 PQCQN, 7 WRQLB, 1 XMCH => 3 CDMX
14 VNGD, 23 ZFSLM, 2 FHRN => 4 SJBPT
1 FSTRZ, 4 VTWB, 2 BLJC => 4 CKFW
2 ZTFH, 19 CKFW, 2 FHRN, 4 FNJM, 9 NWTVF, 11 JBDF, 1 VDKF, 2 DMRCN => 4 HMLTV
1 KVZXR => 5 FPMSL
8 XBZJ => 8 QDKHC
1 VQPC => 9 FHRN
15 RKTFX, 5 HKXPJ => 4 ZFSLM
1 HKXPJ, 8 LQCTQ, 21 VJGKN => 5 QCKFR
1 DCLQ, 1 TQFKQ => 4 KVZXR
4 NWTVF, 20 QNXWF => 9 JFLQD
11 QFVR => 3 RZSD
9 RFSZD, 6 WQTL => 7 JBDF
4 BLJC, 3 LQCTQ, 1 QCKFR => 8 QFVR
6 VNGD => 5 VQPC
7 CTMR, 10 SJBPT => 9 VTWB
1 VTWB => 9 DMRCN
6 BCGLR, 4 TPTN, 29 VNGD, 25 KDQC, 40 JCHF, 5 HMLTV, 4 CHWS, 2 CDMX, 1 VPCP => 1 FUEL
1 TQFKQ, 3 FPMSL, 7 KDPV => 6 RKTFX
8 HKXPJ, 2 WQTL => 6 WRQLB
146 ORE => 3 KDPV
9 KDQC => 2 XMCH
1 BGVXG, 21 KVZXR, 1 LQCTQ => 4 CTVB
1 LQCTQ => 5 VJGKN
16 VNGD, 5 VMBM => 1 CTMR
5 VCVTM, 1 FPMSL => 5 HKXPJ
4 HKXPJ => 5 BLJC
14 FHRN, 6 ZFSLM => 1 NWTVF
7 QCKFR, 2 VNGD => 7 VMBM
4 TXCJ, 1 VDKF => 2 QNXWF
136 ORE => 6 BGVXG
5 LQCTQ, 11 DCLQ => 9 XBZJ
3 VQPC => 7 ZTFH
114 ORE => 3 ZWFZX
1 HJWT, 18 KDPV => 7 TXCJ
1 VJGKN => 2 VCVTM
2 KVZXR => 1 HJWT
12 ZWFZX, 1 FHRN, 9 JFLQD => 1 CHWS
3 QFVR => 5 FSTRZ
5 XBZJ => 4 HVGW
1 ZWFZX => 8 LQCTQ
16 WQTL, 10 TXCJ => 9 KDQC
3 FHRN, 12 LJGZP => 5 TPTN
1 JCHF => 7 PQCQN
7 KDPV, 17 BGVXG => 7 DCLQ
1 CKFW, 3 TKZNJ, 4 PQCQN, 1 VQPC, 32 QFVR, 1 FNJM, 13 FSTRZ => 3 BCGLR
2 FSTRZ => 5 TKZNJ")

(defun list-to-assoc (list)
  (do* ((l list (cddr l))
	(alist (acons (first l) (second l) nil)
	       (acons (first l) (second l) alist)))
       ((null (cddr l)) (nreverse alist))))

(defun read-reactions (str)
  (let ((*package* (find-package :keyword))
	(lines))
    (with-input-from-string (stream str)
      (do ((line (read-line stream nil) (read-line stream nil)))
	  ((null line))
	(push (nreverse
	       (read-from-string (format nil "(~a)" (remove #\, line))))
	      lines)))
    (nreverse lines)))

(defun hash-reactions (list)
  (let ((hash (make-hash-table)))
    (dolist (elem list hash)
      (setf (gethash (first elem) hash)
	    (list* (second elem)
		   (list-to-assoc (nthcdr 3 elem)))))))

(defun ore-p (needs)
  "true if only needs ore."
  (and (not (cdr needs))
       (eq :ore (caar needs))))

(defun mult (qty qty-r)
  (if (>= qty-r qty)
      1
      (ceiling qty qty-r)))

(defun compress-react (needs)
  (let ((hash (make-hash-table))
	out)
    (dolist (e needs)
      (incf (gethash (car e) hash 0)
	    (cdr e)))
    (maphash (lambda (k v)
	       (setf out (acons k v out)))
	     hash)
    out))

(defun hash-to-alist (hash)
  (let (out)
    (maphash (lambda (k v)
	       (setf out (acons k v out)))
	     hash)
    out))

(defun find-final-react (reactions excess elem qty)
  (when (eq :ore elem)
    (return-from find-final-react (values (acons elem qty nil) excess)))
  (destructuring-bind (qty-r &rest needs)
      (gethash elem reactions)
    (declare (ignorable qty-r))
    (let ((elem-excess (gethash elem excess 0)))
      (cond ((>= elem-excess qty)
	     (values () (progn (decf (gethash elem excess) qty) excess)))
	    (t (let* ((new-q (- qty elem-excess))
		      (mult (mult new-q qty-r))
		      (n-excess (- (* qty-r mult) new-q))
		      (accum-reacts))
		 (dolist (n needs)
		   (multiple-value-bind (reacts c-excess)
		       (find-final-react reactions excess (car n) (* (cdr n) mult))
		     (declare (ignorable c-excess))
		     (setf accum-reacts (append reacts accum-reacts))
		     (setf (gethash elem excess) n-excess)))
		 (values (compress-react accum-reacts) excess)))))))

(defun produce-reaction (&key (input *input-day-14*) (output :fuel) (qty 1))
  (let* ((reactions (hash-reactions (read-reactions input)))
	 (final-r (find-final-react reactions (make-hash-table) output qty)))
    (cdar final-r)))

(defun ore-to-fuel (target)
  (let* ((lo 1)
	 (hi target))
    (do* ((mi (floor (+ lo hi) 2) (floor (+ lo hi) 2))
	  (f-mi (produce-reaction :qty mi) (produce-reaction :qty mi)))
	 ((= (- hi lo) 1) lo)
      (if (< f-mi target)
	  (setf lo mi)
	  (setf hi mi)))))

(defun solution-day14-1 ()
  (produce-reaction))

(defun solution-day14-2 ()
  (ore-to-fuel 1000000000000))

(defun solve-day14 ()
  (format t "Answer for puzzle 1 of day 14: ~A~&"
	  (solution-day14-1))
  (format t "Answer for puzzle 2 of day 14: ~A~&"
	  (solution-day14-2)))
