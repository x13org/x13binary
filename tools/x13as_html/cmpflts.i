C
C     component filters common block
C
C     filters: column 1 = symmetric, column 2 = concurrent
C       size of filters is the same as size of data.
      DOUBLE PRECISION treFlt(1200,2), SAFlt(1200,2)
C     cycles/period set - all 1200 entries used.
      DOUBLE PRECISION fltW(0:1200)
C     for each filter: squared-gain, time-shift = - phase-delays;
C       all 1200 entries used.
      DOUBLE PRECISION treGain(0:1200,2), treTmShf(0:1200,2)
      DOUBLE PRECISION SAGain(0:1200,2),  SATmShf(0:1200,2)
C     concurrent filter zero (at some frequency): 1 for SA, 2 for trend
      logical concFltZ(2),
C     Does each quantity exist. for each filter, squared-gain, phase-delays
C     column 1 = symmetric, column 2 = concurrent
     &        ltreFlt(2), ltreGain(2), ltreTmShf(2),
     &        lSAFlt(2),  lSAGain(2),  lSATmShf(2)
      common / cmpflts / fltW,
     &                   treFlt, treGain, treTmShf,
     &                   SAFlt,  SAGain,  SATmShf
      common / lcmpflt / concFltZ,
     &                   ltreFlt, ltreGain, ltreTmShf,
     &                   lSAFlt,  lSAGain,  lSATmShf
	 