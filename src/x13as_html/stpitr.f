C     Last change:  BCM  14 May 1998    9:17 am
      LOGICAL FUNCTION stpitr(Lprier,Objfcn,Devtol,Iter,Nliter,Mxiter,
     &                        Convrg,Armaer,Lhiddn)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     stpitr.f, Release 1, Subroutine Version 1.7, Modified 30 Nov 1994.
c-----------------------------------------------------------------------
c     Function to check for convergence of the Reg + ARIMA model.
c The subroutine checks for convergence of the reletive
c deviance<devtol/2.
c Also, checks for the deviance and the tolarance being less than
c machine precision and the deviance increasing.  stpitr=T is
c returned to call abend()
c call abend()
c because of an error, convrg=F.
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'units.cmn'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
c     ------------------------------------------------------------------
      DOUBLE PRECISION ZERO,ONE,TWO
      PARAMETER(ZERO=0D0,ONE=1D0,TWO=2D0)
c     ------------------------------------------------------------------
      LOGICAL Lprier,Convrg,Lhiddn
      INTEGER Armaer,Iter,Mxiter,Nliter
      DOUBLE PRECISION mprec,oldobj,Objfcn,Devtol,ratio
      SAVE oldobj
c-----------------------------------------------------------------------
      DOUBLE PRECISION dpmpar
      LOGICAL dpeq
      EXTERNAL dpeq,dpmpar
c-----------------------------------------------------------------------
c     Initialize the convergence switches and if this is the first
c iteration set the old objective function and return since
c there the relative deviance can not be calculated without the
c last objective function (ie oldobj).
c-----------------------------------------------------------------------
      mprec=dpmpar(1)
c     ------------------------------------------------------------------
      stpitr=.true.
      Convrg=.true.
c-----------------------------------------------------------------------
c     Stop if the maximum number of overall iterations is exceeded.
c-----------------------------------------------------------------------
      IF(Nliter.ge.Mxiter)THEN
       Armaer=PMXIER
       stpitr=.false.
       Convrg=.false.
c-----------------------------------------------------------------------
c     Only check the differences in the deviances after the second
c iteration.
c-----------------------------------------------------------------------
      ELSE IF(Iter.gt.1.AND.(.not.dpeq(Objfcn,ZERO)))THEN
       ratio=oldobj/Objfcn-ONE
c-----------------------------------------------------------------------
c     Stop if the convergence tolarance is greater than the
c precision of double precision numbers on the IBM PC.  If it
c is the tolarance is too tight you won't be able to calculate
c an accurate relative deviance that is smaller.This should
c be flagged on input.
c-----------------------------------------------------------------------
       IF(Devtol/TWO.lt.mprec)THEN
        Armaer=PCNTER
        stpitr=.false.
        Convrg=.false.
c-----------------------------------------------------------------------
c     Report if there is an increase in the deviance.
c-----------------------------------------------------------------------
       ELSE
        IF(Lprier.and.ratio.lt.0)THEN
         IF(Lhiddn)THEN
          CALL errhdr
          WRITE(Mt2,1011)Objfcn-oldobj,oldobj,MDLSEC,DOCNAM
         ELSE
          IF(.not.Lquiet)
     &       WRITE(STDERR,1010)Objfcn-oldobj,oldobj,MDLSEC,DOCNAM
          WRITE(Mt1,1011)Objfcn-oldobj,oldobj,MDLSEC,DOCNAM
         END IF
 1010    FORMAT(/,' WARNING:  Deviance value increased during ',
     &            'likelihood maximization',/,
     &            '           by ',1p,e25.15,' from ',1p,e25.14,'.',/,
     &            '           This might indicate a convergence ',
     &            'problem of the successive',/,
     &            '           estimates.  Print the iterations and ',
     &            'iterationerrors tables',/,
     &            '           of the estimate spec to check for ',
     &            'this. If there are convergence',/,
     &            '           problems, try specifying initial ',
     &            'values obtained from setting',/,
     &            '           exact=none in estimate, or try a ',
     &            'simpler model without parameter',/,
     &            '           constraints (See ',a,' of the ',a,').')
 1011    FORMAT(/,'<p><strong>WARNING:</strong> Deviance value ',
     &            'increased during likelihood maximization',/,
     &            ' by ',1p,e25.15,' from ',1p,e25.14,'.</p>',/,
     &            '<p>This might indicate a convergence problem of ',
     &            'the successive estimates.</p>',/,
     &            '<p>Print the iterations and iterationerrors ',/,
     &            'tables of the estimate spec to check for this.</p>',
     &            /,'<p>If there are convergence problems, try ',
     &            ' specifying initial values obtained from setting',/,
     &            ' exact=none in estimate, or try a simpler ',
     &            'model without parameter',/,
     &            ' constraints (See ',a,' of the ',a,').</p>')
        END IF
c-----------------------------------------------------------------------
c     Check for convergence
c-----------------------------------------------------------------------
        IF(abs(ratio).lt.Devtol)THEN
         stpitr=.false.
c-----------------------------------------------------------------------
c     Stop if the objective function greater than the machine
c precision.  If it is less it won't be able to calculate the
c relative deviance correctly.
c-----------------------------------------------------------------------
        ELSE IF(Objfcn.lt.mprec)THEN
         Armaer=PDVTER
         stpitr=.false.
        END IF
       END IF
c     ------------------------------------------------------------------
      ELSE
       oldobj=ZERO
      END IF
c     ------------------------------------------------------------------
      oldobj=Objfcn
c     ------------------------------------------------------------------
      RETURN
      END
