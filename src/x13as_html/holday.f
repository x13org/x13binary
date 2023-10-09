C     Last change:  BCM  15 Apr 2005   11:46 am
**==holday.f    processed by SPAG 4.03F  at 15:12 on  1 Aug 1994
      SUBROUTINE holday(Sti,Mt1,Lgraf,Iforc,Xdsp)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'lzero.cmn'
      INCLUDE 'x11adj.cmn'
      INCLUDE 'x11fac.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'x11tbl.i'
      INCLUDE 'x11ptr.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'xeastr.cmn'
      INCLUDE 'extend.cmn'
c-----------------------------------------------------------------------
      LOGICAL F
      DOUBLE PRECISION ZERO,ONEHUN
      INTEGER YR
      PARAMETER(ZERO=0D0,ONEHUN=100D0,F=.false.,YR=1)
c-----------------------------------------------------------------------
      LOGICAL Lgraf
      INTEGER i,iend,l3,Mt1,numfct,nyear,ndfl,ndft,lasthl,Iforc,Xdsp
      DOUBLE PRECISION fstatl,fstatt,plevl,plevt,dvec,Sti
      DIMENSION dvec(1),Sti(PLEN)
c-----------------------------------------------------------------------
C--- INITIALIZE PRIOR FACTORS TO 100, INCLUDING ONE YEAR AHEAD FACTORS
c-----------------------------------------------------------------------
      dvec(1)=ZERO
      numfct=Iforc
      IF(Iforc.eq.0)numfct=12
      iend=Posfob+numfct+Xdsp
      DO i=Pos1bk,iend
       IF(i.le.Posfob)Yhol(i)=Sti(i)*ONEHUN
       X11hol(i)=ONEHUN
      END DO
c-----------------------------------------------------------------------
C--- CALCULATE NUMBER OF YEARS USED IN HOLIDAY ADJUSTMENT
c-----------------------------------------------------------------------
      l3=iend-Pos1bk+1
      nyear=l3/12
      IF(mod(l3,12).ne.0)nyear=nyear+1
c-----------------------------------------------------------------------
c     Compute X-11 holiday factors.
c-----------------------------------------------------------------------
      CALL holidy(X11hol,nyear,Pos1ob,Pos1bk,Begbak(YR),Posfob,numfct+
     &            Xdsp,fstatl,fstatt,ndfl,ndft,plevl,plevt,Keastr,Khol)
c-----------------------------------------------------------------------
c     Convert holiday factors to ratios
c-----------------------------------------------------------------------
      DO i=1,iend
       X11hol(i)=X11hol(i)/ONEHUN
      END DO
c-----------------------------------------------------------------------
c     Print holiday factors
c-----------------------------------------------------------------------
      IF(.not.Lhiddn)THEN
       IF(Prttab(LX11H1))THEN
        Kpart=0
c-----------------------------------------------------------------------
c     Print error message
c-----------------------------------------------------------------------
        IF((Ieast(1)*Ieast(2)*Ieast(3)*Ieast(4)).eq.0)THEN
         IF(Ieast(1).eq.0)CALL mkPOneLine(Mt1,'@',
     &                         'No years with Easter before April 1st.')
         IF(Ieast(2).eq.0)CALL mkPOneLine(Mt1,'@',
     &                         'No years with Easter after April 16th.')
         IF(Ieast(3).eq.0)CALL mkPOneLine(Mt1,'@',
     &                       'No years with Easter between April 2nd '//
     &                                   'and April 8th.')
         IF(Ieast(4).eq.0)CALL mkPOneLine(Mt1,'@',
     &                       'No years with Easter between April 8th '//
     &                                   'and April 15th.')
         CALL mkPOneLine(Mt1,'@','No Easter adjustment performed.')
        ELSE
         CALL table(X11hol,Pos1ob,Posfob+Xdsp,1,1,1,dvec,LX11H1)
         IF(Lfatal)RETURN
        END IF
       END IF
c-----------------------------------------------------------------------
c     Save holiday factors.  Check if forecasts are to be printed out.
c-----------------------------------------------------------------------
       IF(Savtab(LX11H1).or.Lgraf)THEN
        IF(Savfct)THEN
         lasthl=iend
        ELSE
         lasthl=Posfob+Xdsp
        END IF
        IF(Savtab(LX11H1))CALL punch(X11hol,Pos1ob,lasthl,LX11H1,F,F)
        IF(Lgraf)CALL punch(X11hol,Pos1ob,lasthl,LX11H1,Lgraf,F)
        IF(Lfatal)RETURN
       END IF
      END IF
c-----------------------------------------------------------------------
      RETURN
c-----------------------------------------------------------------------
      END
