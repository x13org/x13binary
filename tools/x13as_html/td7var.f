C     Last change:  BCM  30 Jun 1998    2:59 pm
      SUBROUTINE td7var(Begdat,Isp,Nrxy,Ncxy,Begcol,Lom,Ltdstk,Mltadd,
     &                  Xy,Begrgm)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     td7var.f, Release 1, Subroutine Version 1.2, Modified 03 Oct 1994.
c-----------------------------------------------------------------------
c     Adds a seventh trading day variable to the regression matrix Xy
c-----------------------------------------------------------------------
c     Arguments used with this program:
c-----------------------------------------------------------------------
c     Begdat - Integer array of length 2 - beginning date of regression
c                                          matrix Xy
c              Begdat(1) = year (4 decimals assumed, ie, 1994)
c              Begdat(2) = period
c     Isp - Integer scalar - length of seasonal period
c           (12 for monthly, 4 for quarterly)
c     Nrxy - Integer scalar - # of rows in Xy matrix
c     Ncxy - Integer scalar - # of colums in Xy matrix
c     Begcol - Integer scalar - column of Xy matrix to place first 
c                               trading day variable
c     Lom - Logical scalar - True if seventh variable is a length-of-
c                            month regressor, False if seventh TD
c                            variable is a leap year regressor.
c     Ltdstk - Logical scalar - True if stock trading day is performed,
c                               False if not.
c     Mltadd - Logical scalar - True if multiplicative model assumed,
c                               False if additive 
c     Xy - Double precision matrix of dimension Nrxy, Ncxy -
c          regression matrix where trading day variables will be stored
c     Begrgm - Local array that is true where there are trading day
c	       variables defined.
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
c     ------------------------------------------------------------------
      DOUBLE PRECISION ONE,ZERO,MP25,P75,P28FEB,P29FEB,AVEMO,AVEQTR,
     &                 P90QTR,P91QTR
      PARAMETER(ONE=1D0,ZERO=0D0,MP25=-.25D0,P75=.75D0,AVEMO=30.4375D0,
     &          AVEQTR=91.3125D0)
      PARAMETER(P28FEB=28D0/28.25D0)
      PARAMETER(P29FEB=29D0/28.25D0)
      PARAMETER(P90QTR=90D0/90.25D0)
      PARAMETER(P91QTR=91D0/90.25D0)
c     ------------------------------------------------------------------
      LOGICAL Lom,Ltdstk,Mltadd,Begrgm
      INTEGER Begcol,Begdat,idate,irow,Ncxy,Nrxy,predat
      DOUBLE PRECISION Xy,td7,td7sum
      DIMENSION Begdat(2),idate(2),predat(2),Xy(Ncxy,Nrxy),Begrgm(PLEN)
c     ------------------------------------------------------------------
      INTEGER Isp,period,year
      INTEGER lpyr
      INTEGER lnomo(12,2),ndomo
c  moved  Bob Fay
      INTEGER lnoqtr(4,2),ndoqtr
      DOUBLE PRECISION sly(48)
      INTEGER idxsly
c-----------------------------------------------------------------------
      DATA lnomo/31,28,31,30,31,30,31,31,30,31,30,31,31,29,31,30,31,30,
     &     31,31,30,31,30,31/
      DATA lnoqtr/90,91,92,92,91,91,92,92/
c-----------------------------------------------------------------------
c     For initial stock td values see Bell SRD Research Report 84/01
c p. 9 for calculation of \delta^{\script l}.
c-----------------------------------------------------------------------
      DATA sly/-.375D0,12*.375D0,12*.125D0,12*-.125D0,11*-.375D0/
c     ------------------------------------------------------------------      
      CALL addate(Begdat,Isp,-1,predat)
c     ------------------------------------------------------------------
      DO irow=1,Nrxy
       CALL addate(predat,Isp,irow,idate)
       year=idate(YR)
       period=idate(MO)
c     ------------------------------------------------------------------
       IF(Ltdstk)idxsly=mod(year,4)*Isp+period
       IF((mod(year,100).ne.0.and.mod(year,4).eq.0).or.mod(year,400)
     &     .eq.0)THEN
        lpyr=2
       ELSE
        lpyr=1
       END IF
c-----------------------------------------------------------------------
c     Determine if there is a length of month adjustment. Then if
c dtd(7) is the leap february variable or length of month
c Do for quarterly series first.
c-----------------------------------------------------------------------
       IF(Isp.ne.12)THEN
        IF(Lom)THEN
         ndoqtr=lnoqtr(period,lpyr)
         IF(Mltadd)THEN
          td7=DBLE(ndoqtr)/AVEQTR
         ELSE
          td7=DBLE(ndoqtr)-AVEQTR
         END IF
c     ------------------------------------------------------------------
        ELSE IF(Mltadd)THEN
         IF(period.ne.1)THEN
          td7=ONE
         ELSE IF(lpyr.eq.2)THEN
          td7=P91QTR
         ELSE
          td7=P90QTR
         END IF
c     ------------------------------------------------------------------
        ELSE IF(period.ne.1)THEN
         td7=ZERO
        ELSE IF(lpyr.eq.2)THEN
         td7=P75
        ELSE
         td7=MP25
        END IF
c-----------------------------------------------------------------------
c     For monthly series
c-----------------------------------------------------------------------
       ELSE IF(Lom)THEN
        ndomo=lnomo(period,lpyr)
        IF(Mltadd)THEN
         td7=DBLE(ndomo)/AVEMO
        ELSE
         td7=DBLE(ndomo)-AVEMO
        END IF
c     ------------------------------------------------------------------
       ELSE IF(Mltadd)THEN
        IF(period.ne.2)THEN
         td7=ONE
        ELSE IF(lpyr.eq.2)THEN
         td7=P29FEB
        ELSE
         td7=P28FEB
        END IF
c     ------------------------------------------------------------------
       ELSE IF(period.ne.2)THEN
        td7=ZERO
       ELSE IF(lpyr.eq.2)THEN
        td7=P75
       ELSE
        td7=MP25
       END IF
c-----------------------------------------------------------------------
c     Stock Length of month variable
c-----------------------------------------------------------------------
       IF(Ltdstk.and..not.Mltadd)THEN
        td7=sly(idxsly)
       END IF
c     ------------------------------------------------------------------
       IF(Mltadd)THEN
        Xy(Begcol,irow)=ONE
       ELSE
        Xy(Begcol,irow)=ZERO
       END IF
       IF(Begrgm(irow))Xy(Begcol,irow)=td7
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
