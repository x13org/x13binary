C     Last change:  BCM  27 May 1998    3:34 pm
      SUBROUTINE td6var(Begdat,Isp,Numrxy,Numcxy,Begcol,Endcol,Smpday,
     &                  Xy,Begrgm,Td1c)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     td6var.f, Release 1, Subroutine Version 1.3, Modified 20 Oct 1994.
c-----------------------------------------------------------------------
c     Inserts the first six trading day variables into the regression 
c     matrix Xy
c-----------------------------------------------------------------------
c     Arguments used with this program:
c-----------------------------------------------------------------------
c     Begdat - Integer array of length 2 - beginning date of regression
c                                          matrix Xy
c              Begdat(1) = year (4 decimals assumed, ie, 1994)
c              Begdat(2) = period
c     Isp - Integer scalar - length of seasonal period
c           (12 for monthly, 4 for quarterly)
c     Numrxy - Integer scalar - # of rows in Xy matrix
c     Numcxy - Integer scalar - # of colums in Xy matrix
c     Begcol - Integer scalar - column of Xy matrix to place first
c                               trading day variable
c     Smpday - Integer scalar - sample day for stock trading day 
c                               variables
c     Xy - Double precision matrix of dimension Numrxy, Numcxy -
c          regression matrix where trading day variables will be stored
c     Begrgm - logical vector that shows where a regressor is defined
c                          when a change of regime has occurred.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      LOGICAL F,T
      DOUBLE PRECISION ZERO
      PARAMETER(F=.false.,T=.true.,ZERO=0D0)
c     ------------------------------------------------------------------
      LOGICAL Begrgm,Td1c
      INTEGER Begcol,Begdat,idate,irow,Numcxy,Numrxy,precol,predat,
     &        Endcol
      DOUBLE PRECISION Xy
      DIMENSION Begdat(2),idate(2),predat(2),Xy(Numcxy,Numrxy),
     &          Begrgm(PLEN)
c     ------------------------------------------------------------------
      CHARACTER str*(PCOLCR)
      LOGICAL ltdstk
      INTEGER Isp,period,Smpday,year
      INTEGER cendsp,sdoyr,fouryr(0:3)
      INTEGER lpyr
      INTEGER lnomo(12,2),ndomo
      INTEGER fdomo(12,2),sdomo
      INTEGER i,iday,icol,nchr
      INTEGER td(0:6)
      INTEGER ndywkm(0:6,28:31)
      INTEGER lnoqtr(4,2),ndoqtr
      INTEGER fdoqtr(4,2),sdoqtr
      INTEGER ndywkq(0:6,90:92)
      INTEGER dyosmp
c     ------------------------------------------------------------------
c     stckwt - weights for I*(t) to create one-coefficient stock trading
c              day (BCM July 2007)
c     ------------------------------------------------------------------
      DOUBLE PRECISION stckwt(4)
c     ------------------------------------------------------------------
      INTEGER strinx
      EXTERNAL strinx
c     ------------------------------------------------------------------
      CHARACTER DAYDIC*18
      INTEGER dayptr,PDAY
      PARAMETER(PDAY=6)
      DIMENSION dayptr(0:PDAY)
      PARAMETER(DAYDIC='montuewedthufrisat')
c     ------------------------------------------------------------------
      DATA fouryr/0,2,3,4/
      DATA lnomo/31,28,31,30,31,30,31,31,30,31,30,31,
     &           31,29,31,30,31,30,31,31,30,31,30,31/
      DATA fdomo/0,3,3,6,1,4,6,2,5,0,3,5,
     &           0,3,4,0,2,5,0,3,6,1,4,6/
      DATA ndywkm/4,4,4,4,4,4,4,
     &            5,4,4,4,4,4,4,
     &            5,5,4,4,4,4,4,
     &            5,5,5,4,4,4,4/
      DATA lnoqtr/90,91,92,92,
     &            91,91,92,92/
      DATA fdoqtr/0,6,6,0,
     &            0,0,0,1/
      DATA ndywkq/13,13,13,13,13,13,12,
     &            13,13,13,13,13,13,13,
     &            14,13,13,13,13,13,13/
      DATA dayptr/1,4,7,10,13,16,19/
c     ------------------------------------------------------------------
c     stckwt - weights for I*(t) to create one-coefficient stock trading
c              day (BCM July 2007)
c     ------------------------------------------------------------------
      DATA stckwt/-0.6D0,-0.2D0,0.2D0,0.6D0/
c     ------------------------------------------------------------------
      ltdstk=F
      IF(Smpday.gt.0)ltdstk=T
c     ------------------------------------------------------------------
      CALL addate(Begdat,Isp,-1,predat)
      precol=Begcol-1
c     ------------------------------------------------------------------
      DO irow=1,Numrxy
       IF(Begrgm(irow))THEN
        CALL addate(predat,Isp,irow,idate)
        year=idate(YR)
        period=idate(MO)
c-----------------------------------------------------------------------
c     The calendar as we know it begins in October 1752.  If we define
c Sun=0, Mon=1, ..., Sat=6, then we would like to start our pattern
c on the first leap year that start on a Sunday after 1753.  This is
c 1764.
c-----------------------------------------------------------------------
        sdoyr=5*(year/4-441)+fouryr(mod(year,4))
        cendsp=(year-1601)/100
        cendsp=cendsp-cendsp/4-1
        sdoyr=sdoyr-cendsp
        sdoyr=mod(sdoyr,7)
c     ------------------------------------------------------------------
        IF((mod(year,100).ne.0.and.mod(year,4).eq.0).or.mod(year,400)
     &     .eq.0)THEN
         lpyr=2
        ELSE
         lpyr=1
        END IF
c    -------------------------------------------------------------------
        IF(Isp.eq.12)THEN
         sdomo=mod(sdoyr+fdomo(period,lpyr),7)
         ndomo=lnomo(period,lpyr)
c     ------------------------------------------------------------------
         IF(ltdstk)THEN
          IF(Smpday.gt.ndomo)THEN
           dyosmp=mod(sdomo+ndomo-1,7)
c     ------------------------------------------------------------------
          ELSE
           dyosmp=mod(sdomo+Smpday-1,7)
          END IF
c     ------------------------------------------------------------------
          DO iday=0,6
           td(iday)=0
          END DO
          td(dyosmp)=1
c     ------------------------------------------------------------------
         ELSE
          DO i=0,6
           iday=mod(i+sdomo,7)
           td(iday)=ndywkm(i,ndomo)
          END DO
         END IF
c     ------------------------------------------------------------------
        ELSE IF(Isp.eq.4)THEN
         sdoqtr=mod(sdoyr+fdoqtr(period,lpyr),7)
         ndoqtr=lnoqtr(period,lpyr)
c     ------------------------------------------------------------------
         DO i=0,6
          iday=mod(i+sdoqtr,7)
          td(iday)=ndywkq(i,ndoqtr)
         END DO
        END IF
c     ------------------------------------------------------------------
        IF(Td1c)THEN
c     ------------------------------------------------------------------
c     generate stock 1-coef trading day regressor (BCM July 2007)
c     ------------------------------------------------------------------
         IF(ltdstk)THEN
          Xy(Begcol,irow)=DBLE(td(5)-td(0))
          DO iday=1,4
           Xy(Begcol,irow)=(stckwt(iday)*DBLE(td(iday)-td(0)))+
     &                     Xy(Begcol,irow)
          END DO
         ELSE
          Xy(Begcol,irow)=ZERO-(DBLE(td(0)+td(6))*2.5D0)
          DO iday=1,5
           Xy(Begcol,irow)=Xy(Begcol,irow)+DBLE(td(iday))
          END DO
         END IF
        ELSE
         DO icol=Begcol,Endcol
          IF((Endcol-Begcol+1).lt.6)THEN
           CALL getstr(Colttl,Colptr,Ncoltl,icol,str,nchr)
           IF(Lfatal)RETURN
           iday=strinx(F,DAYDIC,dayptr,1,PDAY,str(1:3))
          ELSE
           iday=icol-Begcol+1
          END IF
          td(iday)=td(iday)-td(0)
          Xy(icol,irow)=dble(td(iday))
         END DO
        END IF
       ELSE
        DO iday=1,Endcol-Begcol+1
         Xy(precol+iday,irow)=ZERO
        END DO
       END IF
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
