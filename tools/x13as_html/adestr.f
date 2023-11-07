C     Last change:  BCM  29 Jun 1998    1:02 pm
      SUBROUTINE adestr(Begdat,Nrxy,Ncxy,Isp,Icol,Ndays,Easidx,Xy,
     &                  Xmeans,Emean,Estock)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     adestr.f, Release 1, Subroutine Version 1.1, Modified 20 Oct 1994.
c-----------------------------------------------------------------------
c     BCM April, 2016 - generate easter(0) regressor
c-----------------------------------------------------------------------
c	Subroutine takes input for a year and month where the year is in
c the 20th century, and the subroutine returns Bell's holiday variables
c for easter (see Bell 1983).
c-----------------------------------------------------------------------
c Name  Type Description
c-----------------------------------------------------------------------
c i       i  Local do loop index
c idate   i  Input of length 2 array for the current date (yr,mo)
c kdate   i  Local array of the dates of easter, labor day and thanks
c            giving.
c            Dates range from March 23 - April 25 for Easter
c            kdate(i) = offset to be added to March 22 in order to get
c            the correct dates for year i where the year ranges from
c            1901 to 2100.
c cmlmo   i  cumulative sum of lengths of months
c lpyr    i  Local to indicate leap year and is also the offset for the
c             fdomo and lnomo arrays.  leapyear = 12 and otherwise 0.
c Ndays   i  Local number of days prior to Easter in holiday effect
c            (not including Easter itself). Ndays = 0 specifies only 
c            Easter day will be used in the regressor.
c ZERO    d  Local PARAMETER for 0.0d0
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
c     ------------------------------------------------------------------
      DOUBLE PRECISION ZERO,MONE
      PARAMETER(ZERO=0.0D0,MONE=-1.0D0)
c     ------------------------------------------------------------------
      LOGICAL Xmeans,Estock
      INTEGER Begdat,Ncxy,Nrxy,predat,cmlnmo,cmlnqt,idate,Icol,Isp,
     &        julbeg,juleas,julend,kdate,lpyr,Ndays,ibeg,iend,period,
     &        year,i,Easidx,itmp
      DOUBLE PRECISION tmp,Xy,Emean
      DIMENSION Begdat(2),predat(2),cmlnmo(13,2),cmlnqt(5,2),idate(2),
     &          kdate(1901:2100),Xy(Ncxy,Nrxy),Emean(PSP)
c-----------------------------------------------------------------------
      DOUBLE PRECISION sceast
      EXTERNAL sceast
c-----------------------------------------------------------------------
c     The date of Easter = March 22 + kdate(year)
c for year = 1901 to 2100, inclusive
c-----------------------------------------------------------------------
      DATA kdate/
     &    16, 8,21,12,32,24, 9,28,20, 5,25,16, 1,21,13,32,17, 9,29,13,
     &     5,25,10,29,21,13,26,17, 9,29,14, 5,25,10,30,21, 6,26,18, 2,
     &    22,14,34,18,10,30,15, 6,26,18, 3,22,14,27,19,10,30,15, 7,26,
     &    11,31,23, 7,27,19, 4,23,15, 7,20,11,31,23, 8,27,19, 4,24,15,
     &    28,20,12,31,16, 8,28,12, 4,24, 9,28,20,12,25,16, 8,21,13,32,
     &    24, 9,29,20, 5,25,17, 1,21,13,33,17, 9,29,14, 5,25,10,30,21,
     &    13,26,18, 9,29,14, 6,25,10,30,22, 6,26,18, 3,22,14,34,19,10,
     &    30,15, 7,26,18, 3,23,14,27,19,11,30,15, 7,27,11,31,23, 8,27,
     &    19, 4,24,15, 7,20,12,31,23, 8,28,19, 4,24,16,28,20,12,32,16,
     &     8,28,13, 4,24, 9,29,20,12,25,17, 8,21,13,33,24, 9,29,21, 6/
c-----------------------------------------------------------------------
c    Cumulative sums of lengths of months
c-----------------------------------------------------------------------
      DATA cmlnmo/0,31,59,90,120,151,181,212,243,273,304,334,365,0,31,
     &     60,91,121,152,182,214,244,274,305,335,366/
      DATA cmlnqt/0,90,181,273,365,0,91,182,274,366/
c-----------------------------------------------------------------------
c     Add the holiday effects row by row, row i and columns begcol to
c endcol
c-----------------------------------------------------------------------
      CALL addate(Begdat,Isp,-1,predat)
c     ------------------------------------------------------------------
      DO i=1,Nrxy
       CALL addate(predat,Isp,i,idate)
       year=idate(YR)
       period=idate(MO)
c     ------------------------------------------------------------------
       IF((mod(year,100).ne.0.and.mod(year,4).eq.0).or.mod(year,400)
     &    .eq.0)THEN
        lpyr=2
c     ------------------------------------------------------------------
       ELSE
        lpyr=1
       END IF
c-----------------------------------------------------------------------
c     Quarterly Easter effect.
c-----------------------------------------------------------------------
       IF(Isp.ne.12)THEN
        julbeg=cmlnqt(period,lpyr)+1
        julend=cmlnqt(period+1,lpyr)
c-----------------------------------------------------------------------
c     Calculating Julian date of holidays and proportion of days in
c quarter which fall within the holiday window.  Easter first.
c Computing beginning and ending dates of current month
c which overlap with holiday effect window
c-----------------------------------------------------------------------
        juleas=cmlnqt(2,lpyr)-9+kdate(year)
        IF(Ndays.gt.0)THEN
         ibeg=max(julbeg,juleas-Ndays+Easidx)
         iend=min(julend,juleas-1+Easidx)
        ELSE
         ibeg=max(julbeg,juleas-Easidx)
         iend=min(julend,juleas-Easidx)
        END IF
c-----------------------------------------------------------------------
c     Dividing days in current month which fall within window
c by length of window to computed proportion of days
c-----------------------------------------------------------------------
        IF(ibeg.le.iend)THEN
         itmp=iend-ibeg+1
         IF(Easidx.eq.0)THEN
          tmp=dble(itmp)
          IF(Ndays.gt.0)tmp=tmp/dble(Ndays)
         ELSE 
          tmp=sceast(Ndays,itmp,period.eq.1,julend.ge.juleas)
         END IF
c     ------------------------------------------------------------------
        ELSE
         IF(Easidx.eq.1.and.period.eq.2)THEN
          tmp=MONE
         ELSE
          tmp=ZERO
         END IF
        END IF
c-----------------------------------------------------------------------
c     Monthly Easter effect.   Calculating Julian date of beginning and
c ending of present month
c-----------------------------------------------------------------------
       ELSE
        julbeg=cmlnmo(period,lpyr)+1
        julend=cmlnmo(period+1,lpyr)
c-----------------------------------------------------------------------
c     Calculating Julian date of holidays and proportion of days in
c month which fall within the holiday window.  Easter first.
c Computing beginning and ending dates of current month
c which overlap with holiday effect window
c-----------------------------------------------------------------------
        juleas=cmlnmo(3,lpyr)+22+kdate(year)
        IF(Ndays.gt.0)THEN
         ibeg=max(julbeg,juleas-Ndays+Easidx)
         iend=min(julend,juleas-1+Easidx)
        ELSE
         ibeg=max(julbeg,juleas-Easidx)
         iend=min(julend,juleas-Easidx)
        END IF
c-----------------------------------------------------------------------
c     Dividing days in current month which fall within window
c by length of window to computed proportion of days
c-----------------------------------------------------------------------
        IF(ibeg.le.iend)THEN
         itmp=iend-ibeg+1
         IF(Easidx.eq.0)THEN
          tmp=dble(itmp)
          IF(Ndays.gt.0)tmp=tmp/dble(Ndays)
         ELSE
          tmp=sceast(Ndays,itmp,period.eq.3,julend.ge.juleas)
         END IF
        ELSE
         IF(Easidx.eq.1.and.period.eq.4)THEN
          tmp=MONE
         ELSE
          tmp=ZERO
         END IF
        END IF
       END IF
c-----------------------------------------------------------------------
c     Subtracting off long term means of holiday effects in order
c to make holiday effects orthogonal to the trend
c-----------------------------------------------------------------------
       IF(Xmeans.and.Easidx.eq.0)tmp=tmp-Emean(period)
c-----------------------------------------------------------------------
c     if stock end-of-month easter, change easter regressor based on
c     current period
c-----------------------------------------------------------------------
       IF(Estock)THEN
        IF(Isp.eq.4)THEN
         IF(period.eq.2)tmp=ZERO
        ELSE
         IF(period.eq.3)THEN
          tmp=tmp+Xy(Icol,i-1)
         ELSE IF(period.eq.4)THEN
          tmp=ZERO
         END IF
        END IF
       END IF
c-----------------------------------------------------------------------
c     Put the effect in the regression matrix.
c-----------------------------------------------------------------------
       Xy(Icol,i)=tmp
      END DO
c     ------------------------------------------------------------------
      RETURN
      END

