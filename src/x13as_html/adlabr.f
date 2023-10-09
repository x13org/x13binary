C     Last change:  BCM  25 Nov 97   10:19 am
      SUBROUTINE adlabr(Begdat,Nrxy,Ncxy,Icol,Ndays,Xy,Xmeans)
c-----------------------------------------------------------------------
c     adlabr.f, Release 1, Subroutine Version 1.2, Modified 18 Oct 1994.
c-----------------------------------------------------------------------
c	Subroutine takes input for a year and month where the year is in
c the 20th century, and the subroutine returns Bell's holiday variables
c labor day (see Bell 1983).
c-----------------------------------------------------------------------
c Name  Type Description
c-----------------------------------------------------------------------
c i       i  Local do loop index
c idate   i  Input of length 2 array for the current date (yr,mo)
c kdate   i  Local array of the dates of easter, labor day and thanks
c            giving.
c            Dates range from March 23 - April 25 for Easter
c                        from September 1 - September 7 for Labor Day
c                        from November 22 - November 28
c            kdate(i,j) = offset to be added to March 22, Aug 31, and
c            Nov 21 in order to get the correct dates for year i
c            where the year ranges from 1901 to 2100.
c cmlnmo  i  cumulative sum of lengths of months
c lpyr    i  Local to indicate leap year and is also the offset for the
c             fdomo and lnomo arrays.  leapyear = 12 and otherwise 0.
c means   d  Local vector of long term means of holiday effects
c Ndays   i  Local number of days prior to Labor Day in holiday effect
c            (not including Labor Day itself)
c ZERO    d  Local PARAMETER for 0.0d0
c-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
c     ------------------------------------------------------------------
      DOUBLE PRECISION ZERO
      PARAMETER(ZERO=0.0D0)
c     ------------------------------------------------------------------
      LOGICAL Xmeans
      INTEGER Begdat,Ncxy,Nrxy,predat,cmlnmo,Icol,idate,i,julbeg,julend,
     &        jullab,kdate,lpyr,ibeg,iend,mnindx,period,Ndays,year
      DOUBLE PRECISION means,tmp,Xy
      DIMENSION Begdat(2),predat(2),cmlnmo(13,2),idate(2),
     &          kdate(1901:2100),means(25,8:9),Xy(Ncxy,Nrxy)
c-----------------------------------------------------------------------
c     The date of Labor Day = August 31 + kdate(year)
c for year = 1901 to 2100, inclusive
c-----------------------------------------------------------------------
      DATA kdate/2,1,7,5,4,3,2,7,6,5,4,2,1,7,6,4,3,2,1,6,5,4,3,1,7,6,5,
     &     3,2,1,7,5,4,3,2,7,6,5,4,2,1,7,6,4,3,2,1,6,5,4,3,1,7,6,5,3,2,
     &     1,7,5,4,3,2,7,6,5,4,2,1,7,6,4,3,2,1,6,5,4,3,1,7,6,5,3,2,1,7,
     &     5,4,3,2,7,6,5,4,2,1,7,6,4,3,2,1,6,5,4,3,1,7,6,5,3,2,1,7,5,4,
     &     3,2,7,6,5,4,2,1,7,6,4,3,2,1,6,5,4,3,1,7,6,5,3,2,1,7,5,4,3,2,
     &     7,6,5,4,2,1,7,6,4,3,2,1,6,5,4,3,1,7,6,5,3,2,1,7,5,4,3,2,7,6,
     &     5,4,2,1,7,6,4,3,2,1,6,5,4,3,1,7,6,5,3,2,1,7,5/
c-----------------------------------------------------------------------
c    Cumulative sums of lengths of months
c-----------------------------------------------------------------------
      DATA cmlnmo/0,31,59,90,120,151,181,212,243,273,304,334,365,0,31,
     &     60,91,121,152,182,214,244,274,305,335,366/
c-----------------------------------------------------------------------
c     Long term means of holiday effects:
c means(tau,8) - August Labor Day effect
c means(tau,9) - September Labor Day effect
c-----------------------------------------------------------------------
      DATA(means(i,8),i=1,25)/
     & .8800D0,.8750D0,.8696D0,.8636D0,.8571D0,.8500D0,.8421D0,.8333D0,
     & .8235D0,.8125D0,.8000D0,.7857D0,.7692D0,.7500D0,.7273D0,.7000D0,
     & .6667D0,.6250D0,.5714D0,.5000D0,.4286D0,.3571D0,.2857D0,.2143D0,
     & .1429D0/
      DATA(means(i,9),i=1,25)/
     & .1200D0,.1250D0,.1304D0,.1364D0,.1429D0,.1500D0,.1579D0,.1667D0,
     & .1765D0,.1875D0,.2000D0,.2143D0,.2308D0,.2500D0,.2727D0,.3000D0,
     & .3333D0,.3750D0,.4286D0,.5000D0,.5714D0,.6429D0,.7143D0,.7857D0,
     & .8571D0/
c-----------------------------------------------------------------------
c     Add the holiday effects row by row, row i and columns begcol to
c endcol
c-----------------------------------------------------------------------
      mnindx=25-Ndays+1
      CALL addate(Begdat,12,-1,predat)
c     ------------------------------------------------------------------
      DO i=1,Nrxy
       CALL addate(predat,12,i,idate)
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
c     Calculating Julian date of beginning and ending of present month
c-----------------------------------------------------------------------
       julbeg=cmlnmo(period,lpyr)+1
       julend=cmlnmo(period+1,lpyr)
c-----------------------------------------------------------------------
c     Calculating Julian date of holiday and proportion of days in
c month which fall within the holiday window.  Computing beginning and
c ending dates of current month which overlap with holiday effect
c window.
c-----------------------------------------------------------------------
       jullab=cmlnmo(9,lpyr)+kdate(year)
       ibeg=max(julbeg,jullab-Ndays)
       iend=min(julend,jullab-1)
c-----------------------------------------------------------------------
c     Dividing days in current month which fall within window
c by length of window to computed proportion of days.  Then subtract
c off long term means of holiday effects in order to make holiday
c effects orthogonal to the trend.
c-----------------------------------------------------------------------
       IF(ibeg.le.iend)THEN
        tmp=dble(iend-ibeg+1)/dble(Ndays)
       ELSE
        tmp=ZERO
       END IF
c     ------------------------------------------------------------------
       IF(Xmeans.and.(period.eq.8.or.period.eq.9))
     &     tmp=tmp-means(mnindx,period)
c     ------------------------------------------------------------------
       Xy(Icol,i)=tmp
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
