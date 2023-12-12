C     Last change:  BCM  25 Nov 97   10:20 am
**==adthnk.f    processed by SPAG 4.03F  at 12:44 on  7 Sep 1994
      SUBROUTINE adthnk(Begdat,Nrxy,Ncxy,Icol,Ndays,Xy,Xmeans)
c-----------------------------------------------------------------------
c     %M%, Release %R%, Subroutine Version %I%, Modified %G%.
c-----------------------------------------------------------------------
c	Subroutine takes input for a year and month where the year is in
c the 20th century, and the subroutine returns Bell's holiday variables
c for Thanksgiving-Christmas (see Bell 1983).
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
c Ndays   i  Local number of days after (negative) or before (positive)
c            Thanksgiving of the start of the Christmas buying season
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
     &        julthk,julxms,kdate,lpyr,ibeg,iend,mnindx,period,Ndays,
     &        win,year
      DOUBLE PRECISION means,tmp,Xy
      DIMENSION Begdat(2),predat(2),means(25,11:12),Xy(Ncxy,Nrxy),
     &          cmlnmo(13,2),idate(2),kdate(1901:2100)
c-----------------------------------------------------------------------
c     The date of Thanksgiving = November 21 + kdate(year,3)
c for year = 1901 to 2100, inclusive
c-----------------------------------------------------------------------
      DATA kdate/7,6,5,3,2,1,7,5,4,3,2,7,6,5,4,2,1,7,6,4,3,2,1,6,5,4,3,
     &     1,7,6,5,3,2,1,7,5,4,3,2,7,6,5,4,2,1,7,6,4,3,2,1,6,5,4,3,1,7,
     &     6,5,3,2,1,7,5,4,3,2,7,6,5,4,2,1,7,6,4,3,2,1,6,5,4,3,1,7,6,5,
     &     3,2,1,7,5,4,3,2,7,6,5,4,2,1,7,6,4,3,2,1,6,5,4,3,1,7,6,5,3,2,
     &     1,7,5,4,3,2,7,6,5,4,2,1,7,6,4,3,2,1,6,5,4,3,1,7,6,5,3,2,1,7,
     &     5,4,3,2,7,6,5,4,2,1,7,6,4,3,2,1,6,5,4,3,1,7,6,5,3,2,1,7,5,4,
     &     3,2,7,6,5,4,2,1,7,6,4,3,2,1,6,5,4,3,1,7,6,5,3/
c-----------------------------------------------------------------------
c    Cumulative sums of lengths of months
c-----------------------------------------------------------------------
      DATA cmlnmo/0,31,59,90,120,151,181,212,243,273,304,334,365,0,31,
     &     60,91,121,152,182,214,244,274,305,335,366/
c-----------------------------------------------------------------------
c     Long term means of holiday effects:
c means(tau,11) - November Thanksgiving effect
c means(tau,12) - December Thanksgiving effect
c-----------------------------------------------------------------------
      DATA(means(i,11),i=1,25)/.4884D0,.4773D0,.4656D0,.4534D0,.4406D0,
     &     .4273D0,.4132D0,.3985D0,.3830D0,.3667D0,.3494D0,.3313D0,
     &     .3120D0,.2917D0,.2700D0,.2471D0,.2226D0,.1684D0,.1384D0,
     &     .1062D0,.0776D0,.0530D0,.0326D0,.0167D0,.0057D0/
      DATA(means(i,12),i=1,25)/.5116D0,.5227D0,.5344D0,.5466D0,.5594D0,
     &     .5727D0,.5868D0,.6015D0,.6170D0,.6333D0,.6506D0,.6687D0,
     &     .6880D0,.7083D0,.7300D0,.7529D0,.7774D0,.8316D0,.8616D0,
     &     .8938D0,.9224D0,.9470D0,.9674D0,.9833D0,.9943D0/
c-----------------------------------------------------------------------
c     Add the holiday effects row by row, row i and columns begcol to
c endcol
c-----------------------------------------------------------------------
      mnindx=18-Ndays
      IF(Ndays.lt.0)mnindx=mnindx-1
      CALL addate(Begdat,12,-1,predat)
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
       julthk=cmlnmo(11,lpyr)+21+kdate(year)
       ibeg=max(julbeg,julthk-Ndays)
       julxms=cmlnmo(12,lpyr)+25
       iend=min(julend,julxms-1)
c-----------------------------------------------------------------------
c     Dividing days in current month which fall within window
c by length of window to computed proportion of days
c-----------------------------------------------------------------------
       win=julxms-julthk+Ndays
       IF(ibeg.le.iend)THEN
        tmp=dble(iend-ibeg+1)/dble(win)
       ELSE
        tmp=ZERO
       END IF
c-----------------------------------------------------------------------
c     Subtracting off long term means of holiday effects in order
c to make holiday effects orthogonal to the trend
c-----------------------------------------------------------------------
       IF(Xmeans.and.(period.eq.11.or.period.eq.12))
     &    tmp=tmp-means(mnindx,period)
       Xy(Icol,i)=tmp
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
