      SUBROUTINE emcomp(Emu,Efreq,Ndays)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Generate Easter means given a set of frequencies for Easter given
c     by the user (Efreq).
c-----------------------------------------------------------------------
      DOUBLE PRECISION ZERO
      PARAMETER(ZERO=0D0)
c-----------------------------------------------------------------------
      INTEGER kdate,Ndays,period,cmlnmo,julbeg,julend,ibeg,iend,itmp,
     &        juleas
      DOUBLE PRECISION Emu,Efreq,tmp
      DIMENSION Emu(2:4),Efreq(35),cmlnmo(13)
c-----------------------------------------------------------------------
      DATA cmlnmo/0,31,59,90,120,151,181,212,243,273,304,334,365/
c-----------------------------------------------------------------------
c     initialize easter mean for each month (Feb, March, April) 
c-----------------------------------------------------------------------
      DO period=2,4
       Emu(period)=ZERO
c-----------------------------------------------------------------------
c     compute percentage within easter window for each possible day of 
c     easter. 
c-----------------------------------------------------------------------
       DO kdate=1,35
c-----------------------------------------------------------------------
c     Monthly Easter effect.   Calculating Julian date of beginning and
c ending of present month
c-----------------------------------------------------------------------
        julbeg=cmlnmo(period)+1
        julend=cmlnmo(period+1)
c-----------------------------------------------------------------------
c     Calculating Julian date of holidays and proportion of days in
c month which fall within the holiday window.  Easter first.
c Computing beginning and ending dates of current month
c which overlap with holiday effect window
c-----------------------------------------------------------------------
        juleas=cmlnmo(3)+22+kdate
        ibeg=max(julbeg,juleas-ndays)
        iend=min(julend,juleas-1)
c-----------------------------------------------------------------------
c     Dividing days in current month which fall within window
c by length of window to computed proportion of days
c-----------------------------------------------------------------------
        tmp=ZERO
        IF(ibeg.le.iend)THEN
         itmp=iend-ibeg+1
         tmp=dble(itmp)/dble(ndays)
        END IF
c-----------------------------------------------------------------------
c     multiply percentage by frequency of occurance to get easter mean
c-----------------------------------------------------------------------
        Emu(period)=Emu(period)+tmp*Efreq(kdate)
       END DO
      END DO
c-----------------------------------------------------------------------
      RETURN
      END

