C     Last change:  BCM   4 Sep 1998    3:29 pm
**==grzmth.f    processed by SPAG 4.03F  at 09:49 on  1 Mar 1994
      SUBROUTINE grzmth(Ibeg,Iend)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      DOUBLE PRECISION BIG
      PARAMETER(BIG=10D16)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'chrt.cmn'
      INCLUDE 'units.cmn'
c-----------------------------------------------------------------------
      INTEGER i,Ibeg,Iend,iyr,j,imonth,nfreq
c-----------------------------------------------------------------------
c      DOUBLE PRECISION Ser2
c      DIMENSION Ser2(61,12)
c      COMMON /grzmon/ Ser1,Ser2 
c-----------------------------------------------------------------------
C---- INITIALIZE THE ARRAYS
c-----------------------------------------------------------------------
      DO i=1,61
       DO j=1,12
        Ser1(i,j)=BIG
c        Ser2(i,j)=9999999999.D0
       END DO
      END DO
c-----------------------------------------------------------------------
C---PUT THE PROPER DATA VALUES TO BE PLOTTED INTO THE ARRAYS
c-----------------------------------------------------------------------
      nfreq=12
      IF(Nseas.eq.4)nfreq=4
c     i = 0
      i=(Ibeg/nfreq)*nfreq
      j=0
      DO iyr=1,61
       DO imonth=1,nfreq
        i=i+1
c         write(Mtprof,*) 'i,Ibeg,Iend',i,Ibeg,Iend
        IF(i.ge.Ibeg)THEN
         IF(i.gt.Iend)GO TO 10
         j=j+1
         Ser1(iyr,imonth)=Y1(j)
c         write(Mtprof,*) 'iyr,imonth,j',iyr,imonth,j,Y1(j)
c         Ser2(iyr,imonth)=Y2(j)
        END IF
       END DO
      END DO
   10 RETURN
      END
