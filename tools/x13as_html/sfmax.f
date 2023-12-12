C     Last change:  BCM  19 May 1998    3:28 pm
**==sfmax.f    processed by SPAG 4.03F  at 09:53 on  1 Mar 1994
      INTEGER FUNCTION sfmax(Lterm,Lter,Ny)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Determines the longest seasonal filter length used in a given
c     seasonal adjustment.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
c-----------------------------------------------------------------------
      INTEGER i,Lterm,Lter,Ny
      LOGICAL lstabl
      DIMENSION Lter(PSP)
c-----------------------------------------------------------------------
c     Intialize sfmax
c-----------------------------------------------------------------------
      sfmax=Lterm
      IF(sfmax.eq.6.or.sfmax.eq.5)THEN
       sfmax=0
      ELSE IF(sfmax.eq.7)THEN
       sfmax=-1
      END IF
      lstabl=.true.
c-----------------------------------------------------------------------
c     Loop through monthly seasonal factors to find maximum filter
c     length.
c-----------------------------------------------------------------------
      DO i=2,Ny
       IF(Lter(i).eq.0.and.sfmax.lt.1)THEN
        sfmax=2
       ELSE IF(Lter(i).gt.sfmax.and.Lter(i).lt.5)THEN
        sfmax=Lter(i)
       END IF
       lstabl=lstabl.AND.(Lter(i).eq.5)
      END DO
      IF(lstabl)sfmax=5
c-----------------------------------------------------------------------
      RETURN
      END
