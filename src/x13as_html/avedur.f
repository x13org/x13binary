C     Last change:  BCM  29 Sep 97    9:37 am
      SUBROUTINE avedur(Y,L,M,Adr)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      LOGICAL dpeq
      DOUBLE PRECISION Adr,count,runs,Y
      INTEGER i,L,M
      EXTERNAL dpeq
c-----------------------------------------------------------------------
C --- AVERAGE DURATION OF RUN SUBROUTINE
c-----------------------------------------------------------------------
      DIMENSION Y(*)
      i=L
      runs=1D0
      IF(Y(i).lt.Y(i+1))THEN
      ELSE IF(dpeq(Y(i),Y(i+1)))THEN
       DO WHILE (.true.)
        i=i+1
        IF(i.ge.M)GO TO 30
        IF(Y(i).lt.Y(i+1))GO TO 10
        IF(Y(i).gt.Y(i+1))GO TO 20
c        IF(.not.dpeq(Y(i),Y(i+1)))THEN
C***** SPAG has made duplicate copies of the following statement
c         runs=runs+1D0
c         GO TO 20
c        END IF
       END DO
      ELSE
       GO TO 20
      END IF
   10 DO WHILE (.true.)
       i=i+1
       IF(i.ge.M)GO TO 30
       IF(Y(i).gt.Y(i+1))THEN
C***** The following statement is a duplicate copy made by SPAG
        runs=runs+1D0
        GO TO 20
       END IF
      END DO
   20 DO WHILE (.true.)
       i=i+1
       IF(i.ge.M)GO TO 30
       IF(Y(i).lt.Y(i+1))THEN
        runs=runs+1D0
        GO TO 10
       END IF
      END DO
   30 count=M-L
      Adr=count/runs
      RETURN
      END
