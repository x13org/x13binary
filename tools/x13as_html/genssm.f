C     Last change:  BCM  19 May 2003    9:29 am
      SUBROUTINE genssm(Seatsf,Pos1ob,Posfob,Sfsum,Sf1ob,Ny,Lam)
      IMPLICIT NONE
c     ------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'dimensions.i'
      INCLUDE 'notset.prm'
      INCLUDE 'seatdg.cmn'
c     ------------------------------------------------------------------
      INTEGER N1,N12
      DOUBLE PRECISION ZERO
      PARAMETER (N12 = 12, N1 = 1, ZERO = 0D0)
      INCLUDE 'calc.i'
c     ------------------------------------------------------------------
      DOUBLE PRECISION Seatsf,Sfsum,Lam
      INTEGER Pos1ob,Posfob,Sf1ob,thisbd,i,j,k,jk,Ny
      DIMENSION Seatsf(PLEN),Sfsum(PLEN)
c     ------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c     ------------------------------------------------------------------
      IF(Idssm.ne.NOTSET)THEN
       thisBd=Idssm
      ELSE
       thisbd=Bd
      ENDIF
c     ------------------------------------------------------------------
      DO i=Pos1ob,Posfob
       IF(dpeq(Lam,ZERO))THEN
        Sfsum(i)=log10(Seatsf(i))
       ELSE
        Sfsum(i)=Seatsf(i)
       END IF
      END DO
c     ------------------------------------------------------------------
      Sf1ob=Pos1ob
      IF (thisbd.eq.0)RETURN
      IF (thisbd.eq.1)THEN
       Sf1ob=Pos1ob+Ny-1
       DO i=Posfob,Sf1ob,-1
        DO j=1,Ny-1
         Sfsum(i)=Sfsum(i)+Sfsum(i-j)
        END DO
       END DO
      ELSE
       Sf1ob=Pos1ob+2*(Ny-1)
       DO i=Posfob,Sf1ob,-1
        DO j=0,Ny-1
         DO k=0,Ny-1
          jk = j + k
          if(jk.gt.0)Sfsum(i)=Sfsum(i)+Sfsum(i-jk)
         END DO
        END DO
       END DO
      END IF
c     ------------------------------------------------------------------
      RETURN
      END