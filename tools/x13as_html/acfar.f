      SUBROUTINE acfar(M,R,Res,Ndat,Ip,Iq)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C Construct autocovariances for AR-filtered transformed series.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
c-----------------------------------------------------------------------
      INTEGER PR
      PARAMETER(PR=PLEN/4)
c-----------------------------------------------------------------------
      INCLUDE 'autoq.cmn'
c-----------------------------------------------------------------------
C Dummy arguments
c-----------------------------------------------------------------------
      REAL*8 R,Res
      INTEGER Ip,Iq,M,Ndat
      DIMENSION R(*),Res(*)
c-----------------------------------------------------------------------
C Local variables
c-----------------------------------------------------------------------
      INTEGER i,is,k,m1,n,n35
c-----------------------------------------------------------------------
C     THE AUTOCOVARIANCES ARE STORED IN THE R VECTOR.
c-----------------------------------------------------------------------
      n=Ndat
C
      m1=MAX0(M,Ip+Iq+1)
      n35=INT(DLOG(DBLE(n))**2)
      M=MAX0(m1,n35)
      IF (M.GE.n) THEN
       M=MIN0(m1,n-n/4)
c       IF (Out.EQ.1) WRITE (7,*) 'M HAS BEEN MODIFIED TO ',M
      END IF
C
      C0=0.0D0
      DO i=1,n
       C0=C0+Res(i)**2
      END DO
c      IF (M.GE.n) THEN
c       M=n-2
c       IF (M.LE.0) THEN
c        WRITE (7,'(4X,''TOO FEW DEGREES OF FREEDOM'')')
c        RETURN
c       END IF
c       Ilb=M
c       IF (Out.EQ.1) WRITE (7,*) 'M HAS BEEN MODIFIED TO ',M
c      END IF
      C0=C0/DBLE(n)
      DO k=1,M
       R(k)=0.0D0
       is=k+1
       DO i=is,n
        R(k)=R(k)+Res(i)*Res(i-k)
       END DO
       R(k)=R(k)/(DBLE(n)*C0)
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
