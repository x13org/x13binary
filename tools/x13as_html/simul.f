**==simul.f    processed by SPAG 6.05Fc at 12:31 on 12 Oct 2004
      DOUBLE PRECISION FUNCTION SIMUL(N,A,X,Eps,Indic,Ia)
 
      IMPLICIT NONE
**--SIMUL7
C
C*** Start of declarations rewritten by SPAG
      INCLUDE 'srslen.prm'
C
C Dummy arguments
C
      REAL*8 Eps
      INTEGER Ia,Indic,N
      REAL*8 A(Ia,*),X(N)
C
C Local variables
C
      REAL*8 aijck,deter,pivot,y(PLEN)
      DOUBLE PRECISION DABS,DBLE
      INTEGER i,intch,ip1,irowi,irowj,irowk,iscan,j,jcoli,jcolj,jcolk,
     &        jscan,jtemp,k,km1,imax,nm1,INT
      REAL*8 irow(PLEN),jcol(PLEN),jord(PLEN)

      LOGICAL dpeq
      EXTERNAL dpeq
C
C*** End of declarations rewritten by SPAG
C
c ****  Start of Executable Program                                     
 
      imax=N
      DO i=1,N
       irow(i)=0D0
       jcol(i)=0D0
      END DO
      IF (Indic.GE.0) imax=N+1
      IF (N.LE.396) THEN
 
       deter=1.0D0
       DO k=1,N
        km1=k-1
        pivot=0.0D0
        DO i=1,N
         DO j=1,N
          IF (k.NE.1) THEN
           DO iscan=1,km1
            DO jscan=1,km1
             IF (dpeq(DBLE(i),irow(iscan))) GO TO 10
             IF (dpeq(DBLE(j),jcol(jscan))) GO TO 10
            END DO
           END DO
          END IF
          IF (DABS(A(i,j)).GT.DABS(pivot)) THEN
           pivot=A(i,j)
           irow(k)=DBLE(i)
           jcol(k)=DBLE(j)
          END IF
   10     CONTINUE
         END DO
        END DO
        IF (DABS(pivot).GT.Eps) THEN
 
         irowk=INT(irow(k))
         jcolk=INT(jcol(k))
         deter=deter*pivot
         DO j=1,imax
          A(irowk,j)=A(irowk,j)/pivot
         END DO
         A(irowk,jcolk)=1.0D0/pivot
         DO i=1,N
          aijck=A(i,jcolk)
          IF (i.NE.irowk) THEN
           A(i,jcolk)=-aijck/pivot
           DO j=1,imax
            IF (j.NE.jcolk) A(i,j)=A(i,j)-aijck*A(irowk,j)
           END DO
          END IF
         END DO
        ELSE
         SIMUL=0.0D0
         RETURN
        END IF
       END DO
       DO i=1,N
        irowi=INT(irow(i))
        jcoli=INT(jcol(i))
        jord(irowi)=jcol(i)
        IF (Indic.GE.0) X(jcoli)=A(irowi,imax)
       END DO
       intch=0
       nm1=N-1
       DO i=1,nm1
        ip1=i+1
        DO j=ip1,N
         IF (jord(j).LT.jord(i)) THEN
          jtemp=INT(jord(j))
          jord(j)=jord(i)
          jord(i)=DBLE(jtemp)
          intch=intch+1
         END IF
        END DO
       END DO
       IF (intch/2*2.NE.intch) deter=-deter
       IF (Indic.LE.0) THEN
 
        DO j=1,N
         DO i=1,N
          irowi=INT(irow(i))
          jcoli=INT(jcol(i))
          y(jcoli)=A(irowi,j)
         END DO
         DO i=1,N
          A(i,j)=y(i)
         END DO
        END DO
        DO i=1,N
         DO j=1,N
          irowj=INT(irow(j))
          jcolj=INT(jcol(j))
          y(irowj)=A(i,jcolj)
         END DO
         DO j=1,N
          A(i,j)=y(j)
         END DO
        END DO
        SIMUL=deter
        RETURN
       END IF
      ELSE
       WRITE (6,1010)
 1010  FORMAT ('ON TOO BIG')
       SIMUL=0.0D0
       RETURN
      END IF
      SIMUL=deter
      RETURN
      END
