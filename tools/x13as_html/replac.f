C     Last change:  BCM  11 Sep 97    3:16 pm
      SUBROUTINE replac(X,Y,Stwt,Lfda,Llda,Nm)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C --- THIS SUBROUTINE REPLACES VALUES IN ARRAY X WHICH HAVE A WEIGHT
C --- LESS THAN 1.0. THE REPLACEMENT VALUES ARE STORED IN ARRAY Y.
C --- THE REPLACEMENT VALUES ARE COMPUTED USING AN AVERAGE
C ---  OF THE NON-FULL WEIGHT VALUE TIMES ITS WEIGHT AND THE NEAREST
C --- 4 FULL-WEIGHT VALUES.
c-----------------------------------------------------------------------
      DOUBLE PRECISION BIG,ONE
      PARAMETER(BIG=10D16,ONE=1D0)
c-----------------------------------------------------------------------
      DOUBLE PRECISION totals,ave,Stwt,sumx,X,Y
      INTEGER i,inc,j,kfda,klda,l,Lfda,Llda,m,n,Nm,ihee,ihle,m2,nnm
      DIMENSION X(Llda),Y(Llda),Stwt(Llda)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
      IF(Nm.ne.1)THEN
       DO i=1,Llda
        Y(i)=BIG
       END DO
      END IF
      DO i=1,Nm
       kfda=Lfda+i-1
       klda=(Llda-kfda)/Nm*Nm+kfda
       IF(Nm.ne.1)ave=totals(X,kfda,klda,Nm,1)
       DO j=kfda,Llda,Nm
C --- TEST FOR FULL WEIGHT (1.0).                                       00403500
        IF(dpeq(Stwt(j),ONE))GO TO 80
        n=0
        sumx=Stwt(j)*X(j)
c     change by brian c. monsell
C --- SET INDICATORS FOR HITTING ENDS                                   00403200
        ihee=0
        ihle=0
c     end of change by brian c. monsell
        IF(j-Nm.le.kfda)THEN
C --- EXTREME VALUES IN THE FIRST 2 LOCATIONS AT EITHER END OF THE ARRAY00404100
C --- ARE REPLACED USING THE FOUR NEAREST NON-EXTREME VALUES.           00404200
         m=kfda
         l=klda
         inc=Nm
        ELSE
         IF(klda-Nm.gt.j)GO TO 20
         m=klda
         l=kfda
         inc=-Nm
        END IF
   10   IF(Stwt(m).ge.ONE)THEN
         sumx=sumx+X(m)
         n=n+1
         IF(n.ge.4)GO TO 60
        END IF
        IF(m.eq.l)GO TO 50
        m=m+inc
        GO TO 10
C --- EXTREME CENTRAL VALUES ARE REPLACED BY THE 2 NEAREST NON-EXTREME  00405700
C --- VALUES ON EACH SIDE IF 2 NON-EXTREME VALUES EXIST ON EACH SIDE.   00405800
C --- IF NOT, THE EXTREME CENTRAL VALUES ARE REPLACED BY THE FOUR       00405900
C --- NEAREST NON-EXTREME VALUES.                                       00406000
   20   m=j
c     change by brian c. monsell
        IF(ihle.eq.0.and.ihee.eq.1)m=m2
c     end of change by brian c. monsell
        l=klda
        inc=Nm
   30   DO WHILE (m.ne.l)
         m=m+inc
         IF(dpeq(Stwt(m),ONE))THEN
          sumx=sumx+X(m)
          n=n+1
          GO TO(30,40,30,60),n
          GO TO 40
         END IF
        END DO
        IF(inc.eq.Nm)ihle=1
        nnm=-Nm
        IF(inc.eq.nnm)ihee=1
        IF(ihle.eq.0)GO TO 20
        IF(ihee.ne.0)GO TO 50
c   15 M = J                                                             00407600
c     change by brian c. monsell
   40   IF(ihle.le.0.or.n.ne.2)THEN
         IF(ihee.gt.0)THEN
          m=m2
         ELSE
          m2=m
          m=j
         END IF
c     end of change by brian c. monsell
         l=kfda
         inc=-Nm
        END IF
        GO TO 30
   50   IF(Nm.le.1)GO TO 80
C --- IF THERE ARE FEWER THAN 4 FULL WEIGHT VALUES IN THE MONTH, REPLACE00408100
C --- THE EXTREME VALUE WITH THE AVERAGE OF ALL THE SI VALUES.          00408200
        X(j)=ave
        GO TO 70
   60   X(j)=sumx/(n+Stwt(j))
        IF(Nm.le.1)GO TO 80
   70   Y(j)=X(j)
   80   CONTINUE
       END DO
      END DO
      RETURN
      END
