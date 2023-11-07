C     Last change:  BCM  16 Feb 1999    3:58 pm
      SUBROUTINE sumry(X,Xbar,Xbar2,Xsq,Xsd,Iopt,I,J)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C --- SUBROUTINE TO CALCULATE SUMMARY MEASURES.
C --- X IS THE INPUT SERIES. I IS THE 1ST VALUE OF X AND J IS THE LAST
C --- VALUE OF X.
C --- XBAR  = AVERAGE CHANGE WITHOUT REGARD TO SIGN.
C --- XBAR2 = AVERAGE CHANGE WITH REGARD TO SIGN.
C --- XSQ   = AVERAGE CHANGE SQUARED WITHOUT REGARD TO SIGN.
C --- XSD   = STD. DEV. OF CHANGES WITH REGARD TO SIGN.
C --- NY    = NUMBER OF SPANS IN ONE YEAR.
C --- XBAR,XBAR2,XSQ, AND XSD ARE CALCULATED FOR SPANS 1 TO NY.
C --- IF THE ADJUSTMENT IS ADDITIVE, THE CHANGES ARE DIFFERENCES.
C --- IF THE ADJUSTMENT IS MULTIPLICATIVE, THEY ARE PERCENT CHANGES.
c-----------------------------------------------------------------------
      DOUBLE PRECISION ZERO,ONE
      PARAMETER(ZERO=0D0,ONE=1D0)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'goodob.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION c,xcount,X,Xbar,Xbar2,Xsd,Xsq
      INTEGER I,Iopt,J,k,kj,l
      DIMENSION X(*),Xbar(*),Xbar2(*),Xsq(*),Xsd(*)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
      DO k=1,Ny
       Xbar(k)=ZERO
       xcount=ZERO
       IF(Iopt.le.1)THEN
        Xbar2(k)=ZERO
        Xsd(k)=ZERO
       END IF
       kj=J-k
       DO l=I,kj
        IF(Gudval(l))THEN
         c=X(l+k)-X(l)
         IF(Muladd.eq.0)c=c*100D0/X(l)
         Xbar(k)=Xbar(k)+abs(c)
         IF(Iopt.le.1)Xbar2(k)=Xbar2(k)+c
         xcount=xcount+ONE
        END IF
       END DO
       IF(xcount.gt.ZERO)THEN
        Xbar(k)=Xbar(k)/xcount
       ELSE
        Xbar(k)=DNOTST
       END IF
       IF(Iopt.ne.3)THEN
        IF(Iopt.ne.1)THEN
         IF(dpeq(Xbar(k),DNOTST))THEN
          Xsq(k)=DNOTST
         ELSE
          Xsq(k)=Xbar(k)*Xbar(k)
         END IF
         IF(Iopt.eq.2)GO TO 10
        END IF
        IF(xcount.gt.ZERO)THEN
         Xbar2(k)=Xbar2(k)/xcount
        ELSE
         Xbar2(k)=DNOTST
        END IF
        Xsd(k)=ZERO
        IF(Muladd.eq.0)THEN
         DO l=I,kj
          IF(Gudval(l))Xsd(k)=Xsd(k)+
     &                        ((X(l+k)-X(l))/X(l)*100D0-Xbar2(k))**2
         END DO
        ELSE
         DO l=I,kj
          Xsd(k)=Xsd(k)+(X(l+k)-X(l)-Xbar2(k))**2
         END DO
        END IF
        IF(Xsd(k).gt.ZERO)Xsd(k)=sqrt(Xsd(k)/xcount)
       END IF
   10  CONTINUE  
      END DO
      RETURN
      END
