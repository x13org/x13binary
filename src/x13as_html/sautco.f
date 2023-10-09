**==sautco.f    processed by SPAG 4.03F  at 09:53 on  1 Mar 1994
      SUBROUTINE sautco(X,Cxx,N1,N2,N,Lagh1,R,Good)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      LOGICAL Good,dpeq
      DOUBLE PRECISION cn,R,X,xmean,cx0,Cxx
      INTEGER Lagh1,N,N1,N2
      EXTERNAL dpeq
C*** End of declarations inserted by SPAG
C     THE OUTPUTS ARE AUTOCOVARIANCES (CXX(I), I = 0, LAGH) AND
C     AUTO CORRELATIONS (NORMALIZED COVARIANCES).
C     TUKEY-HANNING TAPER ROUTINE ADDED BY B. C. MONSELL 7-88
      DIMENSION X(*),Cxx(*)
      DIMENSION cn(1001)
C     MEAN DELETION
      CALL smeadl(X,N1,N2,N,xmean)
C     APPLY THE TUKEY-HANNING TAPER PRIOR TO CALCULATING THE SPECTRUM
      IF(R.gt.0D0)CALL taper(X,N1,N2,R)
C     AUTO COVARIANCE COMPUTATION
C     COMMON SUBROUTINE CALL
      CALL crosco(X,X,N1,N2,N,Cxx,Lagh1)
C     NORMALIZATION
      cx0=Cxx(1)
      IF(dpeq(cx0,0D0))THEN
       good=.false.
      ELSE
C     COMMON SUBROUTINE CALL
       CALL cornom(Cxx,cn,Lagh1,cx0,cx0)
      END IF
      RETURN
      END
