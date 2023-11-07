      SUBROUTINE invMat( mA, nA, mB, nB )
c-----------------------------------------------------------------------
c     invMat.f, Release 1, Subroutine Version 1.1, Modified 15 Sep 2005.
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 11 Apr 2005.
c     Modified by REG, on 15 Sep 2005, to change size of dummy argument,
c       dtrmnt, passed to dppdi().
c-----------------------------------------------------------------------
c     This subroutine calculates the inverse mB of a symmetric matrix mA
c     mB = mA^(-1) where nA and nB contain the dimensions of mA and mB.
c-----------------------------------------------------------------------
c Name  Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c mA        d     input matrix to be inverted
c mB        d     output matrix to contain inverted mA
c nA        i     size (rows,columns) of mA matrix
c nB        i     size (rows,columns) of mB matrix
c-----------------------------------------------------------------------
c Name  Type Description (local Variables)
c-----------------------------------------------------------------------
c dtrmnt d  determinate of matrix mp (=mA)
c i,j       i     index variables for do loops
c info      i     errors returned by dppfa()
c JOB       i     constant parameter passed to dppdi()
c k         i     index into matrix mp
c mp        d     upper triangle of symetric matrix mA in vector form
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER nA(2), nB(2)
      DOUBLE PRECISION mA( nA(1), nA(2) ), mB( nA(1), nA(2) )

c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i, j, k, info, JOB
      DOUBLE PRECISION dtrmnt(2), mp( nA(1)*(nA(1)+1)/2 )
      PARAMETER (JOB=1)

c-----------------------------------------------------------------------
c     Check for valid matrix inversion.
c-----------------------------------------------------------------------
      IF (( nA(1) .eq. nA(2) ) .and. ( nA(1) .gt. 0)) THEN

c     ------------------------------------------------------------------
c     Move lower triangular part of mA to mp for processing 
c     by dppfa and dppfi.
c     ------------------------------------------------------------------
       k=0
       DO j=1,nA(1)
        DO i=1,j
         k=k+1
         mp(k)=mA(i,j)
        END DO
       END DO

c     ------------------------------------------------------------------
c     Check that mp (=mA) is positive definite and invert.
c     ------------------------------------------------------------------
       CALL dppfa( mp, nA(1), info )
       IF ( info .eq. 0 ) THEN
        CALL dppdi( mp, nA(1), dtrmnt, JOB )

c     ------------------------------------------------------------------
c     Move the inverted matrix to mB.
c     ------------------------------------------------------------------
        k=0
        DO j=1,nA(1)
         DO i=1,j
          k=k+1
          mB(i,j)=mp(k)
          mB(j,i)=mp(k)
         END DO
        END DO

c     ------------------------------------------------------------------
c     Create dimensions of inverted matrix mB.
c     ------------------------------------------------------------------
        nB(1) = nA(1)
        nB(2) = nA(2)

c-----------------------------------------------------------------------
C     mA matrix is not positive definite.
c-----------------------------------------------------------------------
       ELSE
        nB(1) = 0
        nB(2) = 0
       END IF

c-----------------------------------------------------------------------
c     Invalid matrix inversion.
c-----------------------------------------------------------------------
      ELSE
       nB(1) = 0
       nB(2) = 0
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE getInvDia( mA, nA, iDiag, sDiag, nDiag )
c-----------------------------------------------------------------------
c     getInvDia, Release 1, Subroutine Version 1.0, Created 21 Feb 2006.
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 21 Feb 2006.
c-----------------------------------------------------------------------
c     This subroutine calculates a diagonal entry (index of iDiag) 
c     in the inverse of a symmetric matrix mA where nA and nDiag 
c     contain the dimensions of mA and sDiag.
c-----------------------------------------------------------------------
c Name  Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c iDiag  i   index of diagonal element in inverse of mA to be calculated
c mA     d   input matrix to be inverted
c nA     i   size (rows,columns) of mA matrix
c nDiag  i   size (rows,columns) of sDiag scallar
c sDiag  d   output matrix to contain inverted mA
c-----------------------------------------------------------------------
c Name  Type Description (local Variables)
c-----------------------------------------------------------------------
c i,j       i     index variables for do loops
c info      i     errors returned by dppfa()
c k         i     index into matrix mp
c mp        d     upper triangle of symetric matrix mA in vector form
c ONE   d   constant parameter
c vB    d   vector containing e_iDiag and passed dppsl() in order
c           to solve for column of inverse(mA) that contains desired 
c           diagonal entry
c ZERO  d   constant parameter
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER nA(2), iDiag, nDiag(2)
      DOUBLE PRECISION mA( nA(1), nA(2) ), sDiag
      
c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i, j, k, info
      DOUBLE PRECISION mp( nA(1)*(nA(1)+1)/2 ), vB( nA(1) )
      DOUBLE PRECISION ONE, ZERO
      PARAMETER ( ONE=1.0d0, ZERO=0.0d0 )
      
c-----------------------------------------------------------------------
c     Check for valid matrix inversion.
c-----------------------------------------------------------------------
      IF (( nA(1) .eq. nA(2) ) .and. ( nA(1) .gt. 0) .and.
     &    ( iDiag .ge. 1 ) .and. ( iDiag .le. nA(1) )) THEN

c     ------------------------------------------------------------------
c     Move lower triangular part of mA to mp for processing 
c     by dppfa and dppfi.
c     ------------------------------------------------------------------
       k=0
       DO j=1,nA(1)
        DO i=1,j
         k=k+1
         mp(k)=mA(i,j)
        END DO
       END DO

c     ------------------------------------------------------------------
c     Check that mp (=mA) is positive definite 
c     and solve for diagonal element.
c     ------------------------------------------------------------------
       CALL dppfa( mp, nA(1), info )
       IF ( info .eq. 0 ) THEN
c     ------------------------------------------------------------------
c     Initialize vB to e_iDiag.
c     ------------------------------------------------------------------
        DO j=1,nA(1)
         vB(j)=ZERO
        END DO
        vB( iDiag ) = ONE
        
c     ------------------------------------------------------------------
c     Call dppsl using vB to e_iDiag in order to solve for iDiag column
c     in inverse of mA.
c     ------------------------------------------------------------------
        CALL dppsl( mp, nA(1), vB, .false. )
        
c     ------------------------------------------------------------------
c     Retrieve the desired diagonal entry.
c     ------------------------------------------------------------------
        sDiag = vB( iDiag )
        nDiag(1) = 1
        nDiag(2) = 1
       
       ELSE
        nDiag(1) = 0
        nDiag(2) = 0
       END IF
      END IF

      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE invLTMat( mLMat, nLMat, mInvLMat, nInvLMat )

c-----------------------------------------------------------------------
c     invLMat.f, Release 1, Subroutine Version 1.0, Created 14 Apr 2006.
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 11 Apr 2005.
c-----------------------------------------------------------------------
c     This subroutine calculates the inverse mInvLMAT of a lower 
c     diagonal matrix mLMat using the following equation
c     mInvLMat = mLmat' x (mLMat*mLMat')^(-1) where nLMat and nInvLMat 
c     contain the dimensions of mLMat and mInvLMat.
c-----------------------------------------------------------------------
c Name  Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c mInvLMat d output matrix to containing inverted mLMat
c mLMat  d   input matrix to be inverted
c nInvLMat i size (rows,columns) of mInvLMat matrix
c nLMat  i   size (rows,columns) of mLMat matrix
c-----------------------------------------------------------------------
c Name  Type Description (local Variables)
c-----------------------------------------------------------------------
c mTemp1 d   working matrix to contain mLMat x mLMat'
c mTemp2 d   working matrix to contain inverse of mTemp1
c nTemp1 i   size (rows,columns) of mTemp1 matrix
c nTemp2 i   size (rows,columns) of mTemp2 matrix
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER nLMat(2), nInvLMat(2)
      DOUBLE PRECISION mLMat( nLMat(1), nLMat(2) )
      DOUBLE PRECISION mInvLMat( nLMat(1), nLMat(2) )
      
c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER nTemp1(2), nTemp2(2)
      DOUBLE PRECISION mTemp1( nLMat(1), nLMat(1) )
      DOUBLE PRECISION mTemp2( nLMat(1), nLMat(1) )
      
c-----------------------------------------------------------------------
c     Calculate positive definite matrix to be inverted mLMat x mLMat'
c-----------------------------------------------------------------------
      CALL mulMatTr( mLMat, nLMat, mLMat, nLMat, mTemp1, nTemp1 )

c-----------------------------------------------------------------------
c     Invert positive definite matrix
c-----------------------------------------------------------------------
      CALL invMat( mTemp1, nTemp1 , mTemp2, nTemp2 )

c-----------------------------------------------------------------------
c     Calculate inverse of lower diagonal input matrix
c-----------------------------------------------------------------------
      CALL mulTrMat( mLMat, nLMat, mTemp2, nTemp2, mInvLMat, nInvLMat )
      
      RETURN
      END