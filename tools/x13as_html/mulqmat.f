c-----------------------------------------------------------------------
c     mulQMat.f, Release 1, Subroutine Version 1.1, Modified 13 Jan 2006.
c-----------------------------------------------------------------------
      SUBROUTINE mulQMat( mA, nA, mB, nB, mC, nC )
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 09 Jun 2005.
c     Modified by REG, on 13 Jan 2006, to combine with other quadratic 
c       matrix multiplication utility mulQMatTr().
c-----------------------------------------------------------------------
c     This subroutine calculates the matrix quadratic product of 
c     mC = mA x mB x mA' where mA' represents the transpose of mA and 
c     where nA, nB, and nC contain the dimensions of mA, mB, and mC.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c mA        d     first input matrix to pre/post multipy mB by
c mB        d     second input matrix
c mC        d     matrix output result of mA x mB x mA'
c nA        i     size (rows,columns) of mA matrix
c nB        i     size (rows,columns) of mB matrix
c nC        i     size (rows,columns) of mC matrix
c-----------------------------------------------------------------------
c Name   Type Description (local Variables)
c-----------------------------------------------------------------------
c mTemp     d     temporary result of mA x mB
c nSave     i     identifies default size of large matrices
c                 that are saved (not dynamic)
c nTemp     i     size (rows,columns) of mTemp matrix
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INTEGER nA(2), nB(2), nC(2)
      DOUBLE PRECISION mA( nA(1), nA(2) ), mB( nB(1), nB(2) ), 
*     &                 mC( nA(2) ,nA(2)  )
     &                 mC( nA(1) ,nA(1)  )

c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER nSave, nTemp(2)
      PARAMETER (nSave=POBS*POBS)
c     DOUBLE PRECISION, SAVE :: mTemp( nSave )
      DOUBLE PRECISION mTemp( nSave )
      COMMON /QMATPROC/ mTemp

c-----------------------------------------------------------------------
c     Check for valid matrix multiplication.
c-----------------------------------------------------------------------
      IF (( nA(2) .eq. nB(1) ) .and. ( nB(1) .eq. nB(2) )) THEN

c-----------------------------------------------------------------------
c     Perform matrix multiply of mTemp = mA x mB.
c-----------------------------------------------------------------------
       CALL mulMat( mA, nA, mB, nB, mTemp, nTemp )

c-----------------------------------------------------------------------
c     Perform matrix transpose multiply of mC = mTemp x mA'.
c-----------------------------------------------------------------------
       CALL mulMatTr( mTemp, nTemp, mA, nA, mC, nC )

c-----------------------------------------------------------------------
c     Invalid matrix multiplication.
c-----------------------------------------------------------------------
      ELSE
       nC(1) = 0
       nC(2) = 0
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE mulQMatTr( mA, nA, mB, nB, mC, nC )
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 09 Jun 2005.
c     Modified by REG, on 15 Sep 2005, to correct mC dimensions.
c-----------------------------------------------------------------------
c     This subroutine calculates the matrix quadratic product of 
c     mC = mA' x mB x mA where mA' represents the transpose of mA and 
c     where nA, nB, and nC contain the dimensions of mA, mB, and mC.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c mA        d     first input matrix to pre/post multipy mB by
c mB        d     second input matrix
c mC        d     matrix output result of mA' x mB x mA
c nA        i     size (rows,columns) of mA matrix
c nB        i     size (rows,columns) of mB matrix
c nC        i     size (rows,columns) of mC matrix
c-----------------------------------------------------------------------
c Name   Type Description (local Variables)
c-----------------------------------------------------------------------
c mTemp     d     temporary result of mA' x mB
c nSave     i     identifies default size of large matrices
c                 that are saved (not dynamic)
c nTemp     i     size (rows,columns) of mTemp matrix
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INTEGER nA(2), nB(2), nC(2)
      DOUBLE PRECISION mA( nA(1), nA(2) ), mB( nB(1), nB(2) ), 
     &                 mC( nA(2), nA(2) )

c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i, j, nSave, nTemp(2)
      PARAMETER (nSave=POBS*POBS)
c     DOUBLE PRECISION, SAVE :: mTemp( nSave )
      DOUBLE PRECISION mTemp( nSave )
      COMMON /QMATPROC/ mTemp

c-----------------------------------------------------------------------
c     Check for valid matrix multiplication.
c-----------------------------------------------------------------------
      IF (( nA(1) .eq. nB(1) ) .and. ( nB(1) .eq. nB(2) )) THEN

c-----------------------------------------------------------------------
c     Perform matrix multiply of mTemp = mA' x mB.
c-----------------------------------------------------------------------
       CALL mulTrMat( mA, nA, mB, nB, mTemp, nTemp )

c-----------------------------------------------------------------------
c     Perform matrix multiply of mC = mTemp x mA.
c-----------------------------------------------------------------------
       CALL mulMat( mTemp, nTemp, mA, nA, mC, nC )

c-----------------------------------------------------------------------
c     Invalid matrix multiplication.
c-----------------------------------------------------------------------
      ELSE
       nC(1) = 0
       nC(2) = 0
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE mulQdMat( dA, nA, mB, nB, mC, nC )
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 13 Jan 2006.
c-----------------------------------------------------------------------
c     This subroutine calculates the matrix quadratic product of 
c     mC = mA x mB x mA' where mA' represents the transpose of mA, 
c     where dA represents the constant diagonal form of mA, and
c     where nA, nB, and nC contain the dimensions of mA, mB, and mC.
c-----------------------------------------------------------------------
c Name  Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c dA    d   first input matrix to pre/post multipy mB by where 
c           dA = [ mA(1,1), ..., mA(1,sA) ], sA = nA(2)-nA(1)+1
c mB    d   second input matrix
c mC    d   matrix output result of mA x mB x mA'
c nA    i   size (rows,columns) of mA matrix
c nB    i   size (rows,columns) of mB matrix
c nC    i   size (rows,columns) of mC matrix
c-----------------------------------------------------------------------
c Name   Type Description (local Variables)
c-----------------------------------------------------------------------
c mTemp d   temporary result of mA x mB
c nSave i   identifies default size of large matrices
c           that are saved (not dynamic)
c nTemp i   size (rows,columns) of mTemp matrix
c-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'srslen.prm'
c     ------------------------------------------------------------------
c     added by BCM to correctly dimension variables 
c     ------------------------------------------------------------------
      INTEGER pdA
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER nA(2), nB(2), nC(2)
      DOUBLE PRECISION dA(nA(2)-nA(1)+1), mB( nB(1), nB(2) ),
     &                 mC( nA(1) ,nA(1) )
      
c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER nSave, nTemp(2)
      PARAMETER (nSave=POBS*POBS)
c     DOUBLE PRECISION, SAVE :: mTemp( nSave )
      DOUBLE PRECISION mTemp( nSave )
      COMMON /QMATPROC/ mTemp

c-----------------------------------------------------------------------
c     Check for valid matrix multiplication.
c-----------------------------------------------------------------------
      IF (( nA(2) .eq. nB(1) ) .and. ( nB(1) .eq. nB(2) )) THEN

c-----------------------------------------------------------------------
c     Perform matrix multiply of mTemp = mA x mB using dA.
c-----------------------------------------------------------------------
        pdA = max(nA(2)-nA(1)+1, 1)
        CALL mulDMat( dA, nA, mB, nB, mTemp, nTemp, pdA )

c-----------------------------------------------------------------------
c     Perform matrix transpose multiply of mC = mTemp x mA' using dA.
c-----------------------------------------------------------------------
        CALL mulMatDTr( mTemp, nTemp, dA, nA, mC, nC, pdA )

c-----------------------------------------------------------------------
c     Invalid matrix multiplication.
c-----------------------------------------------------------------------
      ELSE
        nC(1)=0
        nC(2)=0
      END IF
c     ------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE mulQdMatTr( dA, nA, mB, nB, mC, nC )
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 13 Jan 2006.
c-----------------------------------------------------------------------
c     This subroutine calculates the matrix quadratic product of 
c     mC = mA' x mB x mA where mA' represents the transpose of mA, 
c     where dA represents the constant diagonal form of mA, and
c     where nA, nB, and nC contain the dimensions of mA, mB, and mC.
c-----------------------------------------------------------------------
c Name  Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c dA    d   first input matrix to pre/post multipy mB by where 
c           dA = [ mA(1,1), ..., mA(1,sA) ], sA = nA(2)-nA(1)+1
c mB    d   second input matrix
c mC    d   matrix output result of mA' x mB x mA
c nA    i   size (rows,columns) of mA matrix
c nB    i   size (rows,columns) of mB matrix
c nC    i   size (rows,columns) of mC matrix
c-----------------------------------------------------------------------
c Name   Type Description (local Variables)
c-----------------------------------------------------------------------
c mTemp d   temporary result of mA' x mB
c nSave i   identifies default size of large matrices
c           that are saved (not dynamic)
c nTemp i   size (rows,columns) of mTemp matrix
c-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'srslen.prm'
c     ------------------------------------------------------------------
c     added by BCM to correctly dimension variables 
c     ------------------------------------------------------------------
      INTEGER pdA
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER nA(2), nB(2), nC(2)
      DOUBLE PRECISION dA(nA(2)-nA(1)+1), mB( nB(1), nB(2) ),
*     &                 mC( nA(1) ,nA(1) )
     &                 mC( nA(2) ,nA(2) )
      
c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER nSave, nTemp(2)
      PARAMETER (nSave=POBS*POBS)
c     DOUBLE PRECISION, SAVE :: mTemp( nSave )
      DOUBLE PRECISION mTemp( nSave )
      COMMON /QMATPROC/ mTemp

c-----------------------------------------------------------------------
c     Check for valid matrix multiplication.
c-----------------------------------------------------------------------
      IF (( nA(1) .eq. nB(1) ) .and. ( nB(1) .eq. nB(2) )) THEN

c-----------------------------------------------------------------------
c     Perform matrix transpose multiply of mTemp = mA' x mB using dA.
c-----------------------------------------------------------------------
        pdA = max(nA(2)-nA(1)+1, 1)
        CALL mulDTrMat( dA, nA, mB, nB, mTemp, nTemp, pdA )

c-----------------------------------------------------------------------
c     Perform matrix multiply of mC = mTemp x mA using dA.
c-----------------------------------------------------------------------
*        write(6,*) ' nTemp = ',nTemp(1),nTemp(2)
*        write(6,*) ' na = ',na(1),na(2)
        CALL mulMatD( mTemp, nTemp, dA, nA, mC, nC, pdA )

c-----------------------------------------------------------------------
c     Invalid matrix multiplication.
c-----------------------------------------------------------------------
      ELSE
        nC(1)=0
        nC(2)=0
      END IF
c     ------------------------------------------------------------------
      RETURN
      END