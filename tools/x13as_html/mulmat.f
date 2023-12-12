c-----------------------------------------------------------------------
c     mulMat.f, Release 1, Subroutine Version 1.1, Modified 13 Jan 2006.
c-----------------------------------------------------------------------
      SUBROUTINE mulMat( mA, nA, mB, nB, mC, nC )
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 04 Apr 2005.
c     Modified by REG, on 13 Jan 2006, to combine with other matrix
c       multiplication utilities mulMatTr() and mulTrMat().
c-----------------------------------------------------------------------
c     This subroutine calculates the matrix product of mC = mA x mB 
c     where nA, nB, and nC contain the dimensions of mA, mB, and mC.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c mA        d     first input matrix to be multiplied
c mB        d     second input matrix to be multiplied
c mC        d     matrix output result of mA x mB
c nA        i     size (rows,columns) of mA matrix
c nB        i     size (rows,columns) of mB matrix
c nC        i     size (rows,columns) of mC matrix
c-----------------------------------------------------------------------
c Name   Type Description (local Variables)
c-----------------------------------------------------------------------
c ddot      d     external function reference
c i,j       i     index variables for do loops
c vD        d     used as temporary storage for a row of mA
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER nA(2), nB(2), nC(2)
      DOUBLE PRECISION mA( nA(1), nA(2) ), mB( nB(1), nB(2) ), 
     &                 mC( nA(1), nB(2) )

c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i, j
      DOUBLE PRECISION vD( nA(2) ), ddot

c-----------------------------------------------------------------------
c     Check for valid matrix multiplication.
c-----------------------------------------------------------------------
      IF ( nA(2) .eq. nB(1) ) THEN

c-----------------------------------------------------------------------
c     Establish dimensions of mC matrix.
c-----------------------------------------------------------------------
       nC(1) = nA(1)
       nC(2) = nB(2)

c-----------------------------------------------------------------------
c     Perform matrix multiply of mC = mA x mB.
c-----------------------------------------------------------------------
       DO i = 1, nC(1)

c     ------------------------------------------------------------------
c     Move row i of A to temporary vector vD.
c     ------------------------------------------------------------------
        DO j = 1, nA(2)
         vD(j) = mA( i, j )
        END DO

c     ------------------------------------------------------------------
c     Compute dot product of mA row i (from vD) x mB column j.
c     ------------------------------------------------------------------
        DO j = 1, nC(2)
         mC(i,j) = ddot( nA(2), vD(1), 1, mB(1,j), 1  )
        END DO
       END DO

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
      SUBROUTINE mulMatTr( mA, nA, mB, nB, mC, nC )
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 09 Jun 2005.
c-----------------------------------------------------------------------
c     This subroutine calculates the matrix product of mC = mA x mB' 
c     where mB' represents the transpose of mB and 
c     where nA, nB, and nC contain the dimensions of mA, mB, and mC.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c mA        d     first input matrix to be multiplied
c mB        d     second input matrix to be multiplied after transpose
c mC        d     matrix output result of mA x mB'
c nA        i     size (rows,columns) of mA matrix
c nB        i     size (rows,columns) of mB matrix
c nC        i     size (rows,columns) of mC matrix
c-----------------------------------------------------------------------
c Name   Type Description (local Variables)
c-----------------------------------------------------------------------
c ddot      d     external function reference
c i,j       i     index variables for do loops
c vD        d     used as temporary storage for a row of mA
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER nA(2), nB(2), nC(2)
      DOUBLE PRECISION mA( nA(1), nA(2) ), mB( nB(1), nB(2) ), 
     &                 mC( nA(1), nB(1) )

c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i, j
      DOUBLE PRECISION vD( nA(2) ), ddot

c-----------------------------------------------------------------------
c     Check for valid matrix multiplication.
c-----------------------------------------------------------------------
      IF ( nA(2) .eq. nB(2) ) THEN

c-----------------------------------------------------------------------
c     Establish dimensions of mC matrix.
c-----------------------------------------------------------------------
       nC(1) = nA(1)
       nC(2) = nB(1)

c-----------------------------------------------------------------------
c     Perform matrix multiply of mC = mA x mB'.
c-----------------------------------------------------------------------
       DO i = 1, nC(1)

c     ------------------------------------------------------------------
c     Move row i of A to temporary vector vD.
c     ------------------------------------------------------------------
        DO j = 1, nA(2)
         vD(j) = mA( i, j )
        END DO

c     ------------------------------------------------------------------
c     Compute dot product of mA row i (from vD) x mB row j.
c     ------------------------------------------------------------------
        DO j = 1, nC(2)
         mC(i,j) = ddot( nA(2), vD(1), 1, mB(j,1), nB(1)  )
        END DO
       END DO

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
      SUBROUTINE mulTrMat( mA, nA, mB, nB, mC, nC )
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, 23 Aug 2005.
c-----------------------------------------------------------------------
c     This subroutine calculates the matrix product of mC = mA' x mB 
c     where mA' represents the transpose of mA and 
c     where nA, nB, and nC contain the dimensions of mA, mB, and mC.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c mA        d     first input matrix to be multiplied
c mB        d     second input matrix to be multiplied after transpose
c mC        d     matrix output result of mA' x mB
c nA        i     size (rows,columns) of mA matrix
c nB        i     size (rows,columns) of mB matrix
c nC        i     size (rows,columns) of mC matrix
c-----------------------------------------------------------------------
c Name   Type Description (local Variables)
c-----------------------------------------------------------------------
c ddot      d     external function reference
c i,j       i     index variables for do loops
c vD        d     used as temporary storage for a row of mA
c-----------------------------------------------------------------------
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER nA(2), nB(2), nC(2)
      DOUBLE PRECISION mA( nA(1), nA(2) ), mB( nB(1), nB(2) ), 
     &                 mC( nA(2), nB(2) )

c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i, j
      DOUBLE PRECISION vD( nA(1) ), ddot

c-----------------------------------------------------------------------
c     Check for valid matrix multiplication.
c-----------------------------------------------------------------------
      IF ( nA(1) .eq. nB(1) ) THEN

c-----------------------------------------------------------------------
c     Establish dimensions of mC matrix.
c-----------------------------------------------------------------------
       nC(1) = nA(2)
       nC(2) = nB(2)

c-----------------------------------------------------------------------
c     Perform matrix multiply of mC = mA' x mB.
c-----------------------------------------------------------------------
       DO i = 1, nC(1)

c     ------------------------------------------------------------------
c     Move column i of A to temporary vector vD.
c     ------------------------------------------------------------------
        DO j = 1, nA(1)
         vD(j) = mA(j,i)
        END DO

c     ------------------------------------------------------------------
c     Compute dot product of mA column i (from vD) x mB column j.
c     ------------------------------------------------------------------
        DO j = 1, nC(2)
         mC(i,j) = ddot( nA(1), vD(1), 1, mB(1,j), 1  )
        END DO
       END DO

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
      SUBROUTINE mulDMat( dA, nA, mB, nB, mC, nC, pdA )
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 13 Jan 2006.
c-----------------------------------------------------------------------
c     This subroutine calculates the matrix product of mC = mA x mB 
c     where mA has a constant diagonal form as represented by dA and 
c     where nA, nB, and nC contain the dimensions of mA, mB, and mC.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c dA    d   diagonal form of first input matrix to be multiplied
c           where dA = [ mA[1,1], ... mA[1,sA] ], sA = nA(2)-nA(1)+1
c mB    d   second input matrix to be multiplied
c mC    d   matrix output result of mA x mB
c nA    i   size (rows,columns) of mA matrix
c nB    i   size (rows,columns) of mB matrix
c nC    i   size (rows,columns) of mC matrix
c-----------------------------------------------------------------------
c Name   Type Description (local Variables)
c-----------------------------------------------------------------------
c ddot  d   external function reference
c i,j   i   index variables for do loops
c sA    i   size of dA vector
c-----------------------------------------------------------------------
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     added by BCM to correctly dimension variables 
c     ------------------------------------------------------------------
      INTEGER pdA
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER nA(2), nB(2), nC(2)
      DOUBLE PRECISION dA( pdA ), mB( nB(1), nB(2) ),
     &                 mC( nA(1), nB(2) )
      
c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i, j, sA
      DOUBLE PRECISION ddot
      
c-----------------------------------------------------------------------
c     Check for valid matrix multiplication.
c-----------------------------------------------------------------------
      IF (( nA(2) .eq. nB(1) ).and.( nA(2)-nA(1) .ge. 0 )) THEN
      
c-----------------------------------------------------------------------
c     Establish dimensions of mC matrix.
c-----------------------------------------------------------------------
       nC(1) = nA(1)
       nC(2) = nB(2)
       sA = nA(2)-nA(1)+1
      
c-----------------------------------------------------------------------
c     Perform matrix multiply of mC = mA' x mB.
c-----------------------------------------------------------------------
       DO i=1, nC(1)

c     ------------------------------------------------------------------
c     Compute dot product of mA row i (from dA) x mB column j.
c     ------------------------------------------------------------------
        DO j=1, nC(2)
         mC(i,j) = ddot( sA, dA(1), 1, mB(i,j), 1 )
        END DO
       END DO
      
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
      SUBROUTINE mulMatDTr( mA, nA, dB, nB, mC, nC, pdB )
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 13 Jan 2006.
c-----------------------------------------------------------------------
c     This subroutine calculates the matrix product of mC = mA x mB' 
c     where mB' represents the transpose of mB and
c     where mB has a constant diagonal form as represented by dB and 
c     where nA, nB, and nC contain the dimensions of mA, mB, and mC.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c mA    d   first input matrix to be multiplied
c dB    d   diagonal form of second input matrix to be multiplied
c           where dB = [ mB[1,1], ... mB[1,sB] ], sB = nB(2)-nB(1)+1
c mC    d   matrix output result of mA x mB'
c nA    i   size (rows,columns) of mA matrix
c nB    i   size (rows,columns) of mB matrix
c nC    i   size (rows,columns) of mC matrix
c-----------------------------------------------------------------------
c Name   Type Description (local Variables)
c-----------------------------------------------------------------------
c ddot  d   external function reference
c i,j   i   index variables for do loops
c sB    i   size of dB vector
c vA        d     used as temporary storage for a row of mA
c-----------------------------------------------------------------------
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     added by BCM to correctly dimension variables 
c     ------------------------------------------------------------------
      INTEGER pdB
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER nA(2), nB(2), nC(2)
      DOUBLE PRECISION mA( nA(1), nA(2) ), dB( pdB ), mC( nA(1), nB(1) )
      
c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i, j, sB
      DOUBLE PRECISION  vA( nA(2) ), ddot
      
c-----------------------------------------------------------------------
c     Check for valid matrix multiplication.
c-----------------------------------------------------------------------
      IF (( nA(2) .eq. nB(2) ).and.( nB(2)-nB(1) .ge. 0 )) THEN
      
c-----------------------------------------------------------------------
c     Establish dimensions of mC matrix.
c-----------------------------------------------------------------------
       nC(1) = nA(1)
       nC(2) = nB(1)
       sB = nB(2)-nB(1)+1
      
c-----------------------------------------------------------------------
c     Perform matrix multiply of mC = mA x mB'.
c-----------------------------------------------------------------------
       DO i=1, nC(1)

c     ------------------------------------------------------------------
c     Move row i of A to temporary vector vA.
c     ------------------------------------------------------------------
        DO j = 1, nA(2)
         vA(j) = mA( i, j )
        END DO

c     ------------------------------------------------------------------
c     Compute dot product of mA row i (from vA) x mB row j (from dB).
c     ------------------------------------------------------------------
        DO j=1, nC(2)
         mC(i,j) = ddot( sB, vA(j), 1, dB(1), 1 )
        END DO
       END DO
      
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
      SUBROUTINE mulDTrMat( dA, nA, mB, nB, mC, nC, pdA )
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 13 Jan 2006.
c-----------------------------------------------------------------------
c     This subroutine calculates the matrix product of mC = mA' x mB 
c     where mA' represents the transpose of mA and
c     where mA has a constant diagonal form as represented by dA and 
c     where nA, nB, and nC contain the dimensions of mA, mB, and mC.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c dA    d   diagonal form of first input matrix to be multiplied
c           where dA = [ mA[1,1], ... mA[1,sA] ], sA = nA(2)-nA(1)+1
c mB    d   second input matrix to be multiplied
c mC    d   matrix output result of mA' x mB
c nA    i   size (rows,columns) of mA matrix
c nB    i   size (rows,columns) of mB matrix
c nC    i   size (rows,columns) of mC matrix
c-----------------------------------------------------------------------
c Name   Type Description (local Variables)
c-----------------------------------------------------------------------
c ddot  d   external function reference
c i,j   i   index variables for do loops
c nn    i   count variable
c sA    i   size of dA vector
c dRA       d     reversed version of dA
c-----------------------------------------------------------------------
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     added by BCM to correctly dimension variables 
c     ------------------------------------------------------------------
      INTEGER pdA
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER nA(2), nB(2), nC(2)
      DOUBLE PRECISION dA( pdA ), mB( nB(1), nB(2) ),
     &                 mC( nA(2), nB(2) )
      
c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i, j, nn, sA
      DOUBLE PRECISION dRA( nA(2)-nA(1)+1 ), ddot
      
c-----------------------------------------------------------------------
c     Check for valid matrix multiplication.
c-----------------------------------------------------------------------
      IF (( nA(1) .eq. nB(1) ).and.( nA(2)-nA(1) .ge. 0 )) THEN
      
c-----------------------------------------------------------------------
c     Establish dimensions of mC matrix.
c-----------------------------------------------------------------------
       nC(1) = nA(2)
       nC(2) = nB(2)

c     ------------------------------------------------------------------
c     Create reversed version of dA.
c     ------------------------------------------------------------------
       sA = nA(2)-nA(1)+1
       DO j=1,sA
        dRA( sA-j+1 )=dA( j )
       END DO
       
c-----------------------------------------------------------------------
c     Perform matrix multiply of mC = mA' x mB.
c-----------------------------------------------------------------------
       DO i=1, nC(1)

c     ------------------------------------------------------------------
c     Compute dot product of mA column i (from dRA) x mB column j.
c     ------------------------------------------------------------------
        DO j=1, nC(2)
         IF ( i .le. nA(1) ) THEN
           nn = min(i,sA)
c          mC(i,j) = ddot( nn, dA(1), 1, mB(i,j), -1 )
           mC(i,j) = ddot( nn, dRA(sA-nn+1), 1, mB(i-nn+1,j), 1 )
         ELSE
           nn = nA(2)-i+1
c          mC(i,j) = ddot( nn, dA(i-nA(1)+1), 1, mB(nB(1),j), -1 )
           mC(i,j) = ddot( nn, dRA(1), 1, mB(nB(1)-nn+1,j), 1 )
         END IF
        END DO
       END DO
      
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
      SUBROUTINE mulMatD( mA, nA, dB, nB, mC, nC, pdB )
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 13 Jan 2006.
c-----------------------------------------------------------------------
c     This subroutine calculates the matrix product of mC = mA x mB 
c     where mB has a constant diagonal form as represented by dB and 
c     where nA, nB, and nC contain the dimensions of mA, mB, and mC.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c mA    d   first input matrix to be multiplied
c dB    d   diagonal form of second input matrix to be multiplied
c           where dB = [ mB[1,1], ... mB[1,sB] ], sB = nB(2)-nB(1)+1
c mC    d   matrix output result of mA x mB
c nA    i   size (rows,columns) of mA matrix
c nB    i   size (rows,columns) of mB matrix
c nC    i   size (rows,columns) of mC matrix
c-----------------------------------------------------------------------
c Name   Type Description (local Variables)
c-----------------------------------------------------------------------
c ddot  d   external function reference
c i,j   i   index variables for do loops
c nn    i   count variable
c sB    i   size of dB vector
c dRA       d     reversed version of a row in mA
c-----------------------------------------------------------------------
      IMPLICIT NONE
c     ------------------------------------------------------------------
c     added by BCM to correctly dimension variables 
c     ------------------------------------------------------------------
      INTEGER pdB
c-----------------------------------------------------------------------
c     Declare Input/Output variables.
c-----------------------------------------------------------------------
      INTEGER nA(2), nB(2), nC(2)
      DOUBLE PRECISION mA( nA(1), nA(2) ), dB( pdB ), mC( nA(1), nB(2) )
      
c     ------------------------------------------------------------------
c     Declare local variables.
c     ------------------------------------------------------------------
      INTEGER i, j, nn, sB
      DOUBLE PRECISION  vRA( nA(2) ), ddot
      
c-----------------------------------------------------------------------
c     Check for valid matrix multiplication.
c-----------------------------------------------------------------------
      IF (( nA(2) .eq. nB(1) ).and.( nB(2)-nB(1) .ge. 0 )) THEN
      
c-----------------------------------------------------------------------
c     Establish dimensions of mC matrix.
c-----------------------------------------------------------------------
       nC(1) = nA(1)
       nC(2) = nB(2)
       sB = nB(2)-nB(1)+1
       
c-----------------------------------------------------------------------
c     Perform matrix multiply of mC = mA x mB.
c-----------------------------------------------------------------------
       DO i=1, nC(1)

c     ------------------------------------------------------------------
c     Create reversed version of a row in mA.
c     ------------------------------------------------------------------
        DO j = 1, nA(2)
         vRA( nA(2)-j+1 ) = mA( i, j )
        END DO

c     ------------------------------------------------------------------
c     Compute dot product of mA row i (from dRA) x mB column j (from dB).
c     ------------------------------------------------------------------
        DO j=1, nC(2)
         IF ( j .le. nB(1) ) THEN
           nn = min(j,sB)
c          mC(i,j) = ddot( nn, mA(i,j), -nA(1), dB(1), 1 )
           mC(i,j) = ddot( nn, vRA(nA(2)-j+1), 1, dB(1), 1 )
         ELSE
           nn = nB(2)-j+1
c          mC(i,j) = ddot( nn, mA(i,nA(2)), -nA(1), dB(j-nB(1)+1), 1 )
           mC(i,j) = ddot( nn, vRA(1), 1, dB(j-nB(1)+1), 1 )
         END IF
        END DO
       END DO
      
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