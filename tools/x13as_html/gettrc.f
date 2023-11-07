	DOUBLE PRECISION FUNCTION getTrc( mA, nA )
c-----------------------------------------------------------------------
c     getTrc.f, Release 1, Subroutine Version 1.0, Created 18 Apr 2005.
c-----------------------------------------------------------------------
c	This subroutine the trace of the square matrix mA with size nA.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c mA		d	input matrix to calculate trace of
c nA		i	size (rows,columns) of mA matrix
c-----------------------------------------------------------------------
c Name   Type Description (local Variables)
c-----------------------------------------------------------------------
c i		i	index variable for do loops
c-----------------------------------------------------------------------
	IMPLICIT NONE
c-----------------------------------------------------------------------
c	Declare Input/Output variables.
c-----------------------------------------------------------------------
	INTEGER nA(2)
	DOUBLE PRECISION mA(nA(1),nA(2))

c	------------------------------------------------------------------
c	Declare local variables.
c	------------------------------------------------------------------
	INTEGER i

c-----------------------------------------------------------------------
c	Extract the submatrix.
c-----------------------------------------------------------------------
	getTrc = 0.0D0
	IF ( nA(1) .eq. nA(2) ) THEN
	 DO i = 1,nA(1)
	  getTrc = getTrc + mA(i,i)
	 END DO
	END IF
c	------------------------------------------------------------------
	RETURN
	END
c-----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION getTrcAB( mA, nA, mB, nB )
      
      IMPLICIT NONE
      INTEGER nA(2), nB(2)
      DOUBLE PRECISION mA(nA(1),nA(2)), mB(nB(1),nB(2))
      
      INTEGER i, j
      DOUBLE PRECISION diag, ZERO
      PARAMETER (ZERO=0.0D0)
      
      getTrcAB = ZERO
	IF (( nA(1) .eq. nB(2) ) .and. ( nA(2) .eq. nB(1) )) THEN
	 DO i = 1,nA(1)
	   diag = ZERO
	   DO j = 1, nA(2)
	     diag = diag + mA(i,j)*mB(j,i)
	   END DO
	   getTrcAB = getTrcAB + diag
	 END DO
      END IF
      
      RETURN
      END