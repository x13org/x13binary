C     Last change:  SRD  19 Nov 99    7:59 am
**==dpmpar.f    processed by SPAG 4.03F  at 09:48 on  1 Mar 1994
      DOUBLE PRECISION FUNCTION dpmpar(I)
      IMPLICIT NONE
      INTEGER I
C     **********
C
C     FUNCTION DPMPAR
C
C     THIS FUNCTION PROVIDES DOUBLE PRECISION MACHINE PARAMETERS
C     WHEN THE APPROPRIATE SET OF DATA STATEMENTS IS ACTIVATED (BY
C     REMOVING THE C FROM COLUMN 1) AND ALL OTHER DATA STATEMENTS ARE
C     RENDERED INACTIVE. MOST OF THE PARAMETER VALUES WERE OBTAINED
C     FROM THE CORRESPONDING BELL LABORATORIES PORT LIBRARY FUNCTION.
C
C     THE FUNCTION STATEMENT IS
C
C       DOUBLE PRECISION FUNCTION DPMPAR(I)
C
C     WHERE
C
C       I IS AN INTEGER INPUT VARIABLE SET TO 1, 2, OR 3 WHICH
C         SELECTS THE DESIRED MACHINE PARAMETER. IF THE MACHINE HAS
C         T BASE B DIGITS AND ITS SMALLEST AND LARGEST EXPONENTS ARE
C         EMIN AND EMAX, RESPECTIVELY, THEN THESE PARAMETERS ARE
C
C         DPMPAR(1) = B**(1 - T), THE MACHINE PRECISION,
C
C         DPMPAR(2) = B**(EMIN - 1), THE SMALLEST MAGNITUDE,
C
C         DPMPAR(3) = B**EMAX*(1 - B**(-T)), THE LARGEST MAGNITUDE.
C
C     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. MARCH 1980.
C     BURTON S. GARBOW, KENNETH E. HILLSTROM, JORGE J. MORE
C
C     **********
      DOUBLE PRECISION dmach(3)
C     ------------------------------------------------------------------
cdos  value of dmach for lahey fortran
cdos      DATA dmach/2.220446D-16,2.23D-308,1.79D308/
C     ------------------------------------------------------------------
cunix value of dmach for SparcCompiler for Solaris 3.0.1
      DATA dmach/2.220446d-16,2.225074d-308,1.797693d308/
C     ------------------------------------------------------------------
cvax  value of dmach for VAX Alpha
cvax       DATA dmach/2.220446d-16,0.30d-38,1.69d38/
C     ------------------------------------------------------------------
C
      dpmpar=dmach(I)
      RETURN
C
C     LAST CARD OF FUNCTION DPMPAR.
C
      END
