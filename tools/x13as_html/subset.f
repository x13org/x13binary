C     Last change:  SRD  16 Oct 2000    9:26 am
**==subset.f    processed by SPAG 4.03F  at 09:54 on  1 Mar 1994
      SUBROUTINE subset(A,Nrowa,Ncola,Begrow,Endrow,Begcol,Endcol,
     &                  Suba)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Returns in suba, a subset of a[begrow:endrow,begcol:endcol]
c Note that the matrices are defined in FORTRAN with the row and
c column indices reversed.
c-----------------------------------------------------------------------
c Input Arguments
c Name  Type Description
c-----------------------------------------------------------------------
c a       d  Nrowa by ncola matrix
c begcol  i  Begining column of the subset
c begrow  i  Begining row of the subset
c endcol  i  Last column of the subset
c endrow  i  Last row of the subset
c ncola   i  Number of columns in a
c nrowa   i  Number of rows in a
c psuba   i  Number of elements in psuba (removed May 2001 BCM)
c-----------------------------------------------------------------------
      INTEGER Begcol,Begrow,Endcol,Endrow,Ncola,Nrowa
      DOUBLE PRECISION A
      DIMENSION A(Ncola,Nrowa)
c-----------------------------------------------------------------------
c Output Arguments
c Name  Type Description
c-----------------------------------------------------------------------
c suba    d  Endrow-begrow+1 by endcol-begcol+1 output matrix
c-----------------------------------------------------------------------
      DOUBLE PRECISION Suba
      DIMENSION Suba(*)
c-----------------------------------------------------------------------
c Local Arguments
c Name  Type Description
c-----------------------------------------------------------------------
c jcol    i  Column index
c irow    i  Row index
c-----------------------------------------------------------------------
      INTEGER jcol,irow
c     ------------------------------------------------------------------
      DO irow=Begrow,Endrow
       DO jcol=Begcol,Endcol
        Suba(irow-Begrow+1)=A(jcol,irow)
       END DO
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
