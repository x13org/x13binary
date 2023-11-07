**==xprmx.f    processed by SPAG 4.03F  at 09:56 on  1 Mar 1994
      SUBROUTINE xprmx(Xy,Nspobs,Ncxy,Pcxy,Xypxy)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Subroutine to make X'X matrix or an [X:y]'[X:y] with the data
c vector in the pcxyth column.  Does this by matrix multiplication
c of the upper triangle of elements of X'X or [X:y]'[X:y].
c-----------------------------------------------------------------------
c Name     Type Description
c-----------------------------------------------------------------------
c i       i  Local do loop index
c ielt    i  Local packed element index
c j       i  Local do loop index
c k       i  Local do loop index
c ncxy    i  Input number of columns in x
c nspobs    i  Input number of rows in both x and y
c pcxy    i  Input parmeter for the leading array (column) index of the
c             X matrix.
c sum     d  Local temporary sum to get the inner product of x'(i) and
c             x(j)
c xy      d  Input nspobs by ncxy matrix with possibly a vector of data
c             in the pcxyth column.  X'y then is stored in the ncxy+1
c             column if pcxy is greater than ncxy.
c xypxy   d  Output packed ncxy square symmetric output matrix,
c             [X:y]'[X:y] with ncxy(ncxy+1)/2 elements if ncxy=pcxy and
c             (ncxy+1)(ncxy+2)/2 if pcxy>ncxy.
c-----------------------------------------------------------------------
c      LOGICAL T,F
c      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      INTEGER Pcxy,i,ielt,j,Ncxy,Nspobs
      DOUBLE PRECISION ddot,Xy(Pcxy*Nspobs),Xypxy(*)
      EXTERNAL ddot
c-----------------------------------------------------------------------
c     Also, the elements of the array are stored are stored row by row
c which is why the FORTRAN array indices are switched from standard
c matrix notation.
c-----------------------------------------------------------------------
c      CALL under0(T)
      ielt=0
c     ------------------------------------------------------------------
      DO i=1,Ncxy
       DO j=1,i
        ielt=ielt+1
        Xypxy(ielt)=ddot(Nspobs,Xy(i),Pcxy,Xy(j),Pcxy)
       END DO
      END DO
c-----------------------------------------------------------------------
c     If pcxy > ncxy then assume the data array is in the pcxyth column
c so calculate X'y and put y'y in the last element.
c-----------------------------------------------------------------------
      IF(Pcxy.gt.Ncxy)THEN
       DO j=1,Ncxy
        ielt=ielt+1
        Xypxy(ielt)=ddot(Nspobs,Xy(j),Pcxy,Xy(Pcxy),Pcxy)
       END DO
c     ------------------------------------------------------------------
       Xypxy(ielt+1)=ddot(Nspobs,Xy(Pcxy),Pcxy,Xy(Pcxy),Pcxy)
      END IF
c     ------------------------------------------------------------------
c      CALL under0(F)
c-----------------------------------------------------------------------
      RETURN
      END
