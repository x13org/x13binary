C     Last change:  BCM  14 May 1998    7:50 am
**==coladd.f    processed by SPAG 4.03F  at 09:47 on  1 Mar 1994
      SUBROUTINE coladd(Begcol,Endcol,Nrxy,Peltxy,Xy,Ncxy)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Subroutine makes room for  columns starting at begcol to endcol.
c Updates xy and ncxy.
c-----------------------------------------------------------------------
c Name  Type Description
c-----------------------------------------------------------------------
c begcol  i  Local index for the column to make space at
c grp     i  Input 2 element array where the first element is first
c             column of the added columns in the expanded matrix,
c             the second element is the number of added columns
c i       i  Local do loop row index
c ibeg    i  Local index bound for the begining element of a row
c iend    i  Local index bound for the last element of a row
c j       i  Local do loop element index
c ncxy    i  Ouput number of columns in X with space for the added columns
c naddc   i  Local number of columns to make room for
c nnewc   i  Local for the number of new columns this equal to ncxy
c nrxy    i  Input number of rows
c offset  i  Local number of elements that a row must be moved
c xy      d  In/out nrxy by noldc or ncxy<=peltxy matrix
c-----------------------------------------------------------------------
c     Data typing
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'units.cmn'
      INCLUDE 'htmlout.cmn'
c     ------------------------------------------------------------------
      INTEGER Begcol,Endcol,i,ibeg,iend,j,Ncxy,naddc,nnewc,Nrxy,offset,
     &        Peltxy
      DOUBLE PRECISION Xy
      DIMENSION Xy(Peltxy)
c-----------------------------------------------------------------------
c     Check that 1<=begcol<=noldc+1 and nrxy*(noldc+naddc)<=peltxy
c-----------------------------------------------------------------------
      naddc=Endcol-Begcol+1
      nnewc=Ncxy+naddc
c     ------------------------------------------------------------------
      IF(Begcol.lt.1.or.Begcol.gt.Ncxy+1)THEN
       WRITE(STDERR,1010)Begcol,Ncxy
       CALL errhdr
       WRITE(Mt2,1011)Cbr,Cbr,Cbr,Begcol,Ncxy
 1010  FORMAT(/,' ERROR: Invalid column information:  Beginning ',
     &        'column of the insertion',/,
     &          '        must be between 1<=begcol<=ncxy+1',/,
     &          '                        1<=',i4,'<=',i4,'.')
 1011  FORMAT('<p><strong>ERROR:</strong> Invalid column information:',
     &      a,/,' Beginning column of the insertion must be between ',
     &      a,/,'1<=begcol<=ncxy+1',a,' 1<=',i4,'<=',i4,'.</p>')
       CALL abend
       RETURN
c     ------------------------------------------------------------------
      ELSE IF(Nrxy*nnewc.gt.Peltxy)THEN
       WRITE(STDERR,1020)Nrxy,Ncxy,naddc,Peltxy
       CALL errhdr
       WRITE(Mt2,1021)Nrxy,Ncxy,naddc,Peltxy
 1020  FORMAT(/,' ERROR: nrxy*(noldc+naddc)=',i4,'*(',i4,'+',i4,')>',
     &        i6,'.')
 1021  FORMAT(/,'<p><strong>ERROR:</strong> nrxy*(noldc+naddc)=',
     &        i4,'*(',i4,'+',i4,')>',i6,'.')
       CALL abend
       RETURN
      END IF
c-----------------------------------------------------------------------
c     Since elements are added to the same matrix start at the end.
c First setup the indices and index bounds
c-----------------------------------------------------------------------
      offset=Nrxy*naddc
      iend=Nrxy*Ncxy
      ibeg=iend-Ncxy+Begcol
c     ------------------------------------------------------------------
      DO j=iend,ibeg,-1
       Xy(j+offset)=Xy(j)
      END DO
c     ------------------------------------------------------------------
      DO i=Nrxy-1,1,-1
       offset=i*naddc
       iend=ibeg-1
       ibeg=iend-Ncxy+1
c     ------------------------------------------------------------------
       DO j=iend,ibeg,-1
        Xy(j+offset)=Xy(j)
       END DO
      END DO
      Ncxy=nnewc
c     ------------------------------------------------------------------
      RETURN
      END
