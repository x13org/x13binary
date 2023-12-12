C     Last change:  BCM  14 May 1998    7:51 am
      SUBROUTINE dlusrg(Begcol)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Deletes ndelc columns from user defined regressors starting at 
c     begcol.  Other arrays associated with the user defined regressors
c     are also updated.
c-----------------------------------------------------------------------
c Name   Type Description
c-----------------------------------------------------------------------
c begcol  i  Local begining column of columns to be deleted
c i       i  Local do loop row index
c ibeg    i  Local index bound for the begining element of a row or the
c             begining column in the current regression group
c iend    i  Local index bound for the last element of a row or the last
c             column in the current regression group
c j       i  Local do loop element index
c ndelc   i  Local for number of columns to delete
c noldc   i  Local number of columns before deletion
c offset  i  Local number of elements that a row must be moved or the
c             number of rows a regression group needs to be moved due
c             to deleted groups.
c-----------------------------------------------------------------------
c     Type and dimension variables
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'arima.cmn'
      INCLUDE 'usrreg.cmn'
      INCLUDE 'units.cmn'
c     ------------------------------------------------------------------
      INTEGER Begcol,i,ibeg,iend,j,noldc,offset
c-----------------------------------------------------------------------
c     Calculate the number of regression variable and find the group.
c If the group can't be found igrp is 0.
c-----------------------------------------------------------------------
      noldc=Ncusrx
c-----------------------------------------------------------------------
c     Check if columns are within a the X part of the Xy matrix
c-----------------------------------------------------------------------
      IF(Begcol.lt.1.or.Begcol.gt.Ncusrx)THEN
       WRITE(STDERR,1010)Begcol,Ncusrx
 1010  FORMAT(/,' ERROR: Deleted column,',i3,' not within',i3,
     &          ' column user-regression matrix.')
       CALL errhdr
       WRITE(Mt2,1011)Begcol,Ncusrx
 1011  FORMAT(/,' <p><strong>ERROR:</strong> Deleted column,',i3,
     &          ' not within',i3,' column user-regression matrix.</p>')
       CALL abend
       RETURN
      END IF
c-----------------------------------------------------------------------
c     Delete the column titles and regression estimates for the
c deleted columns
c-----------------------------------------------------------------------
      CALL delstr(Begcol,Usrttl,Usrptr,Ncusrx,PUREG)
c-----------------------------------------------------------------------
c     Delete the user regression types
c-----------------------------------------------------------------------
      CALL cpyint(Usrtyp(Begcol+1),noldc-1-Begcol,1,Usrtyp(Begcol))
c-----------------------------------------------------------------------
c     Since elements are deleted from the same matrix start at the
c begining.  First setup the indices and index bounds
*c-----------------------------------------------------------------------
      IF(noldc.eq.1)RETURN
      iend=Begcol-1
c     ------------------------------------------------------------------
      DO i=1,Nrusrx-1
       offset=i*1
       ibeg=iend+1
       iend=iend+Ncusrx
c     ------------------------------------------------------------------
       DO j=ibeg,iend
        Userx(j)=Userx(j+offset)
       END DO
      END DO
c     ------------------------------------------------------------------
      offset=Nrusrx
      ibeg=iend+1
c     ------------------------------------------------------------------
      DO j=ibeg,Nrusrx*Ncusrx
       Userx(j)=Userx(j+offset)
      END DO
c     ------------------------------------------------------------------
      RETURN
      END

