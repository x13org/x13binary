C     Last change:  BCM  14 May 1998    8:45 am
      SUBROUTINE dlrgef(Begcol,Nrxy,Ndelc)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Deletes ndelc columns from [X:y] starting at begcol.
c The regression effect estimates column and regression group title
c and specification arrays are also updated.
c-----------------------------------------------------------------------
c Name   Type Description
c-----------------------------------------------------------------------
c b       d  I/O pb long, nb used, vector of all the regression effects
c begcol  i  Local begining column of columns to be deleted
c endcol  i  Local index for the end column to delete
c i       i  Local do loop row index
c ibeg    i  Local index bound for the begining element of a row or the
c             begining column in the current regression group
c idelc   i  Local number of columns to delete in the current regression
c             group
c iend    i  Local index bound for the last element of a row or the last
c             column in the current regression group
c igrp    i  Local the current regression group
c j       i  Local do loop element index
c ncol    i  Local number of columns in the current regression group
c ndelc   i  Local for number of columns to delete
c nloop   i  Local limit for the number of loops taken in a do loop.
c             Used so that ngrps can be updated within the loop.
c noldc   i  Local number of columns before deletion
c noffst  i  Local number of elements that a row must be moved or the
c             number of rows a regression group needs to be moved due
c             to deleted groups.
c-----------------------------------------------------------------------
c     Type and dimension variables
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      INTEGER Begcol,endcol,i,ibeg,idelc,iend,igrp,j,ncol,Ndelc,nloop,
     &        noldc,Nrxy,ntdelc,noffst,e1
c-----------------------------------------------------------------------
c     Calculate the number of regression variable and find the group.
c If the group can't be found igrp is 0.
c-----------------------------------------------------------------------
      noldc=Ncxy
      endcol=Begcol+Ndelc-1
c-----------------------------------------------------------------------
c     Check if columns are within a the X part of the Xy matrix
c-----------------------------------------------------------------------
      IF(Begcol.lt.1.or.endcol.gt.Nb)THEN
       WRITE(STDERR,1010)Begcol,endcol,Ncxy-1
 1010  FORMAT(/,' ERROR: Deleted columns,',i3,':',i2,', not within',i3,
     &          ' column regression matrix.')
       CALL errhdr
       WRITE(Mt2,1011)Begcol,endcol,Ncxy-1
 1011  FORMAT(/,'<p><strong>ERROR:</strong> Deleted columns,',i3,':',i2,
     &          ', not within',i3,' column regression matrix.</p>')
       CALL abend
       RETURN
      END IF
c-----------------------------------------------------------------------
c     Delete the column titles and regression estimates for the
c deleted columns
c-----------------------------------------------------------------------
      DO i=endcol,Begcol,-1
       CALL delstr(i,Colttl,Colptr,Ncoltl,PB)
       IF(Lfatal)RETURN
      END DO
c-----------------------------------------------------------------------
c     Delete the regression coeffcients and the regression types
c-----------------------------------------------------------------------
      e1=endcol+1
      IF(e1.le.PB)THEN
       CALL copy(B(e1),noldc-1-endcol,1,B(Begcol))
       CALL cpyint(Rgvrtp(e1),noldc-1-endcol,1,Rgvrtp(Begcol))
       CALL copylg(Regfx(e1),noldc-1-endcol,1,Regfx(Begcol))
      END IF
c-----------------------------------------------------------------------
c     Since elements are deleted from the same matrix start at the
c begining.  First setup the indices and index bounds
c-----------------------------------------------------------------------
      Ncxy=noldc-Ndelc
      Nb=Ncxy-1
      iend=Begcol-1
c     ------------------------------------------------------------------
      DO i=1,Nrxy-1
       noffst=i*Ndelc
       ibeg=iend+1
       iend=iend+Ncxy
c     ------------------------------------------------------------------
       DO j=ibeg,iend
        Xy(j)=Xy(j+noffst)
       END DO
      END DO
c     ------------------------------------------------------------------
      noffst=Nrxy*Ndelc
      ibeg=iend+1
c     ------------------------------------------------------------------
      DO j=ibeg,Nrxy*Ncxy
       Xy(j)=Xy(j+noffst)
      END DO
c-----------------------------------------------------------------------
c     Update the grp and grpttl indices
c-----------------------------------------------------------------------
      noffst=0
      nloop=Ngrp
c     ------------------------------------------------------------------
      ntdelc=Ndelc
      DO igrp=1,nloop
       ibeg=Grp(igrp-1)
       iend=Grp(igrp)-1
c     ------------------------------------------------------------------
       IF(iend.ge.Begcol.and.ntdelc.gt.0)THEN
        CALL eltlen(igrp,Grp,Ngrp,ncol)
        IF(Lfatal)RETURN
        IF(ntdelc.gt.0)THEN
         idelc=min(iend,Begcol+ntdelc-1)-max(ibeg,Begcol)+1
         ncol=ncol-idelc
         ntdelc=ntdelc-idelc
c     ------------------------------------------------------------------
         IF(ntdelc.gt.0)Begcol=ibeg+ncol
        ELSE
         idelc=0
        END IF
c     ------------------------------------------------------------------
        IF(ncol.gt.0)THEN
         i=igrp+noffst
c         Grp(i-1)=ibeg
         DO i=i,Ngrp
          Grp(i)=Grp(i)-idelc
         END DO
c     ------------------------------------------------------------------
        ELSE
         CALL delstr(igrp,Grpttl,Grpptr,Ngrptl,PGRP)
         IF(Lfatal)RETURN
         Ngrp=Ngrp-1
         noffst=noffst-1
         DO i=igrp,Ngrp
          Grp(i)=Grp(i+1)-idelc
         END DO
        END IF
       END IF
      END DO
c     ------------------------------------------------------------------
      RETURN
      END
