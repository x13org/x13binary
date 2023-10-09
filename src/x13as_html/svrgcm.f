C     Last change:  BCM   1 Jun 1998    4:55 pm
      SUBROUTINE svrgcm(Nefobs,Xpxinv,Regidx)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Called by prtmdl()
c-----------------------------------------------------------------------
c     Save the covariance matrix from var*(X'X)^-1
c-----------------------------------------------------------------------
      INCLUDE 'cchars.i'
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'mdltbl.i'
      INCLUDE 'error.cmn'
      INCLUDE 'savcmn.cmn'
c     ------------------------------------------------------------------
      LOGICAL locok
      CHARACTER str*(PCOLCR),outstr*(PCOLCR+22*PB),dash*(22)
      INTEGER fh,i,j,ii,jj,nchr,Nefobs,ipos,Regidx,jcol
      DOUBLE PRECISION rgnvar,Xpxinv
      DIMENSION Xpxinv(Nb*Ncxy/2),Regidx(PB) 
c     ------------------------------------------------------------------
      DATA dash /'----------------------'/
c-----------------------------------------------------------------------
      IF(Nb.le.1)RETURN
c     ------------------------------------------------------------------
      CALL opnfil(.true.,.false.,LESTCM,fh,locok)
      IF(.not.locok)THEN
       CALL abend
       RETURN
      END IF
c     ------------------------------------------------------------------
c     Construct and print header
c     ------------------------------------------------------------------
      outstr(1:8)='variable'
      ipos=9
      jcol=0
      DO i=1,Nb
       IF(Regidx(i).ne.NOTSET)THEN
        outstr(ipos:ipos)=TABCHR
        ipos=ipos+1
        outstr(ipos:ipos+2)='var'
        ipos=ipos+3
        CALL itoc(i,outstr,ipos)
        IF(Lfatal)RETURN
        jcol=jcol+1
       END IF
      END DO
      WRITE(fh,1010)outstr(1:ipos-1)
      WRITE(fh,1010)'--------',(TABCHR,dash(1:Svsize),i=1,jcol)
c     ------------------------------------------------------------------
      rgnvar=Var*Nefobs/(Nefobs-jcol)
c     ------------------------------------------------------------------
      DO i=1,Nb
       IF(Regidx(i).ne.NOTSET)THEN
        CALL getstr(Colttl,Colptr,Ncoltl,i,str,nchr)
        IF(Lfatal)RETURN
        outstr(1:nchr)=str(1:nchr)
        ipos=nchr+1
        DO j=1,Nb
         IF(Regidx(j).ne.NOTSET)THEN
          jj=max(Regidx(i),Regidx(j))
          ii=min(Regidx(i),Regidx(j))
          outstr(ipos:ipos)=TABCHR
          ipos=ipos+1
          CALL dtoc(rgnvar*Xpxinv((jj-1)*jj/2+ii),outstr,ipos)
          IF(Lfatal)RETURN
         END IF
        END DO
c     ------------------------------------------------------------------
        WRITE(fh,1010)outstr(1:ipos-1)
       END IF
      END DO
c     ------------------------------------------------------------------
      IF(locok)CALL fclose(fh)
c     ------------------------------------------------------------------
      RETURN
 1010 FORMAT(100a)
      END

