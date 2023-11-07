C     Last change:  BCM  15 Jan 98   12:01 pm
      SUBROUTINE svamcm
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Save the ARMA covariance matrix from (X'X)^-1
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'mdltbl.i'
      INCLUDE 'error.cmn'
      INCLUDE 'savcmn.cmn'
      INCLUDE 'cchars.i'
c     ------------------------------------------------------------------
      INTEGER OPRS
      PARAMETER(OPRS=2)
c-----------------------------------------------------------------------
      LOGICAL locok
      CHARACTER cfix*7,tmpttl*(POPRCR),outstr*(POPRCR+22*PARIMA),
     &          dash*(22)
      INTEGER beglag,begopr,endlag,endopr,fh,i,iestpm,iflt,ilag,iopr,
     &        irow,ntmpcr,ipos
c     ------------------------------------------------------------------
      DATA dash /'----------------------'/
c-----------------------------------------------------------------------
      IF(Nestpm.le.1)RETURN
c     ------------------------------------------------------------------
      CALL opnfil(.true.,.false.,LESTAM,fh,locok)
      IF(.not.locok)THEN
       CALL abend
       RETURN
      END IF
c     ------------------------------------------------------------------
c     Construct and print header
c     ------------------------------------------------------------------
      outstr(1:9)='parameter'
      ipos=10
      DO i=1,Nestpm
       outstr(ipos:ipos)=TABCHR
       ipos=ipos+1
       outstr(ipos:ipos+3)='parm'
       ipos=ipos+4
       CALL itoc(i,outstr,ipos)
       IF(Lfatal)RETURN
      END DO
      WRITE(fh,1010)outstr(1:ipos-1)
      WRITE(fh,1010)'---------',(TABCHR,dash(1:Svsize),i=1,Nestpm)
c     ------------------------------------------------------------------
      iestpm=0
      DO iflt=AR,MA
       begopr=Mdl(iflt-1)
       endopr=Mdl(iflt)-1
c     ------------------------------------------------------------------
       DO iopr=begopr,endopr
        beglag=Opr(iopr-1)
        endlag=Opr(iopr)-1
c     ------------------------------------------------------------------
        CALL isfixd(OPRS,Arimaf,beglag,endlag,cfix)
        IF(cfix.eq.' &nbsp;')THEN
         CALL getstr(Oprttl,Oprptr,Noprtl,iopr,tmpttl,ntmpcr)
         IF(Lfatal)RETURN
c     ------------------------------------------------------------------
         DO ilag=beglag,endlag
          IF(.not.Arimaf(ilag))THEN
           iestpm=iestpm+1
           outstr(1:ntmpcr)=tmpttl(1:ntmpcr)
           ipos=ntmpcr+1
           outstr(ipos:ipos)=TABCHR
           ipos=ipos+1
           CALL itoc(Arimal(ilag),outstr,ipos)
           IF(Lfatal)RETURN
           DO irow=1,Nestpm
            outstr(ipos:ipos)=TABCHR
            ipos=ipos+1
            CALL dtoc(Var*Armacm(iestpm,irow),outstr,ipos)
            IF(Lfatal)RETURN
           END DO
           WRITE(fh,1010)outstr(1:ipos-1)
          END IF
         END DO
        END IF
       END DO
      END DO
c     ------------------------------------------------------------------
      IF(locok)CALL fclose(fh)
c     ------------------------------------------------------------------
 1010 FORMAT(1000a)
      RETURN
      END
