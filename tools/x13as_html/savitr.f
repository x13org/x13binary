C     Last change:  BCM  15 Jan 98   12:05 pm
      SUBROUTINE savitr(Lfcn,Iteri,Itera,Lglkhd,Parms,Nparms)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Save the iterations, both the nonlinear and the overall
c iterations
c-----------------------------------------------------------------------
c Name   Type Description
c-----------------------------------------------------------------------
c iteri  i    Input number of IGLS iterations
c itera  i    Input number of ARMA iterations
c lglkhd d    Input log likelihood
c nparms i    Input number of ARMA parmeters to save
c parms  d    Input ARMA parameters
c-----------------------------------------------------------------------
      INCLUDE 'cchars.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'savcmn.cmn'
      INCLUDE 'mdltbl.i'
c-----------------------------------------------------------------------
      CHARACTER outstr*(10+(22*(PB+PARIMA+1))),dash*(22)
      LOGICAL frstcl,Lfcn,locok
      INTEGER i,Iteri,Itera,Nparms,fh,ipos
      DOUBLE PRECISION Lglkhd,Parms(Nparms)
c-----------------------------------------------------------------------
      SAVE frstcl,fh
c-----------------------------------------------------------------------
      DATA frstcl/.true./
      DATA dash /'----------------------'/
c-----------------------------------------------------------------------
      IF(frstcl)THEN
       CALL opnfil(.true.,.false.,LESTIT,fh,locok)
       IF(.not.locok)THEN
        CALL abend
        RETURN
       END IF
c-----------------------------------------------------------------------
       IF(Nb.gt.0)THEN
        WRITE(fh,1010)'overall',TABCHR,'nonlinear',TABCHR,
     &                'loglikelihood',TABCHR,
     &                ('arma',i,TABCHR,i=1,Nparms),
     &                ('reg',i,TABCHR,i=1,Nb)
 1010   FORMAT(a,a,a,a,a,a,100(a,i2.2,a))
        WRITE(fh,1030)'-------',TABCHR,'---------',TABCHR,
     &                dash(1:Svsize),TABCHR,(dash(1:Svsize),TABCHR,
     &                i=1,Nparms),(dash(1:Svsize),TABCHR,i=1,Nb)
c-----------------------------------------------------------------------
       ELSE
        WRITE(fh,1020)'nonlinear',TABCHR,'loglikelihood',TABCHR,
     &                ('arma',i,TABCHR,i=1,Nparms)
 1020   FORMAT(a,a,a,a,100(a,i2.2,a))
        WRITE(fh,1030)'---------',TABCHR,dash(1:Svsize),TABCHR,
     &                (dash(1:Svsize),TABCHR,i=1,Nparms),
     &                (dash(1:Svsize),TABCHR,i=1,Nb)
       END IF
c-----------------------------------------------------------------------
 1030  FORMAT(a:,100a)
       frstcl=.false.
      END IF
c-----------------------------------------------------------------------
c     Save the iterations
c-----------------------------------------------------------------------
      IF(Lfcn)THEN
       CALL fclose(fh)
c       frstcl=.true.
c-----------------------------------------------------------------------
      ELSE
       ipos=1
c-----------------------------------------------------------------------
       IF(Nb.gt.0)THEN
        CALL itoc(Iteri,outstr,ipos)
        outstr(ipos:ipos)=TABCHR
        ipos=ipos+1
       END IF
c-----------------------------------------------------------------------
       CALL itoc(Itera,outstr,ipos)
       outstr(ipos:ipos)=TABCHR
       ipos=ipos+1
c-----------------------------------------------------------------------
       CALL dtoc(Lglkhd,outstr,ipos)
       outstr(ipos:ipos)=TABCHR
       ipos=ipos+1
c-----------------------------------------------------------------------
       DO i=1,Nparms
        CALL dtoc(Parms(i),outstr,ipos)
        outstr(ipos:ipos)=TABCHR
        ipos=ipos+1
       END DO
c-----------------------------------------------------------------------
       IF(Nb.gt.0)THEN
        DO i=1,Nb
         CALL dtoc(B(i),outstr,ipos)
         outstr(ipos:ipos)=TABCHR
         ipos=ipos+1
        END DO
       END IF
c-----------------------------------------------------------------------
       WRITE(fh,1030)outstr(1:ipos-1)
      END IF
c-----------------------------------------------------------------------
      RETURN
c-----------------------------------------------------------------------
      END
