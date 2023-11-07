C     Last change:  BCM  23 Jul 1998    3:39 pm
      SUBROUTINE svolit(Lfcn,I,Ia,Aord,Otlid,Notlcr,Tval,Rbmse,Rmse,
     &                  Sviter,Lxreg)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Called by idotlr()
c-----------------------------------------------------------------------
c     Save the outlier detection iterations
c     add new argument for svolit (BCM May 2007)
c     add new argument for svolit (LSUN Dec 2019)
c-----------------------------------------------------------------------
c Name   Type Description
c-----------------------------------------------------------------------
c I      i    Input number of forward addition or backward deletion pass
c Ia     i    Input if addall, number of outlier added or deleted in
c                   this pass, otherwise 0
c Aord   c    Input '+' for addition or '-' for deletion
c Otlid  c    Input characters of outlier id
c Tval   d    Input t value
c Rb2mse d    Input tao based robust root mse
c Rbmse  d    Input robust root mse
c Rmse   d    Input normal root mse
c-----------------------------------------------------------------------
      INCLUDE 'cchars.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'error.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'savcmn.cmn'
      INCLUDE 'mdltbl.i'
      INCLUDE 'xrgtbl.i'
c     ------------------------------------------------------------------
      LOGICAL frstcl,Lfcn,locok,Sviter,Lxreg
      CHARACTER outstr*(150)
      CHARACTER Aord*1,Otlid*(PCOLCR),climit*1
      CHARACTER dash*(22)
      INTEGER fh,I,Ia,ipos,j,Notlcr
      DOUBLE PRECISION Tval,Rbmse,Rmse
c     ------------------------------------------------------------------
*      INTEGER nblank
*      EXTERNAL nblank
c     ------------------------------------------------------------------
      SAVE frstcl,fh
c     ------------------------------------------------------------------
      DATA frstcl/.true./
      DATA dash /'----------------------'/
c-----------------------------------------------------------------------
      IF(frstcl.and.Sviter)THEN
       IF(Lxreg)THEN
        CALL opnfil(.true.,.false.,LXROIT,fh,locok)
       ELSE
        CALL opnfil(.true.,.false.,LOTLIT,fh,locok)
       END IF
       IF(.not.locok)THEN
        CALL abend
        RETURN
       END IF
c     ------------------------------------------------------------------
       WRITE(fh,1010)'pass',TABCHR,'io',TABCHR,'outlier',TABCHR,TABCHR,
     &               'medrmse',TABCHR,TABCHR,TABCHR,'rmse',TABCHR,
     &                TABCHR,TABCHR,TABCHR,'t'
 1010  FORMAT(1000a)
       WRITE(fh,1010)'----',TABCHR,'--',TABCHR,'---------',
     &               (TABCHR,dash(1:Svsize),j=1,3)
       frstcl=.false.
      END IF
c-----------------------------------------------------------------------
c     Close the file if requested.
c-----------------------------------------------------------------------
      IF(Lfcn)THEN
       IF(Sviter)THEN
        CALL fclose(fh)
        frstcl=.true.
       END IF
c-----------------------------------------------------------------------
c     Save the iterations
c-----------------------------------------------------------------------
      ELSE
       IF(Sviter)THEN
        climit=TABCHR
       ELSE
        climit=' '
       END IF
       ipos=1
       CALL itoc(I,outstr,ipos)
       IF(Lfatal)RETURN
c     ------------------------------------------------------------------
       IF(Sviter)THEN
        IF (Ia.gt.0) THEN
          outstr(ipos:ipos)='.'
        ELSE
          outstr(ipos:ipos)=climit
        END IF
       ELSE
        outstr(ipos:ipos)='.'
       END IF
       ipos=ipos+1
       IF(Ia.gt.0)THEN
         CALL itoc(Ia,outstr,ipos)
         outstr(ipos:ipos)='.'
         ipos = ipos + 1
         IF(Lfatal)RETURN
         IF(Sviter)THEN
           outstr(ipos:ipos)=climit
           ipos = ipos + 1
         END IF
       END IF
       outstr(ipos:ipos)=Aord
       ipos=ipos+1
c     ------------------------------------------------------------------
       IF(Sviter)THEN
        outstr(ipos:ipos)=climit
        ipos=ipos+1
       ELSE
        outstr(ipos:ipos+1)=': '
        ipos=ipos+2
       END IF
c      rather than compute length of regressor with nblank, take as
c      argument to routine (BCM May 2007)
*       notlcr=nblank(Otlid)
       outstr(ipos:ipos+Notlcr-1)=Otlid(1:Notlcr)
       ipos=ipos+notlcr
c     ------------------------------------------------------------------
       outstr(ipos:ipos)=climit
       ipos=ipos+1
       CALL dtoc(Rbmse,outstr,ipos)
       IF(Lfatal)RETURN
c     ------------------------------------------------------------------
       outstr(ipos:ipos)=climit
       ipos=ipos+1
       CALL dtoc(Rmse,outstr,ipos)
       IF(Lfatal)RETURN
c     ------------------------------------------------------------------
       outstr(ipos:ipos)=climit
       ipos=ipos+1
       CALL dtoc(Tval,outstr,ipos)
       IF(Lfatal)RETURN
c     ------------------------------------------------------------------
       IF(Sviter)THEN
        WRITE(fh,1010)outstr(1:ipos-1)
       ELSE
        IF(Lxreg)THEN
         WRITE(Nform,1010)'xotlitr.',outstr(1:ipos-1)
        ELSE
         WRITE(Nform,1010)'otlitr.',outstr(1:ipos-1)
        END IF
       END IF
      END IF
c     ------------------------------------------------------------------
      RETURN
      END

