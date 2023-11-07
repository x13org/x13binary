C     Last change:  BCM  15 May 1998    1:11 pm
      SUBROUTINE gtinvl(Optype,Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Add initial value to the ARIMA model
c-----------------------------------------------------------------------
      INCLUDE 'lex.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      CHARACTER str*(LINLEN)
      LOGICAL argok,getdbl,hvcmma,Inptok,locok,opngrp
      INTEGER beglag,begopr,endlag,endopr,FIXVAL,ielt,ipos,nchr,Optype
      DOUBLE PRECISION tmp
      PARAMETER(FIXVAL=1)
      EXTERNAL getdbl
c     ------------------------------------------------------------------
      CHARACTER FIXDIC*2
      INTEGER fixidx,fixptr,PFIX
      PARAMETER(PFIX=2)
      DIMENSION fixptr(0:PFIX)
      PARAMETER(FIXDIC='fe')
c     ------------------------------------------------------------------
      CHARACTER OPDIC*8
      INTEGER opptr,POP
      PARAMETER(POP=3)
      DIMENSION opptr(0:POP)
      PARAMETER(OPDIC='diffarma')
      DATA opptr/1,5,7,9/
c-----------------------------------------------------------------------
      DATA fixptr/1,2,3/
c-----------------------------------------------------------------------
c     Find the lags to initialize
c-----------------------------------------------------------------------
      begopr=Mdl(Optype-1)
      endopr=Mdl(Optype)-1
      beglag=Opr(begopr-1)
      endlag=Opr(endopr)-1
      ielt=beglag-1
c----------------------------------------------------------------------
      locok=T
      hvcmma=F
c     -----------------------------------------------------------------
      IF(Nxtktp.eq.EOF)THEN
       locok=F
c-----------------------------------------------------------------------
c     Only a single value
c-----------------------------------------------------------------------
      ELSE IF(getdbl(tmp))THEN
       ielt=ielt+1
       Arimap(ielt)=tmp
c     -----------------------------------------------------------------
       CALL gtdcnm(FIXDIC,fixptr,PFIX,fixidx,argok)
       IF(argok)Arimaf(ielt)=fixidx.eq.FIXVAL
c-----------------------------------------------------------------------
c     Is a list.
c-----------------------------------------------------------------------
      ELSE IF(Nxtktp.ne.LPAREN)THEN
       CALL inpter(PERROR,Lstpos,
     &             'Expected a real number or a list of real numbers,'//
     &             ' not "'//Nxttok(1:Nxtkln)//'"',T)
       locok=F
       opngrp=F
c     -----------------------------------------------------------------
      ELSE
       opngrp=T
c     -----------------------------------------------------------------
       CALL lex()
c----------------------------------------------------------------------
c     Process the list of doubles
c----------------------------------------------------------------------
       DO WHILE (T)
        DO WHILE (T)
         IF(Nxtktp.ne.RPAREN)THEN
c----------------------------------------------------------------------
c     Check for a NULL in the first place, for example, (,10.2, -8.3)
c or (6,,10.2,-8.3).  This section is repeated because there may be
c multiple NULLs
c----------------------------------------------------------------------
          IF(Nxtktp.eq.COMMA)THEN
           IF(hvcmma.or.opngrp)THEN
            IF(ielt.ge.endlag)THEN
             str='Number of initial values must equal sum of all the '
             ipos=52
             CALL getstr(OPDIC,opptr,POP,Optype,str(ipos:),nchr)
             IF(Lfatal)RETURN
             ipos=ipos+nchr
             str(ipos:)=' parameters in all the factors.'
             ipos=ipos+31
             CALL inpter(PERROR,Errpos,str(1:ipos),T)
             locok=F
c     -----------------------------------------------------------------
            ELSE
             ielt=ielt+1
c             Arimap(ielt)=PTONE
            END IF
           END IF
c     -----------------------------------------------------------------
           CALL lex()
           hvcmma=T
           opngrp=F
           GO TO 10
          END IF
c----------------------------------------------------------------------
c     There is not a close group or comma here so there must be a real.
c----------------------------------------------------------------------
          IF(.not.(getdbl(tmp)))THEN
           CALL inpter(PERROR,Lstpos,'Expected an real number not "'//
     &                 Nxttok(1:Nxtkln)//'"',T)
           locok=F
          ELSE IF(ielt.ge.endlag)THEN
           str='Number of initial values must equal sum of all the '
           ipos=52
           CALL getstr(OPDIC,opptr,POP,Optype,str(ipos:),nchr)
           IF(Lfatal)RETURN
           ipos=ipos+nchr
           str(ipos:)=' parameters in all the factors.'
           ipos=ipos+31
           CALL inpter(PERROR,Errpos,str(1:ipos),T)
           locok=F
c     -----------------------------------------------------------------
          ELSE
           ielt=ielt+1
           Arimap(ielt)=tmp
c-----------------------------------------------------------------------
c     Find out if the value is fixed or estimated.
c-----------------------------------------------------------------------
           CALL gtdcnm(FIXDIC,fixptr,PFIX,fixidx,argok)
           IF(argok)Arimaf(ielt)=fixidx.eq.FIXVAL
c     -----------------------------------------------------------------
           hvcmma=F
           opngrp=F
           GO TO 20
          END IF
c----------------------------------------------------------------------
c     Check for a comma after the last element and before the close of
c the list.
c----------------------------------------------------------------------
         ELSE IF(hvcmma.and..not.opngrp)THEN
          IF(ielt.ge.endlag)THEN
           str='Number of initial values must equal sum of all the '
           ipos=52
           CALL getstr(OPDIC,opptr,POP,Optype,str(ipos:),nchr)
           IF(Lfatal)RETURN
           ipos=ipos+nchr
           str(ipos:)=' parameters in all the factors.'
           ipos=ipos+31
           CALL inpter(PERROR,Errpos,str(1:ipos),T)
           locok=F
c     -----------------------------------------------------------------
          ELSE
           ielt=ielt+1
c           Arimap(ielt)=PTONE
          END IF
         END IF
c     -----------------------------------------------------------------
         IF(locok)THEN
          CALL lex()
c     -----------------------------------------------------------------
         ELSE
          CALL skplst(RPAREN)
         END IF
         GO TO 30
   10    CONTINUE
        END DO
   20   CONTINUE
       END DO
      END IF
c-----------------------------------------------------------------------
c     Allow empty lists ma=(), ielt=0 to be as if the argument has
c not been called.
c-----------------------------------------------------------------------
   30 IF(ielt.gt.beglag-1.and.ielt.ne.endlag)THEN
       str='Number of initial values must equal sum of all the '
       ipos=52
       CALL getstr(OPDIC,opptr,POP,Optype,str(ipos:),nchr)
       IF(Lfatal)RETURN
       ipos=ipos+nchr
       str(ipos:)=' parameters in all the factors.'
       ipos=ipos+31
       CALL inpter(PERROR,Errpos,str(1:ipos),T)
       locok=F
      END IF
c     -----------------------------------------------------------------
      Inptok=Inptok.and.locok
c     -----------------------------------------------------------------
      RETURN
      END
