C     Last change:  BCM   1 Dec 1998   10:23 am
      SUBROUTINE gtrgvl(Ielt,Fixvec,Bvec,Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Add initial value of the regression coefficients to the regARIMA
c     model
c-----------------------------------------------------------------------
      INCLUDE 'lex.i'
c      INCLUDE 'stdio.i'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
c      INCLUDE 'model.cmn'
c      INCLUDE 'mdldat.cmn'
c      INCLUDE 'usrreg.cmn'
c      INCLUDE 'units.cmn'
c      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
c      DOUBLE PRECISION ONE
      INTEGER FIXVAL
      LOGICAL T,F
      PARAMETER(T=.true.,F=.FALSE.,FIXVAL=1)
c      PARAMETER(ONE=1D0,T=.true.,F=.FALSE.,FIXVAL=1)
c     ------------------------------------------------------------------
      DOUBLE PRECISION Bvec,tmp
      LOGICAL argok,Fixvec,hvcmma,Inptok,locok,opngrp
      INTEGER Ielt
      DIMENSION Bvec(PB),Fixvec(PB)
c-----------------------------------------------------------------------
c      DOUBLE PRECISION Lam
c      INTEGER Fcntyp
c      COMMON /armalm/ Lam,Fcntyp
c     ------------------------------------------------------------------
      LOGICAL getdbl,dpeq
      INTEGER strinx
      EXTERNAL dpeq,getdbl,strinx
c     ------------------------------------------------------------------
      CHARACTER FIXDIC*2
      INTEGER fixidx,fixptr,PFIX
      PARAMETER(PFIX=2)
      DIMENSION fixptr(0:PFIX)
      PARAMETER(FIXDIC='fe')
c     ------------------------------------------------------------------
      DATA fixptr/1,2,3/
c-----------------------------------------------------------------------
c     Find the lags to initialize
c-----------------------------------------------------------------------
      ielt=0
c-----------------------------------------------------------------------
      locok=T
      hvcmma=F
c     ------------------------------------------------------------------
      IF(Nxtktp.eq.EOF)THEN
       locok=F
c-----------------------------------------------------------------------
c     Only a single value
c-----------------------------------------------------------------------
      ELSE IF(getdbl(tmp))THEN
       ielt=ielt+1
       Bvec(ielt)=tmp
c     ------------------------------------------------------------------
       CALL gtdcnm(FIXDIC,fixptr,PFIX,fixidx,argok)
       IF(argok)Fixvec(ielt)=fixidx.eq.FIXVAL
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
c-----------------------------------------------------------------------
          IF(Nxtktp.eq.COMMA)THEN
c     ------------------------------------------------------------------
           IF(hvcmma.or.opngrp)THEN
            ielt=ielt+1
c             Bvec(ielt)=PTONE
           END IF
c     ------------------------------------------------------------------
           CALL lex()
           hvcmma=T
           opngrp=F
           GO TO 10
          END IF
c-----------------------------------------------------------------------
c     There is not a close group or comma here so there must be a real.
c-----------------------------------------------------------------------
          IF(.not.(getdbl(tmp)))THEN
           CALL inpter(PERROR,Lstpos,'Expected an real number not "'//
     &                 Nxttok(1:Nxtkln)//'"',T)
           locok=F
          ELSE
           ielt=ielt+1
           Bvec(ielt)=tmp
c-----------------------------------------------------------------------
c     Find out if the value is fixed or estimated.
c-----------------------------------------------------------------------
           CALL gtdcnm(FIXDIC,fixptr,PFIX,fixidx,argok)
           IF(argok)Fixvec(ielt)=fixidx.eq.FIXVAL
c     ------------------------------------------------------------------
           hvcmma=F
           opngrp=F
           GO TO 20
          END IF
c-----------------------------------------------------------------------
c     Check for a comma after the last element and before the close of
c the list.
c-----------------------------------------------------------------------
         ELSE IF(hvcmma.and..not.opngrp)THEN
          ielt=ielt+1
c          Bvec(ielt)=PTONE
         END IF
c     ------------------------------------------------------------------
         IF(locok)THEN
          CALL lex()
c     ------------------------------------------------------------------
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
   30 Inptok=Inptok.and.locok
c     ------------------------------------------------------------------
      RETURN
      END
