c     Add Endmdl as argument to getreg, gtpdrg for a new format of the
c     end of the series for sequence outliers such as
c     AOSdate-0.0/LSSdate-0.0, March, 2021
C     Last change:  BCM  12 May 1998   11:19 am
**==gtpdrg.f    processed by SPAG 4.03F  at 09:50 on  1 Mar 1994
      SUBROUTINE gtpdrg(Begsrs,Endmdl,Nobs,Havsrs,Havesp,X11reg,
     &                  Havtd,Havhol,Havln,Havlp,Locok,Inptok)
      IMPLICIT NONE
c     ------------------------------------------------------------------
      INCLUDE 'lex.i'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      LOGICAL argok,Havesp,Havsrs,hvcmma,Inptok,Locok,opngrp,X11reg,
     &        Havtd,Havhol,Havln,Havlp
      INTEGER Begsrs,Endmdl,Nobs
      DIMENSION Begsrs(2),Endmdl(2)
c-----------------------------------------------------------------------
c     Assume the input is OK and we don't have any of the arguments
c-----------------------------------------------------------------------
      Locok=T
c     -----------------------------------------------------------------
      IF(Nxtktp.ne.EOF)THEN
c     -----------------------------------------------------------------
       IF(Nxtktp.eq.NAME.or.Nxtktp.eq.QUOTE)THEN
        CALL adpdrg(Begsrs,Endmdl,Nobs,Havsrs,Havesp,Nxttok,Nxtkln,
     &              X11reg,Havtd,Havhol,Havln,Havlp,argok,Locok)
        IF(Lfatal)RETURN
c     -----------------------------------------------------------------
       ELSE IF(Nxtktp.ne.LPAREN)THEN
        CALL inpter(PERROR,Lstpos,
     &           'Expected regression variable name or "(" but found "'
     &           //Nxttok(1:Nxtkln)//'"',T)
        CALL lex()
        Locok=F
c-----------------------------------------------------------------------
c     Get the list of regression variables
c-----------------------------------------------------------------------
       ELSE
        hvcmma=F
        opngrp=T
        CALL lex()
c     -----------------------------------------------------------------
        DO WHILE (T)
         DO WHILE (T)
          IF(Nxtktp.ne.RPAREN.and.Nxtktp.ne.EOF)THEN
c----------------------------------------------------------------------
c     Check for a NULL in the first place, for example, (,td,lom)
c or (const,,td,lom).  This section is repeated because there may be
c multiple NULLs
c----------------------------------------------------------------------
           IF(Nxtktp.eq.COMMA)THEN
            IF(hvcmma.or.opngrp)THEN
             CALL inpter(PERROR,Lstpos,
     &                   'Found a NULL value; check your commas.',T)
             Locok=F
            END IF
c     -----------------------------------------------------------------
            CALL lex()
            hvcmma=T
            opngrp=F
            GO TO 10
           END IF
c-----------------------------------------------------------------------
c     There is not a close group or comma here so there must be a NAME
c or a QUOTE.
c-----------------------------------------------------------------------
           IF(Nxtktp.ne.NAME.and.Nxtktp.ne.QUOTE)THEN
            CALL inpter(PERROR,Lstpos,
     &           'Expected regression variable name or ")" but found "'
     &           //Nxttok(1:Nxtkln)//'"',T)
            Locok=F
            CALL skplst(RPAREN)
c     ------------------------------------------------------------------
           ELSE
            CALL adpdrg(Begsrs,Endmdl,Nobs,Havsrs,Havesp,Nxttok,Nxtkln,
     &                  X11reg,Havtd,Havhol,Havln,Havlp,argok,Locok)
            IF(Lfatal)RETURN
            hvcmma=F
            opngrp=F
            GO TO 20
           END IF
c-----------------------------------------------------------------------
c     Check for a NULL in the first place, for example, (,td,lom)
c or (const,,td,lom).  This section is repeated because there may be
c multiple NULLs
c-----------------------------------------------------------------------
          ELSE IF(hvcmma)THEN
           CALL inpter(PERROR,Lstpos,
     &                 'Found a NULL value; check your commas.',T)
           Locok=F
          END IF
c     ------------------------------------------------------------------
          CALL lex()
          GO TO 30
   10     CONTINUE
         END DO
   20    CONTINUE
        END DO
       END IF
      END IF
c-----------------------------------------------------------------------
c     Overall error checks
c-----------------------------------------------------------------------
   30 Inptok=Inptok.and.Locok
c     ------------------------------------------------------------------
      RETURN
      END
