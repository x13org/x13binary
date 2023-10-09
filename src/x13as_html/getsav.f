C     Last change:  BCM   8 Dec 1998    2:24 pm
**==getsav.f    processed by SPAG 4.03F  at 11:17 on 14 Sep 1994
      SUBROUTINE getsav(Spcdsp,Nspctb,Inptok)
c-----------------------------------------------------------------------
c     Parses the input for the print argument in each of the specs
c-----------------------------------------------------------------------
c     Variable typing and parameters initialization
c-----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      INCLUDE 'lex.i'
      INCLUDE 'stdio.i'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'stable.prm'
      INCLUDE 'units.cmn'
c     ------------------------------------------------------------------
      LOGICAL hvcmma,Inptok,argok,opngrp
      INTEGER Nspctb,Spcdsp,tblidx
c-----------------------------------------------------------------------
      INCLUDE 'stable.var'
c-----------------------------------------------------------------------
      hvcmma=F
      IF(Nxtktp.eq.EOF)THEN
       Inptok=F
c-----------------------------------------------------------------------
c     Check for a list (, and possibly a null list).
c-----------------------------------------------------------------------
      ELSE IF(Nxtktp.ne.LPAREN)THEN
c-----------------------------------------------------------------------
c     Check for a table
c-----------------------------------------------------------------------
       IF(Spcdsp.lt.BRKDSP)THEN
        CALL gtdcnm(TB1DIC,tb1ptr(2*Spcdsp),2*Nspctb,tblidx,argok)
       ELSE IF(Spcdsp.lt.BRKDS2)THEN
        CALL gtdcnm(TB2DIC,tb2ptr(2*(Spcdsp-BRKDSP)),2*Nspctb,tblidx,
     &              argok)
       ELSE IF(Spcdsp.lt.BRKDS3)THEN
        CALL gtdcnm(TB3DIC,tb3ptr(2*(Spcdsp-BRKDS2)),2*Nspctb,tblidx,
     &              argok)
       ELSE
        CALL gtdcnm(TB4DIC,tb4ptr(2*(Spcdsp-BRKDS3)),2*Nspctb,tblidx,
     &              argok)
       END IF
       IF(tblidx.eq.0)THEN
        CALL inpter(PERROR,Lstpos,'Save argument is not defined.',F)
        CALL writln(
     &         '        Check the available table names for this spec.',
     &              STDERR,Mt2,F,T)
        CALL lex()
        Inptok=F
c     ------------------------------------------------------------------
       ELSE
        tblidx=Spcdsp+(tblidx+1)/2
        Savtab(tblidx)=T
       END IF
c-----------------------------------------------------------------------
c     Process a list (Could be a null list.)
c-----------------------------------------------------------------------
      ELSE
       opngrp=T
       CALL lex()
c     -----------------------------------------------------------------
       DO WHILE (T)
        IF(Nxtktp.eq.EOF)THEN
         CALL inpter(PERROR,Lstpos,'Unexpected EOF',T)
         Inptok=F
         GO TO 20
        ELSE IF(Nxtktp.ne.RPAREN)THEN
c-----------------------------------------------------------------------
c     Check for a NULL in the first place, for example, (,acf)
c or (acf,,pacf).  Check for multiple NULLs.
c-----------------------------------------------------------------------
         IF(Nxtktp.eq.COMMA)THEN
          IF(hvcmma.or.opngrp)THEN
           CALL inpter(PERROR,Lstpos,
     &                 'Found a NULL value; check your commas.',T)
           Inptok=F
          END IF
c     -----------------------------------------------------------------
          CALL lex()
          hvcmma=T
          opngrp=F
          GO TO 10
         END IF
c-----------------------------------------------------------------------
c     Check for a table.
c-----------------------------------------------------------------------
         IF(Spcdsp.lt.BRKDSP)THEN
          CALL gtdcnm(TB1DIC,tb1ptr(2*Spcdsp),2*Nspctb,tblidx,argok)
         ELSE IF(Spcdsp.lt.BRKDS2)THEN
          CALL gtdcnm(TB2DIC,tb2ptr(2*(Spcdsp-BRKDSP)),2*Nspctb,tblidx,
     &                argok)
         ELSE IF(Spcdsp.lt.BRKDS3)THEN
          CALL gtdcnm(TB3DIC,tb3ptr(2*(Spcdsp-BRKDS2)),2*Nspctb,tblidx,
     &                argok)
         ELSE
          CALL gtdcnm(TB4DIC,tb4ptr(2*(Spcdsp-BRKDS3)),2*Nspctb,tblidx,
     &                argok)
         END IF
         IF(tblidx.eq.0)THEN
          CALL inpter(PERROR,Lstpos,'Save argument is not defined.',F)
          CALL writln('        Check the available table names for '//
     &                'this spec.',STDERR,Mt2,F,T)
          CALL lex()
          Inptok=F
c-----------------------------------------------------------------------
c     Tables have long and short names so there are double the
c number of entries in the dictionary and so entry 2i-1 and 2i
c map to table i.
c-----------------------------------------------------------------------
         ELSE
          tblidx=Spcdsp+(tblidx+1)/2
          Savtab(tblidx)=T
         END IF
c     -----------------------------------------------------------------
         hvcmma=F
         opngrp=F
c-----------------------------------------------------------------------
c     Check for a comma after the last element and before the close of
c the list.  This indicates a NULL value, for example, (acf,pacf,).
c-----------------------------------------------------------------------
        ELSE
         IF(hvcmma)THEN
          CALL inpter(PERROR,Lstpos,
     &                'Found a NULL value; check your commas.',T)
          Inptok=F
         END IF
c     ------------------------------------------------------------------
         CALL lex()
         GO TO 20
        END IF
   10   CONTINUE
       END DO
      END IF
c     ------------------------------------------------------------------
   20 RETURN
      END
