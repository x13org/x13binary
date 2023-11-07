C     Last change:  BCM   8 Dec 1998    2:24 pm
      SUBROUTINE getprt(Spcdsp,Nspctb,Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Parses the input for the print argument in each of the specs
c-----------------------------------------------------------------------
c     Variable typing and parameters initialization
c-----------------------------------------------------------------------
      INTEGER NLVL
      PARAMETER(NLVL=5)
c-----------------------------------------------------------------------
      INCLUDE 'lex.i'
      INCLUDE 'stdio.i'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'table.prm'
      INCLUDE 'level.prm'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'units.cmn'
c     ------------------------------------------------------------------
      LOGICAL addtbl,argok,hvcmma,Inptok,opngrp
      INTEGER i,itmp,Nspctb,Spcdsp,tblidx
c-----------------------------------------------------------------------
      CHARACTER LVLDIC*28
      INTEGER lvlidx,lvlptr,PLVL
      PARAMETER(PLVL=5)
      DIMENSION lvlptr(0:PLVL)
      PARAMETER(LVLDIC='defaultnonebriefalltablesall')
c-----------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
      LOGICAL tblmsk(NTBL)
c-----------------------------------------------------------------------
      DATA lvlptr/1,8,12,17,26,29/
c-----------------------------------------------------------------------
      INCLUDE 'table.var'
      INCLUDE 'level.var'
c-----------------------------------------------------------------------
c     Table argument
c-----------------------------------------------------------------------
      hvcmma=F
      lvlidx=1
      IF(Lnoprt)lvlidx=2
      CALL setlg(T,Nspctb,tblmsk(Spcdsp+1))
c-----------------------------------------------------------------------
c     Check for an unexpected EOF
c-----------------------------------------------------------------------
      IF(Nxtktp.eq.EOF)THEN
       Inptok=F
c-----------------------------------------------------------------------
c     Check for a single item, a level, or a table with an optional +/-
c-----------------------------------------------------------------------
      ELSE IF(Nxtktp.ne.LPAREN)THEN
       CALL gtdcnm(LVLDIC,lvlptr,NLVL,itmp,argok)
       IF(argok.and.itmp.gt.0)THEN
        lvlidx=itmp
c     -----------------------------------------------------------------
       ELSE
        addtbl=T
c     -----------------------------------------------------------------
        IF(.not.argok)THEN
         IF(Nxtktp.eq.MINUS.or.Nxtktp.eq.PLUS)THEN
          IF(Nxtktp.eq.MINUS)addtbl=F
          CALL lex()
c     -----------------------------------------------------------------
         ELSE
          CALL inpter(PERROR,Lstpos,
     &                'Prefix must be "+", "-", or nothing.',T)
          CALL lex()
          Inptok=F
         END IF
        END IF
c-----------------------------------------------------------------------
c     Check for a table
c-----------------------------------------------------------------------
        IF(Spcdsp.lt.BRKDSP)THEN
         CALL gtdcnm(TB1DIC,tb1ptr(2*Spcdsp),2*Nspctb,tblidx,argok)
        ELSE IF(Spcdsp.lt.BRKDS2)THEN
         CALL gtdcnm(TB2DIC,tb2ptr(2*(Spcdsp-BRKDSP)),2*Nspctb,tblidx,
     &               argok)
        ELSE IF(Spcdsp.lt.BRKDS3)THEN
         CALL gtdcnm(TB3DIC,tb3ptr(2*(Spcdsp-BRKDS2)),2*Nspctb,tblidx,
     &               argok)
        ELSE
         CALL gtdcnm(TB4DIC,tb4ptr(2*(Spcdsp-BRKDS3)),2*Nspctb,tblidx,
     &               argok)
        END IF
        IF(tblidx.eq.0)THEN
         CALL inpter(PERROR,Lstpos,
     &               'Print or level argument is not defined.',F)
         CALL writln('        Check the available table names and '//
     &               'levels for this spec.',STDERR,Mt2,F,T)
         CALL lex()
         Inptok=F
c     ------------------------------------------------------------------
        ELSE
         tblidx=Spcdsp+(tblidx+1)/2
         tblmsk(tblidx)=F
         Prttab(tblidx)=addtbl
        END IF
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
c or (6,,10.2,-8.3).  Check for multiple NULLs.
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
c     Check for a level, or a table with an optional +/-
c-----------------------------------------------------------------------
         CALL gtdcnm(LVLDIC,lvlptr,NLVL,itmp,argok)
         IF(argok.and.itmp.gt.0)THEN
          lvlidx=itmp
c     -----------------------------------------------------------------
         ELSE
          addtbl=T
c     -----------------------------------------------------------------
          IF(.not.argok)THEN
           IF(Nxtktp.eq.MINUS.or.Nxtktp.eq.PLUS)THEN
            IF(Nxtktp.eq.MINUS)addtbl=F
            CALL lex()
c     -----------------------------------------------------------------
           ELSE
            CALL inpter(PERROR,Lstpos,
     &                  'Prefix must be "+", "-", or nothing',T)
            CALL lex()
            Inptok=F
            GO TO 10
           END IF
          END IF
c-----------------------------------------------------------------------
c     Check for a table.
c-----------------------------------------------------------------------
          IF(Spcdsp.lt.BRKDSP)THEN
           CALL gtdcnm(TB1DIC,tb1ptr(2*Spcdsp),2*Nspctb,tblidx,argok)
          ELSE IF(Spcdsp.lt.BRKDS2)THEN
           CALL gtdcnm(TB2DIC,tb2ptr(2*(Spcdsp-BRKDSP)),2*Nspctb,
     &                 tblidx,argok)
          ELSE IF(Spcdsp.lt.BRKDS3)THEN
           CALL gtdcnm(TB3DIC,tb3ptr(2*(Spcdsp-BRKDS2)),2*Nspctb,
     &                 tblidx,argok)
          ELSE
           CALL gtdcnm(TB4DIC,tb4ptr(2*(Spcdsp-BRKDS3)),2*Nspctb,
     &                 tblidx,argok)
          END IF
          IF(tblidx.eq.0)THEN
           CALL inpter(PERROR,Lstpos,
     &                 'Print or level argument is not defined.',F)
           CALL writln('        Check the available table names and '//
     &                 'levels for this spec.',STDERR,Mt2,F,T)
           CALL lex()
           Inptok=F
c     -----------------------------------------------------------------
          ELSE
           tblidx=Spcdsp+(tblidx+1)/2
           tblmsk(tblidx)=F
           Prttab(tblidx)=addtbl
          END IF
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
         CALL lex()
         GO TO 20
        END IF
   10   CONTINUE
       END DO
      END IF
c----------------------------------------------------------------------
c     Construct the print tables for the spec
c----------------------------------------------------------------------
   20 IF(Inptok)THEN
       DO i=Spcdsp+1,Spcdsp+Nspctb
        IF(tblmsk(i))Prttab(i)=level(i,lvlidx)
       END DO
      END IF
c     -----------------------------------------------------------------
      RETURN
      END
