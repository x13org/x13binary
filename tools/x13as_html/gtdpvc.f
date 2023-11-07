C     Last change:  BCM   1 Feb 98    2:01 pm
**==gtdpvc.f    processed by SPAG 4.03F  at 09:49 on  1 Mar 1994
      SUBROUTINE gtdpvc(Grpchr,Flgnul,Pelt,Avec,Nelt,Locok,Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     gtdpvc.f, Release 1, Subroutine Version 1.6, Modified 1/3/95.
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'lex.i'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      CHARACTER str*(LINLEN)
      LOGICAL Flgnul,hvcmma,Inptok,Locok,opngrp
      INTEGER clsgtp,Grpchr,ipos,Nelt,Pelt
      DOUBLE PRECISION Avec,tmp
      DIMENSION Avec(Pelt)
c     ------------------------------------------------------------------
      LOGICAL getdbl
      INTEGER clsgrp
      EXTERNAL clsgrp,getdbl
c     ------------------------------------------------------------------
      Locok=T
c     ------------------------------------------------------------------
      IF(Nxtktp.eq.EOF)THEN
       Locok=F
c     ------------------------------------------------------------------
      ELSE IF(getdbl(Avec(1)))THEN
       Nelt=1
c     ------------------------------------------------------------------
      ELSE IF(Nxtktp.ne.Grpchr)THEN
       CALL inpter(PERROR,Lstpos,
     &         'Expected a real number or a list of real numbers, not "'
     &           //Nxttok(1:Nxtkln)//'"',T)
       Locok=F
       opngrp=F
       CALL lex()
c     ------------------------------------------------------------------
      ELSE
       Nelt=0
       opngrp=T
       hvcmma=F
       clsgtp=clsgrp(Grpchr)
c     ------------------------------------------------------------------
       CALL lex()
c-----------------------------------------------------------------------
c     Process the list of doubles
c-----------------------------------------------------------------------
       DO WHILE (T)
        IF(Nxtktp.ne.clsgtp)THEN
c-----------------------------------------------------------------------
c     Check for a NULL in the first place, for example, (,10.2, -8.3)
c or (6,,10.2,-8.3).  This section is repeated because there may be
c multiple NULLs
c-----------------------------------------------------------------------
         IF(Nxtktp.eq.COMMA)THEN
          IF(hvcmma.or.opngrp)THEN
           IF(Flgnul)THEN
            CALL inpter(PERROR,Lstpos,
     &                  'Found a NULL value; check your commas.',T)
            Locok=F
c     ------------------------------------------------------------------
           ELSE IF(Nelt.ge.Pelt)THEN
            str='Real Vector exceeds '
            ipos=21
            CALL itoc(Pelt,str,ipos)
            IF(Lfatal)RETURN
            str(ipos:)=', the maximum number of elements.'
            ipos=ipos+33
            CALL inpter(PERROR,Lstpos,str(1:ipos-1),T)
            Locok=F
c     ------------------------------------------------------------------
           ELSE
            Nelt=Nelt+1
            Avec(Nelt)=DNOTST
           END IF
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
     &                Nxttok(1:Nxtkln)//'"',T)
          Locok=F
         ELSE IF(Nelt.ge.Pelt)THEN
          str='Real vector exceeds '
          ipos=21
          CALL itoc(Pelt,str,ipos)
          IF(Lfatal)RETURN
          str(ipos:)=', the maximum number of elements.'
          ipos=ipos+33
          CALL inpter(PERROR,Lstpos,str(1:ipos-1),T)
          Locok=F
c     ------------------------------------------------------------------
         ELSE
          Nelt=Nelt+1
          Avec(Nelt)=tmp
          hvcmma=F
          opngrp=F
          GO TO 10
         END IF
c-----------------------------------------------------------------------
c     Check for a comma after the last element and before the close of
c the list.  This indicates a NULL value, for example,
c (6.2,10.2, -8.3,).  These default values may exceed the length
c of the list.
c-----------------------------------------------------------------------
        ELSE IF(hvcmma.and..not.opngrp)THEN
         IF(Flgnul)THEN
          CALL inpter(PERROR,Lstpos,
     &                'Found a NULL value; check your commas.',T)
          Locok=F
c     ------------------------------------------------------------------
         ELSE IF(Nelt.ge.Pelt)THEN
          str='Real vector exceeds '
          ipos=21
          CALL itoc(Pelt,str,ipos)
          IF(Lfatal)RETURN
          str(ipos:)=', the maximum number of elements.'
          ipos=ipos+33
          CALL inpter(PERROR,Lstpos,str(1:ipos-1),T)
          Locok=F
c     ------------------------------------------------------------------
         ELSE
          Nelt=Nelt+1
          Avec(Nelt)=DNOTST
         END IF
        END IF
c     ------------------------------------------------------------------
        IF(Locok)THEN
         CALL lex()
c     ------------------------------------------------------------------
        ELSE
         CALL skplst(clsgtp)
        END IF
        GO TO 20
   10   CONTINUE
       END DO
c     ------------------------------------------------------------------
      END IF
   20 Inptok=Inptok.and.Locok
c     ------------------------------------------------------------------
      RETURN
      END
