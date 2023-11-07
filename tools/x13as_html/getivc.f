C     Last change:  BCM   1 Feb 98    0:40 am
      SUBROUTINE getivc(Grpchr,Flgnul,Pelt,Avec,Nelt,Locok,Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     getivc.f, Release 1, Subroutine Version 1.5, Modified 1/3/95.
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'lex.i'
c     ------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c     -----------------------------------------------------------------
      CHARACTER str*(LINLEN)
      LOGICAL Flgnul,getint,hvcmma,Inptok,Locok,opngrp
      INTEGER Avec,clsgtp,clsgrp,Grpchr,ipos,Nelt,Pelt,tmp
      DIMENSION Avec(Pelt)
      EXTERNAL clsgrp,getint
c----------------------------------------------------------------------
      Locok=T
c     -----------------------------------------------------------------
      IF(Nxtktp.eq.EOF)THEN
       Locok=F
c     -----------------------------------------------------------------
      ELSE IF(getint(Avec(1)))THEN
       Nelt=1
c     -----------------------------------------------------------------
      ELSE IF(Nxtktp.ne.Grpchr)THEN
       CALL inpter(PERROR,Lstpos,
     &             'Expected an integer or an integer list, not "'//
     &             Nxttok(1:Nxtkln)//'"',T)
       Locok=F
       opngrp=F
       CALL lex()
c     -----------------------------------------------------------------
      ELSE
       Nelt=0
       opngrp=T
       hvcmma=F
       clsgtp=clsgrp(Grpchr)
c     -----------------------------------------------------------------
       CALL lex()
c----------------------------------------------------------------------
c     Process the list of integers
c----------------------------------------------------------------------
       DO WHILE (T)
        DO WHILE (T)
c     -----------------------------------------------------------------
         IF(Nxtktp.ne.clsgtp)THEN
c----------------------------------------------------------------------
c     Check for a NULL in the first place, for example, (,10, -8)
c or (6,,10,-8).  This section is repeated because there may be
c multiple NULLs
c----------------------------------------------------------------------
          IF(Nxtktp.eq.COMMA)THEN
           IF(hvcmma.or.opngrp)THEN
            IF(Flgnul)THEN
             CALL inpter(PERROR,Lstpos,
     &                   'Found a NULL value; check your commas.',T)
             Locok=F
c     -----------------------------------------------------------------
            ELSE IF(Nelt.ge.Pelt)THEN
             str='Integer vector exceeds '
             ipos=24
             CALL itoc(Pelt,str,ipos)
             str(ipos:)=', the maximum number of elements.'
             ipos=ipos+33
             CALL inpter(PERROR,Lstpos,str(1:ipos-1),T)
             Locok=F
c     -----------------------------------------------------------------
            ELSE
             Nelt=Nelt+1
             Avec(Nelt)=NOTSET
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
          IF(.not.(getint(tmp)))THEN
           CALL inpter(PERROR,Lstpos,'Expected an integer not "'//
     &                 Nxttok(1:Nxtkln)//'"',T)
           Locok=F
          ELSE IF(Nelt.ge.Pelt)THEN
           str='Integer vector exceeds '
           ipos=23
           CALL itoc(Pelt,str,ipos)
           str(ipos:)=', the maximum number of elements.'
           ipos=ipos+33
           CALL inpter(PERROR,Lstpos,str(1:ipos-1),T)
           Locok=F
c     -----------------------------------------------------------------
          ELSE
           Nelt=Nelt+1
           Avec(Nelt)=tmp
           hvcmma=F
           opngrp=F
           GO TO 20
          END IF
c----------------------------------------------------------------------
c     Check for a comma after the last element and before the close of
c the list.  This indicates a NULL value, for example,
c (6,10, -8,).  These default values may exceed the length
c of the list.
c----------------------------------------------------------------------
         ELSE IF(hvcmma.and..not.opngrp)THEN
          IF(Flgnul)THEN
           CALL inpter(PERROR,Lstpos,
     &                 'Found a NULL value; check your commas.',T)
           Locok=F
c     -----------------------------------------------------------------
          ELSE IF(Nelt.ge.Pelt)THEN
           str='Integer vector exceeds '
           ipos=24
           CALL itoc(Pelt,str,ipos)
           str(ipos:)=', the maximum number of elements.'
           ipos=ipos+33
           CALL inpter(PERROR,Lstpos,str(1:ipos-1),T)
           Locok=F
c     -----------------------------------------------------------------
          ELSE
           Nelt=Nelt+1
           Avec(Nelt)=NOTSET
          END IF
c     -----------------------------------------------------------------
         ELSE IF(opngrp.and.Flgnul)THEN
          CALL inpter(PERROR,Lstpos,
     &                'Found a NULL value; check for null list.',T)
          Locok=F
         END IF
c     -----------------------------------------------------------------
         IF(Locok)THEN
          CALL lex()
         ELSE
          CALL skplst(clsgtp)
         END IF
         GO TO 30
   10    CONTINUE
        END DO
   20   CONTINUE
       END DO
c     -----------------------------------------------------------------
      END IF
   30 Inptok=Inptok.and.Locok
c     -----------------------------------------------------------------
      RETURN
      END
