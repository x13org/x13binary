      SUBROUTINE gtdtvc(Havesp,Sp,Grpchr,Flgnul,Pelt,Datvec,Nelt,Locok,
     &                  Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     gtdtvc.f, Release 1, Subroutine Version 1.4, Modified 05 Oct 1994.
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'lex.i'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
c     ------------------------------------------------------------------
      CHARACTER str*(LINLEN)
      LOGICAL argok,Flgnul,Havesp,hvcmma,Inptok,Locok,opngrp
      INTEGER clsgtp,clsgrp,Datvec,defval,Grpchr,ipos,Nelt,Pelt,Sp,
     &        tmpdat
      DIMENSION Datvec(2,Pelt),defval(2),tmpdat(2)
      EXTERNAL clsgrp
      DATA defval/NOTSET,NOTSET/
c-----------------------------------------------------------------------
      Locok=T
c     ------------------------------------------------------------------
      IF(Nxtktp.eq.EOF)THEN
       Locok=F
c     ------------------------------------------------------------------
      ELSE IF(Nxtktp.ne.Grpchr)THEN
       CALL getdat(Havesp,Sp,Datvec,argok,Locok)
       IF(argok)THEN
        Nelt=1
c     ------------------------------------------------------------------
       ELSE IF(Nxtktp.ne.Grpchr)THEN
        CALL inpter(PERROR,Errpos,
     &              'Expected a date or a list of dates',T)
        Locok=F
       END IF
c     ------------------------------------------------------------------
      ELSE
       Nelt=0
       opngrp=T
       hvcmma=T
       clsgtp=clsgrp(Grpchr)
c     ------------------------------------------------------------------
       CALL lex()
c-----------------------------------------------------------------------
c     Process the date list
c-----------------------------------------------------------------------
       DO WHILE (T)
        IF(Nxtktp.ne.clsgtp)THEN
c-----------------------------------------------------------------------
c     Check for a NULL in the first place, for example, (,1992.feb)
c or (1967.jan,,1992.mar).  This section is repeated because there may
c be multiple NULLs
c-----------------------------------------------------------------------
         IF(Nxtktp.eq.COMMA)THEN
          IF(hvcmma.or.opngrp)THEN
           IF(Flgnul)THEN
            CALL inpter(PERROR,Errpos,
     &                  'Found a NULL date; check your commas.',T)
            Locok=F
c     ------------------------------------------------------------------
           ELSE IF(Nelt.ge.Pelt)THEN
            str='Date vector exceeds '
            ipos=21
            CALL itoc(Pelt,str,ipos)
            IF(Lfatal)RETURN
            str(ipos:)=', the maximum number of elements.'
            ipos=ipos+33
            CALL inpter(PERROR,Errpos,str(1:ipos-1),T)
            Locok=F
c     ------------------------------------------------------------------
           ELSE
            Nelt=Nelt+1
            CALL cpyint(defval,2,1,Datvec(1,Nelt))
           END IF
          END IF
c     ------------------------------------------------------------------
          CALL lex()
          hvcmma=T
          opngrp=F
          GO TO 10
         END IF
c-----------------------------------------------------------------------
c     There is not a close group or comma here so there must be a date.
c-----------------------------------------------------------------------
         CALL getdat(Havesp,Sp,tmpdat,argok,Locok)
         IF(.not.argok)THEN
          CALL inpter(PERROR,Errpos,'Expected a date not "'//
     &                Nxttok(1:Nxtkln)//'"',T)
          Locok=F
         ELSE IF(Nelt.ge.Pelt)THEN
          str='Date vector exceeds '
          ipos=21
          CALL itoc(Pelt,str,ipos)
          IF(Lfatal)RETURN
          str(ipos:)=', the maximum number of elements.'
          ipos=ipos+33
          CALL inpter(PERROR,Errpos,str(1:ipos-1),T)
          Locok=F
c     ------------------------------------------------------------------
         ELSE
          Nelt=Nelt+1
          CALL cpyint(tmpdat,2,1,Datvec(1,Nelt))
          hvcmma=F
          opngrp=F
          GO TO 10
         END IF
c-----------------------------------------------------------------------
c     Check for a comma after the last element and before the close of
c the list.  This indicates a NULL value, for example, (1967.jan,).
c-----------------------------------------------------------------------
        ELSE IF(hvcmma.and..not.opngrp)THEN
         IF(Flgnul)THEN
          CALL inpter(PERROR,Errpos,
     &                'Found a NULL date; check your commas.',T)
          Locok=F
c     ------------------------------------------------------------------
         ELSE IF(Nelt.ge.Pelt)THEN
          str='Date vector exceeds '
          ipos=21
          CALL itoc(Pelt,str,ipos)
          IF(Lfatal)RETURN
          str(ipos:)=', the maximum number of elements.'
          ipos=ipos+33
          CALL inpter(PERROR,Errpos,str(1:ipos-1),T)
          Locok=F
c     ------------------------------------------------------------------
         ELSE
          Nelt=Nelt+1
          CALL cpyint(defval,2,1,Datvec(1,Nelt))
         END IF
        END IF
c     ------------------------------------------------------------------
        IF(Locok)THEN
         CALL lex()
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
