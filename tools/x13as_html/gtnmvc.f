C     Last change:  BCM  23 Jul 1998    3:38 pm
      SUBROUTINE gtnmvc(Grpchr,Flgnul,Pelt,Chrvec,Ptrvec,Nelt,Eltlen,
     &                  Locok,Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     gtnmvc.f, Release 1, Subroutine Version 1.7, Modified 14 Feb 1995.
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'lex.i'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      CHARACTER Chrvec*(*),str*(LINLEN),str1*(5)
      LOGICAL Flgnul,hvcmma,Inptok,Locok,opngrp
      INTEGER clsgtp,clsgrp,Grpchr,ipos,Nelt,Pelt,Ptrvec,Eltlen,nstr1
      DIMENSION Ptrvec(0:Pelt)
      EXTERNAL clsgrp
c     ------------------------------------------------------------------
      Locok=T
      CALL intlst(Pelt,Ptrvec,Nelt)
c     ------------------------------------------------------------------
      IF(Nxtktp.eq.EOF)THEN
       Locok=F
c-----------------------------------------------------------------------
c     Get just one name or quote
c-----------------------------------------------------------------------
      ELSE IF(Nxtktp.eq.NAME.or.Nxtktp.eq.QUOTE)THEN
       IF(Nxtkln.eq.0)THEN
        IF(Nxtktp.eq.NAME)
     &     CALL inpter(PERROR,Lstpos,'Expected a NAME, QUOTE, or '//
     &                 'list of either, not an empty string.',T)
        Locok=F
       ELSE
        IF(Pelt.eq.1.and.Nxtkln.gt.len(Chrvec))THEN
         nstr1=1
         CALL itoc(len(Chrvec),str1,nstr1)
         CALL inpter(PERROR,Lstpos,
     &               'Values for this argument cannot be longer than '//
     &               str1(1:nstr1-1)//' characters.',T)
         Locok=F
        ELSE 
         CALL putstr(Nxttok(1:Nxtkln),Pelt,Chrvec,Ptrvec,Nelt)
         IF(Lfatal)RETURN
        END IF
       END IF
       CALL lex()
c     ------------------------------------------------------------------
      ELSE IF(Nxtktp.ne.Grpchr)THEN
       CALL inpter(PERROR,Lstpos,
     &             'Expected a NAME or a QUOTE or a list of either, '//
     &             'not "'//Nxttok(1:Nxtkln)//'"',T)
       Locok=F
       opngrp=F
       CALL lex()
c-----------------------------------------------------------------------
c     Get a list of names or quotes
c-----------------------------------------------------------------------
      ELSE
       opngrp=T
       hvcmma=F
       clsgtp=clsgrp(Grpchr)
       DO WHILE (T)
c     ------------------------------------------------------------------
        CALL lex()
c     ------------------------------------------------------------------
        IF(Nxtktp.ne.clsgtp)THEN
c-----------------------------------------------------------------------
c     Check for a NULL in the first place, for example, (,td,lom)
c or (const,,td,lom).  This section is repeated because there may be
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
            str='List of names exceeds '
            ipos=23
            CALL itoc(Pelt,str,ipos)
            IF(Lfatal)RETURN
            str(ipos:)=', the maximum number of elements.'
            ipos=ipos+33
            CALL inpter(PERROR,Lstpos,str(1:ipos-1),T)
            Locok=F
c     ------------------------------------------------------------------
           ELSE
            CALL putstr(CNOTST,Pelt,Chrvec,Ptrvec,Nelt)
            IF(Lfatal)RETURN
           END IF
          END IF
c     ------------------------------------------------------------------
          hvcmma=T
          opngrp=F
          GO TO 10
         END IF
c-----------------------------------------------------------------------
c     There is not a close group or comma here so there must be a NAME
c or a QUOTE.
c-----------------------------------------------------------------------
         IF(Nxtktp.ne.NAME.and.Nxtktp.ne.QUOTE)THEN
          CALL inpter(PERROR,Lstpos,'Expected a NAME or QUOTE not "'//
     &                Nxttok(1:Nxtkln)//'"',T)
          Locok=F
         ELSE IF(Nelt.ge.Pelt)THEN
          str='List of names exceeds '
          ipos=23
          CALL itoc(Pelt,str,ipos)
          str(ipos:)=', the maximum number of elements.'
          ipos=ipos+33
          CALL inpter(PERROR,Lstpos,str(1:ipos-1),T)
          Locok=F
c     ------------------------------------------------------------------
         ELSE 
          IF(Nxtkln.eq.0)THEN
           IF(Nxtktp.eq.NAME)
     &        CALL inpter(PERROR,Lstpos,'Expected a NAME, QUOTE, or '//
     &                    'list of either, not an empty string.',T)
           Locok=F
c     ------------------------------------------------------------------
          ELSE IF(Pelt.gt.1.and.Nxtkln.gt.Eltlen)THEN
           nstr1=1
           CALL itoc(Eltlen,str1,nstr1)
           CALL inpter(PERROR,Lstpos,
     &               'Values for this argument cannot be longer than '//
     &               str1(1:nstr1-1)//' characters.',T)
           Locok=F
          ELSE
           CALL putstr(Nxttok(1:Nxtkln),Pelt,Chrvec,Ptrvec,Nelt)
           IF(Lfatal)RETURN
          END IF
          hvcmma=F
          opngrp=F
          GO TO 10
         END IF
c-----------------------------------------------------------------------
c     Check for a NULL after the last element but before the close of
c the list.   This indicates a NULL value, for example, (td,lom,).
c These default values may exceed the length of the list.
c-----------------------------------------------------------------------
        ELSE IF(hvcmma.and..not.opngrp)THEN
         IF(Flgnul)THEN
          CALL inpter(PERROR,Lstpos,
     &                'Found a NULL value; check your commas.',T)
          Locok=F
c     ------------------------------------------------------------------
         ELSE IF(Nelt.ge.Pelt)THEN
          str='List of names exceeds '
          ipos=23
          CALL itoc(Pelt,str,ipos)
          str(ipos:)=', the maximum number of elements.'
          ipos=ipos+33
          CALL inpter(PERROR,Lstpos,str(1:ipos-1),T)
          Locok=F
c     ------------------------------------------------------------------
         ELSE
          CALL putstr(CNOTST,Pelt,Chrvec,Ptrvec,Nelt)
          IF(Lfatal)RETURN
         END IF
        END IF
c     ------------------------------------------------------------------
        IF(Locok)THEN
         CALL lex()
        ELSE
         CALL skplst(clsgtp)
        END IF
        GO TO 20
   10  CONTINUE
      END DO
c     ------------------------------------------------------------------
      END IF
   20 Inptok=Inptok.and.Locok
c     ------------------------------------------------------------------
      RETURN
      END

