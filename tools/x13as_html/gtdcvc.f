C     Last change:  BCM   1 Feb 98    1:11 pm
      SUBROUTINE gtdcvc(Grpchr,Flgnul,Pelt,Args,Argptr,Nargs,Errmsg,
     &                  Idxvec,Nelt,LstTag,Locok,Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     gtdcvc.f, Release 1, Subroutine Version 1.3, Modified 1/3/95.
c-----------------------------------------------------------------------
c Given a dictionary of choices (Args, Argptr, and Nargs) it returns
c an integer index vector.  Entries not found in the dictionary would
c result in a element of 0.
c-----------------------------------------------------------------------
      INCLUDE 'lex.i'
      INCLUDE 'error.cmn'
c     ------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      CHARACTER Args*(*),Errmsg*(*),str*(LINLEN)
      LOGICAL argok,Flgnul,hvcmma,Inptok,Locok,opngrp,LstTag
      INTEGER Argptr,argidx,clsgtp,Grpchr,Idxvec,ipos,Nargs,Nelt,Pelt
      DIMENSION Argptr(0:Nargs),Idxvec(Pelt)
c-----------------------------------------------------------------------
      INTEGER clsgrp
      EXTERNAL clsgrp
c     ------------------------------------------------------------------
      Locok=T
      Nelt=0
      hvcmma=F
c     ------------------------------------------------------------------
      IF(Nxtktp.eq.EOF)THEN
       Locok=F
c-----------------------------------------------------------------------
c     Get just one name or quote
c-----------------------------------------------------------------------
      ELSE IF(Nxtktp.eq.NAME.or.Nxtktp.eq.QUOTE)THEN
       CALL gtdcnm(Args,Argptr,Nargs,argidx,argok)
       IF(.not.argok.or.argidx.eq.0)THEN
        CALL inpter(PERROR,Lstpos,Errmsg,T)
        Locok=F
       ELSE
        Nelt=1
        Idxvec(Nelt)=argidx
       END IF
c     ------------------------------------------------------------------
       Locok=Locok.and.argok
c     ------------------------------------------------------------------
      ELSE IF(Nxtktp.ne.Grpchr)THEN
       CALL inpter(PERROR,Lstpos,
     &             'Expected a name or a quote or a list of names or '//
     &             'quotes, not "'//Nxttok(1:Nxtkln)//'"',LstTag)
       Locok=F
       opngrp=F
       CALL lex()
c-----------------------------------------------------------------------
c     Get a list of names or quotes
c-----------------------------------------------------------------------
      ELSE
       opngrp=T
       clsgtp=clsgrp(Grpchr)
       CALL lex()
c     ------------------------------------------------------------------
       DO WHILE (T)
        IF(Nxtktp.ne.clsgtp)THEN
c-----------------------------------------------------------------------
c     Check for a NULL in the first place, for example, (,td,lom)
c or (const,,td,lom).  This section is repeated because there may be
c multiple NULLs
c-----------------------------------------------------------------------
         IF(Nxtktp.eq.COMMA)THEN
          IF(.not.(hvcmma.or.opngrp))THEN
           CALL lex()
          ELSE IF(Flgnul)THEN
           CALL inpter(PERROR,Lstpos,
     &                 'Found a NULL value; check your commas.',T)
           CALL lex()
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
           CALL gtdcnm(Args,Argptr,Nargs,argidx,argok)
           IF(.not.argok.or.argidx.eq.0)THEN
            CALL inpter(PERROR,Lstpos,Errmsg,T)
            CALL lex()
            Locok=F
           END IF
c     ------------------------------------------------------------------
           Nelt=Nelt+1
           Idxvec(Nelt)=argidx
           Locok=Locok.and.argok
c     ------------------------------------------------------------------
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
          IF(Lfatal)RETURN
          str(ipos:)=', the maximum number of elements.'
          ipos=ipos+33
          CALL inpter(PERROR,Lstpos,str(1:ipos-1),T)
          Locok=F
c     ------------------------------------------------------------------
         ELSE
          CALL gtdcnm(Args,Argptr,Nargs,argidx,argok)
          IF(.not.argok.or.argidx.eq.0)THEN
           CALL inpter(PERROR,Lstpos,Errmsg,T)
           CALL lex()
           Locok=F
          END IF
c     ------------------------------------------------------------------
          Nelt=Nelt+1
          Idxvec(Nelt)=argidx
          Locok=Locok.and.argok
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
          IF(Lfatal)RETURN
          str(ipos:)=', the maximum number of elements.'
          ipos=ipos+33
          CALL inpter(PERROR,Lstpos,str(1:ipos-1),T)
          Locok=F
c     ------------------------------------------------------------------
         ELSE
          CALL gtdcnm(Args,Argptr,Nargs,argidx,argok)
          IF(.not.argok.or.argidx.eq.0)THEN
           CALL inpter(PERROR,Lstpos,Errmsg,T)
           CALL lex()
           Locok=F
          END IF
c     ------------------------------------------------------------------
          Nelt=Nelt+1
          Idxvec(Nelt)=argidx
          Locok=Locok.and.argok
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
