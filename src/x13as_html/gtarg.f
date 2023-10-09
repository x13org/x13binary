**==gtarg.f    processed by SPAG 4.03F  at 09:49 on  1 Mar 1994
      LOGICAL FUNCTION gtarg(Args,Argptr,Nargs,Argidx,Arglog,Inptok)
c-----------------------------------------------------------------------
c     Gets a valid argument and the ='s from the input stream and
c positions the stream at the next argument, after the }.  If the
c argument is invalid (ie not in the dictionary) then the error is
c reported and an attempt is made to return the next argument.  This
c is done for checking purposes.
c-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'lex.i'
      INCLUDE 'notset.prm'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      CHARACTER argnm*(LINLEN),Args*(*),colnum*(5),linnum*(5)
      LOGICAL argok,Inptok
      INTEGER argpos,Argptr,nargcr,Nargs,Argidx,Arglog,icol,ilin
      DIMENSION argpos(2),Argptr(0:Nargs),Arglog(2,Nargs)
c     ------------------------------------------------------------------
      gtarg=T
      DO WHILE (T)
       CALL cpyint(Lstpos,2,1,argpos)
       nargcr=Nxtkln
       argnm=Nxttok(1:nargcr)
       CALL gtdcnm(Args,Argptr,Nargs,Argidx,argok)
       IF(Nxtktp.eq.EOF)THEN
        gtarg=F
       ELSE IF(Nxtktp.eq.RBRACE)THEN
        CALL lex()
        gtarg=F
c     ------------------------------------------------------------------
       ELSE IF(.not.argok)THEN
        CALL inpter(PERROR,argpos,
     &              'Expected argument name or "}" but found "'//
     &              argnm(1:nargcr)//'"',T)
        Inptok=F
        CALL lex()
        gtarg=F
c     ------------------------------------------------------------------
       ELSE IF(Argidx.eq.0)THEN
        CALL inpter(PERROR,argpos,'Argument name "'//argnm(1:nargcr)//
     &              '" not found',T)
        Inptok=F
        CALL lex()
        CALL skparg()
        IF(Lfatal)RETURN
        GO TO 10
c     ------------------------------------------------------------------
       ELSE IF(Nxtktp.ne.EQUALS)THEN
        CALL inpter(PERROR,Lstpos,' Expected "=" but found "'//
     &              Nxttok(1:Nxtkln)//'"',T)
        Inptok=F
        CALL skparg()
        IF(Lfatal)RETURN
        GO TO 10
c     ------------------------------------------------------------------
       ELSE
        IF(Arglog(PLINE,Argidx).ne.NOTSET)THEN
         ilin=1
         CALL itoc(Arglog(PLINE,Argidx),linnum,ilin)
         icol=1
         CALL itoc(Arglog(PCHAR,Argidx),colnum,icol)
         CALL inpter(PERROR,argpos,'Argument name "'//argnm(1:nargcr)//
     &               '" also found on line '//linnum(1:(ilin-1))//
     &               ' position '//colnum(1:(icol-1))//
     &               ' of the input file.',T)
         Inptok=F
         CALL lex()
         CALL skparg()
         IF(Lfatal)RETURN
         GO TO 10
        ELSE
         Arglog(PLINE,Argidx)=argpos(PLINE)
         Arglog(PCHAR,Argidx)=argpos(PCHAR)
         CALL lex()
        END IF
       END IF
c     ------------------------------------------------------------------
       RETURN
   10  CONTINUE
      END DO
      END
