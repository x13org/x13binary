C     Last change:  BCM  23 Jul 1998    3:37 pm
      LOGICAL FUNCTION getfcn(Fcns,Fcnptr,Nfcns,Fcnidx,Fcnlog,Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     getfcn.f, Release 1, Subroutine Version 1.4, Modified 1/3/95.
c-----------------------------------------------------------------------
      INCLUDE 'lex.i'
      INCLUDE 'notset.prm'
c-----------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
c     ------------------------------------------------------------------
      CHARACTER Fcns*(*),fname*(LINLEN),linnum*5,colnum*5
      LOGICAL argok,Inptok
      INTEGER fcnpos,Fcnptr,Nfcns,Fcnidx,nfname,Fcnlog,ilin,icol
      DIMENSION fcnpos(2),Fcnptr(0:Nfcns),Fcnlog(2,Nfcns)
c-----------------------------------------------------------------------
      DO WHILE (T)
c-----------------------------------------------------------------------
       IF(Nxtktp.eq.EOF)THEN
        getfcn=F
c-----------------------------------------------------------------------
       ELSE
        getfcn=T
        fname=Nxttok(1:Nxtkln)
        nfname=Nxtkln
        CALL cpyint(Lstpos,2,1,fcnpos)
c-----------------------------------------------------------------------
        CALL gtdcnm(Fcns,Fcnptr,Nfcns,Fcnidx,argok)
c-----------------------------------------------------------------------
        IF(.not.argok)THEN
         CALL inpter(PERROR,fcnpos,
     &               'Expected specification name but found "'//
     &               fname(1:nfname)//'"',T)
         Inptok=F
         CALL skpfcn(fname,nfname)
c-----------------------------------------------------------------------
c     Added by BCM 12/28/94
c-----------------------------------------------------------------------
         GO TO 10
c-----------------------------------------------------------------------
        ELSE IF(Fcnidx.eq.0)THEN
         CALL inpter(PERROR,fcnpos,fname(1:nfname)//
     &               ' is not a valid spec name.',T)
         Inptok=F
         CALL skpfcn(fname,nfname)
         GO TO 10
c-----------------------------------------------------------------------
        ELSE
         IF(Nxtktp.eq.EOF)getfcn=F
         IF(Nxtktp.ne.LBRACE)THEN
          CALL inpter(PERROR,Lstpos,' Expected "{" but found '//
     &                Nxttok(1:Nxtkln),T)
          Inptok=F
          CALL skpfcn(fname,nfname)
          GO TO 10
         ELSE
          IF(Fcnlog(PLINE,Fcnidx).ne.NOTSET)THEN
           ilin=1
           CALL itoc(Fcnlog(PLINE,Fcnidx),linnum,ilin)
           icol=1
           CALL itoc(Fcnlog(PCHAR,Fcnidx),colnum,icol)
           CALL inpter(PERROR,fcnpos,fname(1:nfname)//
     &                 ' also found on line '//linnum(1:(ilin-1))//
     &                 ' position '//colnum(1:(icol-1))//
     &                 ' of the input file.',T)
           Inptok=F
           CALL skpfcn(fname,nfname)
           GO TO 10
          ELSE
           Fcnlog(PLINE,Fcnidx)=fcnpos(PLINE)
           Fcnlog(PCHAR,Fcnidx)=fcnpos(PCHAR)
           CALL lex()
          END IF
         END IF
        END IF
       END IF
c-----------------------------------------------------------------------
       RETURN
   10  CONTINUE
      END DO
      END

