c     Last Change: Mar. 2021-Fix automatic generated keys issue
      SUBROUTINE gtmtdt(Inptok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Get user defined metadata for X-13ARIMA-SEATS diagnostic output.
c----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'stdio.i'
      INCLUDE 'lex.i'
      INCLUDE 'metadata.prm'
      INCLUDE 'metadata.cmn'
      INCLUDE 'error.cmn'
c----------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
c     ------------------------------------------------------------------
      CHARACTER ckey*(5),cval*(5),thisky*(LINLEN),thatky*(LINLEN)
      LOGICAL argok,Inptok
      INTEGER i,ikey,j,jkey,ival
c     ------------------------------------------------------------------
c     metadata arguments data dictionary
c     ------------------------------------------------------------------
      CHARACTER MDTDIC*10,ikeystr*10
      INTEGER mdtidx,mdtptr,mdtlog,PMETA,ipos
      PARAMETER(PMETA=2)
      DIMENSION mdtptr(0:PMETA),mdtlog(2,PMETA)
      PARAMETER(MDTDIC='keysvalues')
c     ------------------------------------------------------------------
      LOGICAL gtarg
      EXTERNAL gtarg
c     ------------------------------------------------------------------
      DATA mdtptr/1,5,11/
c-----------------------------------------------------------------------
c     Initialize variables
c-----------------------------------------------------------------------
      argok=T
      CALL setint(NOTSET,2*PMETA,mdtlog)
c-----------------------------------------------------------------------
      DO WHILE (T)
c-----------------------------------------------------------------------
c     Argument get loop
c-----------------------------------------------------------------------
       IF(gtarg(MDTDIC,mdtptr,PMETA,mdtidx,mdtlog,Inptok))THEN
        IF(Lfatal)RETURN
        GO TO(10,20),mdtidx
c-----------------------------------------------------------------------
c     key argument
c-----------------------------------------------------------------------
   10   CALL getttl(LPAREN,T,PMTDAT,Keystr,Keyptr,Nkey,argok,Inptok)
        IF(Lfatal)RETURN
        GO TO 30
c-----------------------------------------------------------------------
c     value argument
c-----------------------------------------------------------------------
   20   CALL getttl(LPAREN,T,PMTDAT,Valstr,Valptr,Nval,argok,Inptok)
        IF(Lfatal)RETURN
        GO TO 30
c     ------------------------------------------------------------------
       END IF
       IF(Lfatal)RETURN
       IF(argok)Hvmtdt=T
c     ------------------------------------------------------------------
c      Check to see if keys have any prohibited characters
c     ------------------------------------------------------------------
       IF(Nkey.gt.0)THEN
        DO i=1,Nkey
         IF(argok)THEN
          CALL getstr(Keystr,Keyptr,Nkey,i,thisky,ikey)
          IF(Lfatal)RETURN
          ipos=index(thisky(1:ikey),' ')
          IF (ipos.gt.0) THEN
           CALL inpter(PERRNP,Pos,
     &       'Keys specified in metadata spec cannot contain spaces.',T)
           Hvmtdt=F
           argok=F
          END IF
          IF(argok)THEN
           ipos=index(thisky(1:ikey),':')
           IF (ipos.gt.0) THEN
            CALL inpter(PERRNP,Pos,
     &       'Keys specified in metadata spec cannot contain colons.',T)
            Hvmtdt=F
            argok=F
           END IF
          END IF
         END IF
        END DO
       END IF
c     ------------------------------------------------------------------
c     if no keys specified, create a set of keys for the values using
c     the template 'keyn'
c     ------------------------------------------------------------------
       IF(Nkey.eq.0.and.Nval.gt.0)THEN
        Keyptr(0)=1
        DO i=1,Nval
         ikeystr(1:3)='key'
         ikey=4
         CALL itoc(i,ikeystr,ikey)
         IF(Lfatal)RETURN
         CALL insstr(ikeystr(1:(ikey-1)),i,PMTDAT,Keystr,Keyptr,Nkey)
        END DO
c     ------------------------------------------------------------------
c     if not enough keys specified, print out warning message and
c     create a set of keys for the values using the template 'keyn'
c     ------------------------------------------------------------------
       ELSE IF(Nval.gt.Nkey)THEN
        ikey=1
        CALL itoc(Nkey,ckey,ikey)
        IF(Lfatal)RETURN
        ival=1
        CALL itoc(Nval,cval,ival)
        IF(Lfatal)RETURN
        CALL inpter(PWRNNP,Pos,'Fewer keys ('//ckey(1:(ikey-1))//
     &              ') than values ('//cval(1:(ival-1))//
     &              ') specified in metadata spec.',T)
        DO i=Nkey+1,Nval
         thisky(1:3)='key'
         ikey=4
         CALL itoc(i,thisky,ikey)
         IF(Lfatal)RETURN
         CALL insstr(thisky(1:(ikey-1)),i,PMTDAT,Keystr,Keyptr,Nkey)
        END DO
c     ------------------------------------------------------------------
c     if not enough values specified, print out an error message
c     ------------------------------------------------------------------
       ELSE IF(Nval.lt.Nkey)THEN
        ikey=1
        CALL itoc(Nkey,ckey,ikey)
        IF(Lfatal)RETURN
        ival=1
        CALL itoc(Nval,cval,ival)
        IF(Lfatal)RETURN
        CALL inpter(PERRNP,Pos,'Fewer values ('//ckey(1:(ikey-1))//
     &              ') than keys ('//cval(1:(ival-1))//
     &              ') specified in metadata spec.',T)
        Hvmtdt=F
        argok=F
c     ------------------------------------------------------------------
       ELSE IF(Nval.eq.0.and.Nkey.eq.0)THEN
        Hvmtdt=F
c     ------------------------------------------------------------------
       END IF
c     ------------------------------------------------------------------
c     Check if key values are unique
c     ------------------------------------------------------------------
       IF(argok.and.Hvmtdt)THEN
        DO i=1,Nval-1
         CALL getstr(Keystr,Keyptr,Nkey,i,thisky,ikey)
         IF(Lfatal)RETURN
         DO j=i+1,Nval
          CALL getstr(Keystr,Keyptr,Nkey,j,thatky,jkey)
          IF(Lfatal)RETURN
          IF(ikey.eq.jkey)THEN
           IF(thisky(1:ikey).eq.thatky(1:jkey))THEN
            CALL inpter(PERRNP,Pos,'Key values must be unique.',T)
            Hvmtdt=F
            argok=F
           END IF
          END IF
         END DO
        END DO
       END IF
c     ------------------------------------------------------------------
       Inptok=Inptok.and.argok 
       RETURN
  30   CONTINUE
      END DO
      END
