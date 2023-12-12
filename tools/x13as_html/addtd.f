C     Last change:  BCM  22 Sep 1998   10:59 am
      SUBROUTINE addtd(Aicstk,Aicrgm,Aictd0,Sp,Tdindx)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     This subroutine adds trading day regressors for the 
c     automatic AIC test.
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      LOGICAL F
      PARAMETER(F=.FALSE.)
c-----------------------------------------------------------------------
      CHARACTER day*(3),tgrptl*(PGRPCR),datstr*(10)
      INTEGER Aicstk,Aicrgm,ipos,i,Sp,nchdat,nchr,Aictd0,Tdindx,ipos2,
     &        vartd,vartd1,vartd2,nvar
      DIMENSION Aicrgm(2),day(6)
c-----------------------------------------------------------------------
      DATA day/'Mon','Tue','Wed','Thu','Fri','Sat'/
c-----------------------------------------------------------------------
      IF(Tdindx.eq.0)RETURN
c-----------------------------------------------------------------------
      IF(Aicrgm(1).ne.NOTSET)THEN
       CALL wrtdat(Aicrgm,Sp,datstr,nchdat)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
c     Add stock trading day variables.
c-----------------------------------------------------------------------
      IF(Tdindx.eq.3.or.Tdindx.eq.6)THEN
       tgrptl='Stock Trading Day['
       ipos=19
       CALL itoc(Aicstk,tgrptl,ipos)
       IF(Lfatal)RETURN
       tgrptl(ipos:ipos)=']'
       IF(Tdindx.eq.6)THEN
        vartd= PRG1ST
        vartd1=PRR1ST
        vartd2=PRA1ST
        nvar=1
       ELSE
        vartd= PRGTST
        vartd1=PRRTST
        vartd2=PRATST
        nvar=6
       END IF
c-----------------------------------------------------------------------
c     Add 1 parameter trading day variables.
c-----------------------------------------------------------------------
      ELSE
       tgrptl='Trading Day'
       ipos=11
       IF(Tdindx.eq.4.or.Tdindx.eq.5)THEN
        vartd= PRG1TD
        vartd1=PRR1TD
        vartd2=PRA1TD
        nvar=1
       ELSE
        vartd= PRGTTD
        vartd1=PRRTTD
        vartd2=PRATTD
        nvar=6
       END IF
      END IF
      IF(Aictd0.eq.0)THEN
       ipos2=ipos
       IF(Aicrgm(1).ne.NOTSET)THEN
        ipos2=ipos+nchdat+9
        tgrptl(1:ipos2)=tgrptl(1:ipos)//' (after '//datstr(1:nchdat)//
     &                  ')'
       END IF
       DO i=1,nvar
        IF(nvar.eq.1)THEN
         CALL adrgef(DNOTST,'Weekday','1-Coefficient '//tgrptl(1:ipos2),
     &               vartd,F,F)
        ELSE
         CALL adrgef(DNOTST,day(i),tgrptl(1:ipos2),vartd,F,F)
        END IF
        IF(Lfatal)RETURN
       END DO
      END IF
      IF(Aicrgm(1).ne.NOTSET)THEN
       IF(Aictd0.ge.0)THEN
        IF(Aictd0.eq.0)THEN
         ipos2=ipos+nchdat+22
         tgrptl(1:ipos2)=tgrptl(1:ipos)//' (change for before '//
     &                    datstr(1:nchdat)//')'
        ELSE
         ipos2=ipos+nchdat+10
         tgrptl(1:ipos2)=tgrptl(1:ipos)//' (before '//
     &                   datstr(1:nchdat)//')'
        END IF
        IF(nvar.eq.1)THEN
         CALL adrgef(DNOTST,'Weekday I',
     &               '1-Coefficient '//tgrptl(1:ipos2),vartd1,F,F)
         IF(Lfatal)RETURN
        ELSE
         DO i=1,nvar
          CALL adrgef(DNOTST,day(i)//' I',tgrptl(1:ipos2),vartd1,F,F)
          IF(Lfatal)RETURN
         END DO
        END IF
       ELSE
        ipos2=ipos+nchdat+12
        tgrptl(1:ipos2)=tgrptl(1:ipos)//' (starting '//
     &                  datstr(1:nchdat)//')'
        IF(nvar.eq.1)THEN
         CALL adrgef(DNOTST,'Weekday II',
     &               '1-Coefficient '//tgrptl(1:ipos2),vartd2,F,F)
         IF(Lfatal)RETURN
        ELSE
         DO i=1,nvar
          CALL adrgef(DNOTST,day(i)//' II',tgrptl(1:ipos2),vartd2,F,F)
          IF(Lfatal)RETURN
         END DO
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
