C     Last change:  BCM  22 Sep 1998   10:59 am
      SUBROUTINE addlom(Aicrgm,Aicln0,Sp,Lnindx)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     This subroutine adds trading day or holiday regressors for the 
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
      CHARACTER tgrptl*(PGRPCR),datstr*(10)
      INTEGER Aicstk,Aicrgm,ipos,Sp,nchdat,nchr,Aicln0,Lnindx,ipos2,
     &        varln,varln1,varln2
      DIMENSION Aicrgm(2)
c-----------------------------------------------------------------------
      IF(Aicrgm(1).ne.NOTSET)THEN
       CALL wrtdat(Aicrgm,Sp,datstr,nchdat)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
      IF(Lnindx.eq.0)RETURN
c-----------------------------------------------------------------------
c     Add lom/loq/lpyear regressors.
c-----------------------------------------------------------------------
      IF(Lnindx.eq.1)THEN
       tgrptl='Length-of-Month'
       ipos=15
       varln= PRGTLM
       varln1=PRRTLM
       varln2=PRATLM
      ELSE IF(Lnindx.eq.2)THEN
       tgrptl='Length-of-Quarter'
       ipos=17
       varln= PRGTLQ
       varln1=PRRTLQ
       varln2=PRATLQ
      ELSE
       tgrptl='Leap Year'
       ipos=9
       varln= PRGTLY
       varln1=PRRTLY
       varln2=PRATLY
      ENDIF
      IF(Aicln0.eq.0)THEN
       ipos2=ipos
       IF(Aicrgm(1).ne.NOTSET)THEN
        ipos2=ipos+nchdat+9
        tgrptl(1:ipos2)=tgrptl(1:ipos)//' (after '//datstr(1:nchdat)//
     &                  ')'
       END IF
       CALL adrgef(DNOTST,tgrptl(1:ipos),tgrptl(1:ipos2),varln,F,F)
       IF(Lfatal)RETURN
      END IF
      IF(Aicrgm(1).ne.NOTSET)THEN
       IF(Aicln0.ge.0)THEN
        IF(Aicln0.eq.0)THEN
         ipos2=ipos+nchdat+22
         tgrptl(1:ipos2)=tgrptl(1:ipos)//' (change for before '//
     &                    datstr(1:nchdat)//')'
        ELSE
         ipos2=ipos+nchdat+10
         tgrptl(1:ipos2)=tgrptl(1:ipos)//' (before '//
     &                   datstr(1:nchdat)//')'
        END IF
        CALL adrgef(DNOTST,tgrptl(1:ipos)//' I',tgrptl(1:ipos2),varln1,
     &              F,F)
        IF(Lfatal)RETURN
       ELSE
        ipos2=ipos+nchdat+12
        tgrptl(1:ipos2)=tgrptl(1:ipos)//' (starting '//
     &                  datstr(1:nchdat)//')'
        CALL adrgef(DNOTST,tgrptl(1:ipos)//' II',tgrptl(1:ipos2),varln2,
     &              F,F)
        IF(Lfatal)RETURN
       END IF
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
