      SUBROUTINE svaict(Savtd,Savlom,Saveas,Savusr,Lsvlog,Hvmdl,Lsumm,
     &                  Mdltxt,Nmdtxt)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'arima.cmn'
      INCLUDE 'picktd.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'htmlout.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      LOGICAL T,F
      DOUBLE PRECISION ZERO
      PARAMETER(T=.TRUE.,F=.FALSE.,ZERO=0D0)
c-----------------------------------------------------------------------
      CHARACTER rgstr*(155),rgudg*(6),rgabb*(50),Mdltxt*(9),temp*(30),
     &          tdabb*(80)
      INTEGER nrgchr,nrgudg,nrgabb,nrgab0,Lsumm,iaic,Nmdtxt,ntmp,ieas,j,
     &        ntdabb
      LOGICAL Savtd,Savlom,Saveas,Savusr,Lsvlog,Hvmdl
c-----------------------------------------------------------------------
      INTEGER strinx
      EXTERNAL strinx
c-----------------------------------------------------------------------
      IF(.not.(Savtd.or.Savlom.or.Saveas.or.Savusr))RETURN
      CALL setchr(' ',155,rgstr)
      CALL setchr(' ',80,tdabb)
      IF(Lsvlog)THEN
       Inlgfl=Inlgfl+1
       WRITE(Ng,1000)Inlgfl
       CALL mkTableTag(Ng,'w60','Results of AIC tests')
       CALL mkCaption(Ng,
     &        'Results of <abbr title="Akaike information criterion">'//
     &                'AIC</abbr> tests')
      END IF
      IF(Savtd)THEN
c-----------------------------------------------------------------------
       IF(Hvmdl)THEN
        iaic=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Trading Day')
        IF(iaic.eq.0)iaic=strinx(T,Grpttl,Grpptr,1,Ngrptl,
     &                           '1-Coefficient Trading Day')
        IF(iaic.eq.0)iaic=strinx(T,Grpttl,Grpptr,1,Ngrptl,
     &                           'Stock Trading Day')
        IF(iaic.eq.0)iaic=strinx(T,Grpttl,Grpptr,1,Ngrptl,
     &                           '1-Coefficient Stock Trading Day')
        IF(iaic.gt.0)THEN
         CALL mktdlb(rgstr,nrgchr,tdabb,ntdabb,Aicint,Aicstk,Tddate,
     &               Tdzero,Sp)
         IF(Lfatal)RETURN
         IF(Lsvlog)CALL mkAicRow(Ng,'Trading Day','accepted (',
     &                           rgstr(1:nrgchr),tdabb(1:ntdabb))
         IF(Lsumm.gt.0)WRITE(Nform,1010)'aictest.td: ',rgstr(1:nrgchr)
        ELSE
         IF(Lsvlog)CALL mkAicRow(Ng,'Trading Day', 'none','@','@')
         IF(Lsumm.gt.0)WRITE(Nform,1010)'aictest.td: no'
        END IF
        IF(Lsumm.gt.0)THEN
         WRITE(Nform,1040)'aictest.diff.td',Dfaict
         IF(Rgaicd(PTDAIC).gt.ZERO)
     &      WRITE(Nform,1040)'aictest.cvaic.td',Rgaicd(PTDAIC)
        END IF
       ELSE
        IF(Lsvlog)CALL mkAicRow(Ng,'Trading Day','ARIMA model not '//
     &                          Mdltxt(1:Nmdtxt),'@','@')
        IF(Lsumm.gt.0)WRITE(Nform,1010)'aictest.td: nomodel'
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(Savlom)THEN
       CALL mklnlb(rgstr,nrgchr,rgudg,nrgudg,rgabb,nrgabb,nrgab0,Lomtst,
     &             Lndate,Lnzero,Sp)
       IF(Lsumm.gt.0)
     &    WRITE(Nform,1010)'aictest.'//rgudg(1:nrgudg)//'.reg: ',
     &                     rgstr(1:nrgchr)
       IF(Hvmdl)THEN
        iaic=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Length-of-Month')
        IF(iaic.eq.0)iaic=strinx(T,Grpttl,Grpptr,1,Ngrptl,
     &                           'Length-of-Quarter')
        IF(iaic.eq.0)iaic=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Leap Year')
        IF(iaic.gt.0)THEN
         IF(Lsvlog)CALL mkAicRow(Ng,rgabb(1:nrgab0),'accepted (',
     &                           rgstr(1:nrgchr),rgabb(1:nrgabb))
         IF(Lsumm.gt.0)
     &      WRITE(Nform,1010)'aictest.'//rgudg(1:nrgudg)//': yes'
        ELSE
         IF(Lsvlog)CALL mkAicRow(Ng,rgabb(1:nrgab0),'rejected (',
     &                  rgstr(1:nrgchr),rgabb(1:nrgabb))
         IF(Lsumm.gt.0)
     &      WRITE(Nform,1010)'aictest.'//rgabb(1:nrgabb)//': no'
        END IF
        IF(Lsumm.gt.0)THEN
         WRITE(Nform,1040)'aictest.diff.'//rgabb(1:nrgabb),Dfaicl
         IF(Rgaicd(PTDAIC).gt.ZERO)
     &      WRITE(Nform,1040)'aictest.cvaic.'//rgabb(1:nrgabb),
     &                       Rgaicd(PLAIC)
        END IF
       ELSE
        IF(Lsvlog)CALL mkAicRow(Ng,rgabb(1:nrgab0),
     &                     'ARIMA model not '//Mdltxt(1:Nmdtxt),'@','@')
        IF(Lsumm.gt.0)
     &     WRITE(Nform,1010)'aictest.'//rgabb(1:nrgabb)//': nomodel'
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(Saveas)THEN
       IF(Lsumm.gt.0)THEN
        CALL mkealb(rgstr,nrgchr,Eastst,Easidx,Aicind,T,F)
        IF(Lfatal)RETURN
        WRITE(Nform,1010)'aictest.easter.reg: ',rgstr(1:nrgchr)
       END IF
       IF(Hvmdl)THEN
        iaic=strinx(T,Grpttl,Grpptr,1,Ngrptl,'Easter')
        IF(iaic.eq.0)
     &     iaic=strinx(T,Grpttl,Grpptr,1,Ngrptl,'StatCanEaster')
        IF(iaic.eq.0)
     &     iaic=strinx(T,Grpttl,Grpptr,1,Ngrptl,'StockEaster')
        IF(iaic.gt.0)THEN
         IF(Lsvlog)THEN
          IF(Aicind.eq.99)THEN
           ieas=1
           DO j=2,Neasvc-1
            CALL mkealb(temp,ntmp,Eastst,Easidx,Easvec(j)+Easidx,F,F)
            IF(.not.Lfatal)THEN
             rgstr(ieas:(ieas+ntmp))=temp(1:ntmp)//'+'
             ieas=ieas+ntmp+1
            END IF
            IF(Lfatal)RETURN
           END DO
           rgstr(ieas-1:ieas-1)=' '
           nrgchr=ieas-2
          ELSE
           CALL mkealb(rgstr,nrgchr,Eastst,Easidx,Aicind,F,T)
          END IF
          IF(Lfatal)RETURN
          CALL mkAicRow(Ng,'Easter', 
     &                    'accepted ('//rgstr(1:nrgchr)//')','@','@')
         END IF
         IF(Lsumm.gt.0)THEN
          WRITE(Nform,1010)'aictest.e: yes'
          IF(Aicind.eq.99)THEN
           WRITE(Nform,1030)'aictest.e.window: ',
     &                      (Easvec(j),j=2,Neasvc-1)
          ELSE
           WRITE(Nform,1020)'aictest.e.window: ',Aicind
          END IF
         END IF
        ELSE
         IF(Lsvlog)CALL mkAicRow(Ng,'Easter','rejected','@','@')
         IF(Lsumm.gt.0)THEN
          WRITE(Nform,1010)'aictest.e: no'
          WRITE(Nform,1020)'aictest.e.window: ',-99999
         END IF
        END IF
        IF(Lsumm.gt.0)THEN
         WRITE(Nform,1040)'aictest.diff.e',Dfaice
         IF(Rgaicd(PEAIC).gt.ZERO)
     &      WRITE(Nform,1040)'aictest.cvaic.e',Rgaicd(PEAIC)
        END IF
       ELSE
        IF(Lsvlog)
     &     CALL mkAicRow(Ng,'Easter',
     &                     'ARIMA model not '//Mdltxt(1:Nmdtxt),'@','@')
        IF(Lsumm.gt.0)THEN
         WRITE(Nform,1010)'aictest.e: nomodel'
         WRITE(Nform,1020)'aictest.e.window: ',-99999
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(Savusr)THEN
       IF(Hvmdl)THEN
        iaic=strinx(T,Grpttl,Grpptr,1,Ngrptl,'User-defined')
        IF(iaic.gt.0)THEN
         IF(Lsvlog)CALL mkAicRow(Ng,'User Defined Regressors',
     &                             'accepted','@','@')
         IF(Lsumm.gt.0)WRITE(Nform,1010)'aictest.u: yes'
        ELSE
         IF(Lsvlog)CALL mkAicRow(Ng,'User Defined Regressors',
     &                             'rejected','@','@')
         IF(Lsumm.gt.0)WRITE(Nform,1010)'aictest.u: no'
        END IF
        IF(Lsumm.gt.0)THEN
         WRITE(Nform,1040)'aictest.diff.u',Dfaicu
         IF(Rgaicd(PUAIC).gt.ZERO)
     &      WRITE(Nform,1040)'aictest.cvaic.e',Rgaicd(PUAIC)
        END IF
       ELSE
        IF(Lsvlog)
     &     CALL mkAicRow(Ng,'User Defined Regressors',
     &                   'ARIMA model not '//Mdltxt(1:Nmdtxt),'@','@')
        IF(Lsumm.gt.0)WRITE(Nform,1010)'aictest.u: nomodel'
       END IF
      END IF
c-----------------------------------------------------------------------
      IF(Lsvlog)THEN
       CALL writTag(Ng,'</table></div>')
       CALL mkPOneLine(Ng,'@','&nbsp;')
      END IF
c-----------------------------------------------------------------------
 1000 FORMAT('<div id="lgaic',i6.6,'">')
 1010 FORMAT(a:,a)
 1020 FORMAT(a,i6)
 1030 FORMAT(a,5i6)
 1040 FORMAT(a,': ',e20.10)
c-----------------------------------------------------------------------
      RETURN
      END
      