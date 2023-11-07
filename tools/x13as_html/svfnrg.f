C     Last change:  BCM  13 May 1998    9:04 am
      SUBROUTINE svfnrg(Ttlstr,Ngrp,Grpttl,Grpptr,Ngrptl)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Constructs a description of the regression model
c At some point need to show that the matrix might be length of month
c adjusted
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'error.cmn'
*      INCLUDE 'title.cmn'
      INCLUDE 'units.cmn'
c     ------------------------------------------------------------------
      CHARACTER addon*3,str*(PGRPCR),tmpttl*80,Grpttl*(PGRPCR*PGRP),
     &          Ttlstr*(*),regttl*80
      INTEGER igrp,naddcr,nchr,nttlcr,Grpptr,Ngrptl,Ngrp,ifnreg,n1,
     &        nrgttl
      DIMENSION Grpptr(0:PGRP),regttl(10),nrgttl(10)
c-----------------------------------------------------------------------
c     Print the regression part of the model
c-----------------------------------------------------------------------
      nttlcr=0
      ifnreg=1
      CALL setchr(' ',80,tmpttl) 
      addon='   '
      naddcr=0
c     ------------------------------------------------------------------
      DO igrp=1,Ngrp
       CALL getstr(Grpttl,Grpptr,Ngrptl,igrp,str,nchr)
       IF(Lfatal)RETURN
c     ------------------------------------------------------------------
       IF(nttlcr+nchr+naddcr.ge.78)THEN
        CALL setchr(' ',80,regttl(ifnreg))
        n1=nttlcr+naddcr
        regttl(ifnreg)(1:n1)=tmpttl(1:nttlcr)//addon(1:naddcr)
        nrgttl(ifnreg)=n1
        ifnreg=ifnreg+1
        CALL setchr(' ',80,tmpttl)
        nttlcr=nchr
        tmpttl(1:nttlcr)=str(1:nchr)
c     ------------------------------------------------------------------
       ELSE
        n1=nttlcr+1
        IF(naddcr.gt.0)THEN
         tmpttl(n1:nttlcr+nchr+naddcr)=addon(1:naddcr)//str(1:nchr)
         nttlcr=nttlcr+nchr+naddcr
        ELSE
         tmpttl(n1:nttlcr+nchr)=str(1:nchr)
         nttlcr=nttlcr+nchr
         addon=' + '
         naddcr=3
        END IF
       END IF
      END DO
c     ------------------------------------------------------------------
      CALL setchr(' ',80,regttl(ifnreg))
      regttl(ifnreg)=tmpttl(1:nttlcr)
      nrgttl(ifnreg)=nttlcr
c     ------------------------------------------------------------------
      WRITE(Nform,1010)Ttlstr,ifnreg
 1010 FORMAT('n',a,': ',i3)
      DO igrp=1,ifnreg
       WRITE(Nform,1020)Ttlstr,igrp,regttl(igrp)(1:nrgttl(igrp))
 1020  FORMAT(a,i2.2,': ',a)
      END DO
c     ------------------------------------------------------------------
      RETURN
      END

