C     Last change:  BCM  19 May 1998    4:29 pm
**==agr1.f    processed by SPAG 4.03F  at 11:38 on  7 Nov 1994
      SUBROUTINE agr1(Y,Ie)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C --- THIS SUBROUTINE IS FOR PRODUCING SEASONALLY ADJUSTED COMPOSITE
C --- SERIES.
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'ssap.prm'
      INCLUDE 'agr.cmn'
      INCLUDE 'agrsrs.cmn'
      INCLUDE 'ssap.cmn'
      INCLUDE 'sspdat.cmn'
      INCLUDE 'rev.prm'
      INCLUDE 'rev.cmn'
      INCLUDE 'revsrs.cmn'
*      INCLUDE 'hiddn.cmn'
c-----------------------------------------------------------------------
      LOGICAL F
      DOUBLE PRECISION ZERO
      PARAMETER(F=.false.,ZERO=0D0)
c-----------------------------------------------------------------------
      DOUBLE PRECISION Y
      INTEGER i,Ie
      DIMENSION Y(PLEN)
c-----------------------------------------------------------------------
      IF(Iagr.eq.0)THEN
c-----------------------------------------------------------------------
C --- INITIALIZATION
c-----------------------------------------------------------------------
       Ncomp=0
       Nrcomp=0
       Nscomp=0
       Indssp=NOTSET
       Indnfc=NOTSET
       Indnbc=NOTSET
       Iagr=1
       DO i=1,PLEN
        O(i)=ZERO
        O2(i)=ZERO
        O3(i)=ZERO
        O4(i)=ZERO
        O5(i)=ZERO
        Omod(i)=ZERO
        Ci(i)=ZERO
       END DO
       Lindao=F
       Lindls=F
       Lindcl=F
c-----------------------------------------------------------------------
C --- INITIALIZE SLIDING SPANS VARIABLES FOR INDIRECT RUNS
c-----------------------------------------------------------------------
       CALL setdp(ZERO,MXLEN*MXCOL,Saind)
       CALL setdp(ZERO,MXLEN*MXCOL,Sfind)
       CALL setdp(ZERO,MXLEN*MXCOL,Sfinda)
c-----------------------------------------------------------------------
C --- INITIALIZE Revisions history VARIABLES FOR INDIRECT RUNS
c-----------------------------------------------------------------------
       CALL setdp(ZERO,PREV,Cncisa)
       CALL setdp(ZERO,PREV,Finisa)
       Indrev=NOTSET
       Indrvs(YR)=0
       Indrvs(MO)=0
       RETURN
c-----------------------------------------------------------------------
C --- PRODUCE THE DIRECT SEASONALLY ADJUSTED COMPOSITE SERIES.
c-----------------------------------------------------------------------
      ELSE IF(Iagr.gt.0)THEN
       Iagr=3
       Ie=(Itest(5)-Itest(4))*Itest(1)+(Itest(3)-Itest(2))+1
       DO i=1,Ie
        Y(i)=dble(O(i+Ind1ob-1))
       END DO
       RETURN
      END IF
      Iagr=0
      Ncomp=0
      RETURN
      END
