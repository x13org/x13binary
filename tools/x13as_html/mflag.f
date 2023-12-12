C     Last change:  BCM  26 Feb 1999    3:41 pm
**==mflag.f    processed by SPAG 4.03F  at 12:23 on 21 Jun 1994
      SUBROUTINE mflag(X,Iopt,Iop2,Km,Dmax,Ncol,Ssdiff)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C  *****  THIS SUBROUTINE CONTROLS THE FLAGGING OF INDIVIDUAL MONTHS
C  *****  FOR A GIVEN SET OF SEASONALLY ADJUSTED ESTIMATES.  IN
C  *****  ADDITION, THE VALUES FOR THE SLIDING SPANS HISTOGRAM (KOUNT)
C  *****  ARE GENERATED HERE.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'ssap.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'ssap.cmn'
      INCLUDE 'sspvec.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION ZERO
      PARAMETER(ZERO=0D0)
c-----------------------------------------------------------------------
      LOGICAL Ssdiff
      DOUBLE PRECISION Dmax,X
      INTEGER i,Iop2,Iopt,j,k,Km,t2,numyr,numpr,Ncol
      DIMENSION X(MXLEN,MXCOL),Dmax(MXLEN,NEST),numyr(MXYR),numpr(PSP)
c-----------------------------------------------------------------------
c     Initialize counter variables
c-----------------------------------------------------------------------
      Ntot(Iopt)=0
      Chsgn(Iopt)=0
c      Iscf(Iopt)=0
      Iturn(Iopt)=0
c      Itf(Iopt)=0
      DO j=1,MXYR
       SSnyr(j,Iopt)=0
       Ayr(j,Iopt)=ZERO
       Ayrmx(Iopt)=ZERO
       numyr(j)=0
      END DO
      DO j=1,PSP
       SSnobs(j,Iopt)=0
       Aobs(j,Iopt)=ZERO
       Aobsmx(Iopt)=ZERO
       numpr(j)=0
      END DO
      DO k=1,4
       Kount(Iopt,k)=0
      END DO
c-----------------------------------------------------------------------
c     Set up character flags for each observation in sliding spans
c-----------------------------------------------------------------------
      DO i=1,Sslen+Im-1
       Per(i,Iopt)=0
       Csign(i,Iopt)=0
       Cturn(i,Iopt)=0
c-----------------------------------------------------------------------
c     If observation is common to less than two spans, set the 
c     maximum percent difference to null
c-----------------------------------------------------------------------
       IF(i.le.Km.or.i.ge.Sslen2)THEN
        Dmax(i,Iopt)=DNOTST
       ELSE
c-----------------------------------------------------------------------
c     Else, check if observation is flagged as extreme, has a turning 
c     point or a change of sign.
c-----------------------------------------------------------------------
        CALL rplus(X,i,Iopt,Iop2,Dmax(i,Iopt),numyr,numpr,Ncol,Ssdiff)
       END IF
      END DO
c-----------------------------------------------------------------------
c     Set histogram counts.
c-----------------------------------------------------------------------
      IF(.not.Ssdiff)THEN
       DO k=1,3
        Kount(Iopt,k)=Kount(Iopt,k)-Kount(Iopt,(k+1))
       END DO
      END IF
c-----------------------------------------------------------------------
c     Set time index
c-----------------------------------------------------------------------
      t2=(Sslen2-2)/Nsea
c-----------------------------------------------------------------------
C  *****  COMPUTE MONTHLY (AOBS) AND YEARLY (AYR) AVERAGES OF THE
C  *****  MAXIMUM PERCENTAGE DIFFERENCE.  
c-----------------------------------------------------------------------
      DO j=1,t2
       IF(numyr(j).eq.0)THEN
        Ayr(j,Iopt)=ZERO
       ELSE
        Ayr(j,Iopt)=Ayr(j,Iopt)/numyr(j)
        IF(Ayr(j,Iopt).gt.Ayrmx(Iopt))Ayrmx(Iopt)=Ayr(j,Iopt)
       END IF
      END DO
      DO j=1,Nsea
       Aobs(j,Iopt)=Aobs(j,Iopt)/numpr(j)
       IF(Aobs(j,Iopt).gt.Aobsmx(Iopt))Aobsmx(Iopt)=Aobs(j,Iopt)
      END DO
c-----------------------------------------------------------------------
c     Count number of sign changes, turning points in unstable months.
c     Commented out by BCM Jan-1998
c-----------------------------------------------------------------------
c      j1=Nsea+1
c      IF(Iop2.eq.3)j1=j1+Nsea
c      DO j=j1,Sslen2
c       IF(Per(j,Iopt).ne.'    '.and.Csign(j,Iopt).eq.'*'.and.Iopt.ne.3)
c     &    Iscf(Iopt)=Iscf(Iopt)+1
c     &    Itf(Iopt)=Itf(Iopt)+1
c      END DO
c-----------------------------------------------------------------------
      RETURN
      END
