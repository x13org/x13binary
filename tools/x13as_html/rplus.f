C     Last change:  BCM  20 May 1998   11:10 am
      SUBROUTINE rplus(X,I,Nopt,Nop2,Mpd,Numyr,Numpr,Ncol,Ssdiff)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c  *****  this subroutine computes the maximum percentage difference
c  *****  r for observation i over ncol spans, and determines if
c  *****  this month should be flagged.  monthly (aobs) and yearly
c  *****  (ymon) averages of the maximum percentage difference are
c  *****  incremented, as well as the sliding spans histogram values
c  *****  (kount) and the number of months flagged for each month
c  *****  (nobs) and year (nyr).  determines if the estimates for
c  *****  observation i have undergone a change of direction.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'ssap.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'ssap.cmn'
      INCLUDE 'sspvec.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION ONEHND,ZERO
      PARAMETER(ONEHND=100D0,ZERO=0D0)
c-----------------------------------------------------------------------
      LOGICAL Lsaneg,Ssdiff
      DOUBLE PRECISION Mpd,X,xbase,xj,xmn,xmx,Saabav
      INTEGER I,ibase,ij,iy,j,j2,jay,jay2,jay3,jbase,k,mon,Nop2,Nopt,
     &        Numyr,Numpr,year,Ncol,temp
      DIMENSION X(MXLEN,MXCOL),Numyr(MXYR),Numpr(PSP)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
      COMMON /addneg/ Saabav,Lsaneg
c-----------------------------------------------------------------------
      mon=mod(I,Nsea)
      IF(mon.eq.0)mon=Nsea
      iy=(I-1)/Nsea
      year=Iyr+iy
      temp=0
      Csign(I,Nopt)=0
c-----------------------------------------------------------------------
c     Find the earliest span with a value of X that is defined (jay)
c-----------------------------------------------------------------------
      j=1
      DO WHILE (dpeq(X(I,j),DNOTST))
       j=j+1
      END DO
      jay=j
c-----------------------------------------------------------------------
c     Compute the minimum and maximum value of X for observation I,
c     and also the latest span with a value (jay2).
c-----------------------------------------------------------------------
      xmx=X(I,j)
      xmn=X(I,j)
      DO j2=jay,Ncol
       IF(.not.dpeq(X(I,j2),DNOTST))THEN
        IF(xmx.lt.X(I,j2))xmx=X(I,j2)
        IF(xmn.gt.X(I,j2))xmn=X(I,j2)
        j=j2
       END IF
      END DO
      jay2=j
c-----------------------------------------------------------------------
c     Compute the maximum percentage difference (Mpd)
c-----------------------------------------------------------------------
      Mpd=xmx-xmn
      IF((.not.Ssdiff).and.Nop2.eq.0)THEN
       IF(Lsaneg.and.(xmx.gt.0.and.xmn.lt.0))THEN
        Mpd=(Mpd/Saabav)*ONEHND
       ELSE IF(xmn.gt.0)THEN
        Mpd=(Mpd/xmn)*ONEHND
       ELSE 
        Mpd=(Mpd/abs(xmx))*ONEHND
       END IF
      END IF
c-----------------------------------------------------------------------
c     If observation I is before starting date of sliding spans 
c     comparisons, set label to dashes
c-----------------------------------------------------------------------
      IF(I.lt.Ic)THEN
       Per(I,Nopt)=-1
       RETURN
      END IF
c-----------------------------------------------------------------------
c     If trading day factors done, adjust total number of trading day
c     comparisons if observation tested is a non-leap year february.
c-----------------------------------------------------------------------
      IF(Nopt.eq.2.and.Nsea.eq.12.and.mon.eq.2.and.mod(year,4).gt.0)THEN
       Itot(Nopt)=Itot(Nopt)-1
      ELSE
c-----------------------------------------------------------------------
c     Update number of observations tested for a given year, month
c-----------------------------------------------------------------------
       Numpr(mon)=Numpr(mon)+1
       Numyr(iy)=Numyr(iy)+1
      END IF
c-----------------------------------------------------------------------
c     If maximum percent difference is greater than cutoff value,
c     set label, histogram variables to reflect magnitude of Mpd.
c-----------------------------------------------------------------------
      IF((.not.Ssdiff).and.Mpd.ge.Cut(Nopt,1))THEN
       DO k=1,4
        IF(Mpd.ge.Cut(Nopt,k))THEN
         temp=temp+1
         Kount(Nopt,k)=Kount(Nopt,k)+1
        END IF
       END DO
       Per(I,Nopt)=temp
c-----------------------------------------------------------------------
c     Also increment counters for number of observations flagged, as
c     well as number of observations within each calendar month/quarter
c     and year.
c-----------------------------------------------------------------------
       Ntot(Nopt)=Ntot(Nopt)+1
       SSnobs(mon,Nopt)=SSnobs(mon,Nopt)+1
       SSnyr(iy,Nopt)=SSnyr(iy,Nopt)+1
      END IF
c-----------------------------------------------------------------------
c     Add maximum percent difference to variables used to compute the
c     average MPD for given calendar months/quarters and years.
c-----------------------------------------------------------------------
      Aobs(mon,Nopt)=Aobs(mon,Nopt)+Mpd
      Ayr(iy,Nopt)=Ayr(iy,Nopt)+Mpd
c-----------------------------------------------------------------------
c     Check to see if there is a turning point over the spans
c-----------------------------------------------------------------------
      Cturn(I,Nopt)=0
      jay3=jay2-jay+1
      IF(jay3.ge.3)THEN
       ibase=0
       jbase=0
c       ittot=ittot+1
       DO j=jay+1,jay2
        IF(Cturn(I,Nopt).eq.0)THEN
         xj=X(I,j)-X(I,j-1)
         IF(xj.lt.ZERO)ibase=-1
         IF(xj.gt.ZERO)ibase=1
         IF(jbase.eq.0)THEN
          jbase=ibase
         ELSE IF(ibase.ne.jbase)THEN
          IF((.not.Ssdiff).and.Nop2.eq.0)xj=(xj/X(I,j-1))*ONEHND
          IF(abs(xj).gt.1)THEN
           Iturn(Nopt)=Iturn(Nopt)+1
           Cturn(I,Nopt)=1
          ELSE
           jbase=ibase
          END IF
         END IF
        END IF
       END DO
      END IF
c-----------------------------------------------------------------------
c     Check to see if there is a change in sign over the spans (except
c     if the seasonally adjusted data is being analyzed).
c-----------------------------------------------------------------------
      IF(Nopt.eq.3)RETURN
      jbase=0
      xbase=ONEHND
      IF(Ssdiff.or.Nop2.gt.0)xbase=ZERO
      DO j=jay,jay2
       IF(X(I,j).lt.xbase)THEN
        ibase=-1
       ELSE IF(X(I,j).gt.xbase)THEN
        ibase=1
       ELSE
        ibase=0
       END IF
       IF(jbase.eq.0)THEN
        jbase=ibase
       ELSE
        ij=ibase+jbase
        IF(ij.eq.0)THEN
         Chsgn(Nopt)=Chsgn(Nopt)+1
         Csign(I,Nopt)=1
         RETURN
        END IF
       END IF
      END DO
      RETURN
      END
