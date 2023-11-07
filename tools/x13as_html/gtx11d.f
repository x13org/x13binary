C     Last change:  BCM  12 Mar 98   10:10 am
**==gtx11d.f    processed by SPAG 4.03F  at 11:37 on 10 Jun 1994
      SUBROUTINE gtx11d(Probs,Freq,Indec,Xfmind,Chnl,Start,Last,Nobs,Y,
     &                  Srsnam,File,Nfil,Argok)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'notset.prm'
      INCLUDE 'units.cmn'
      INCLUDE 'x11msc.cmn'
      INCLUDE 'htmlout.cmn'
c-----------------------------------------------------------------------
      LOGICAL T,F
      INTEGER PCUT2K,YR,MO
      PARAMETER(PCUT2K=45,T=.true.,F=.false.,YR=1,MO=2)
c-----------------------------------------------------------------------
      CHARACTER File*(PFILCR),Srsnam*(64),fmtdat*(PFILCR),lab*8
      LOGICAL havsrs,Argok
      INTEGER Freq,Nobs,Start,Chnl,year,Probs,ind,Indec,i,j,n,lenx11,
     &        nyy,Last,ic1,ny1,Nfil
      DOUBLE PRECISION Y
      DIMENSION Y(Probs),Start(2),Last(2)
c-----------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
      INTEGER Xfmind,xfmptr,PXFM,PX11F
      PARAMETER(PXFM=12,PX11F=7)
      DIMENSION xfmptr(0:PXFM)
      DATA xfmptr / 1,15,38,52,78,111,135,159,178,196,215,229,262 /
c-----------------------------------------------------------------------
c     Derive format from input code
c-----------------------------------------------------------------------
      ind=Xfmind
      IF(ind.eq.5)ind=6
      IF(ind.eq.8)ind=6
      IF(ind.eq.12)ind=5
      IF(ind.eq.10.or.ind.eq.11)ind=7
      IF(Freq.eq.4.and.ind.lt.6)ind=ind+PX11F
      IF(ind.eq.1)THEN
       WRITE(fmtdat,1010)Indec
      ELSE IF(ind.eq.2)THEN
       WRITE(fmtdat,1020)Indec,Indec
      ELSE IF(ind.eq.3)THEN
       WRITE(fmtdat,1030)Indec
      ELSE IF(ind.eq.4)THEN
       WRITE(fmtdat,1040)Indec,Indec
      ELSE IF(ind.eq.5)THEN
       WRITE(fmtdat,1050)Indec,Indec
      ELSE IF(ind.eq.6)THEN
       WRITE(fmtdat,1060)
      ELSE IF(ind.eq.7)THEN
       WRITE(fmtdat,1070)
      ELSE IF(ind.eq.8)THEN
       WRITE(fmtdat,1080)Indec
      ELSE IF(ind.eq.9)THEN
       WRITE(fmtdat,1090)Indec
      ELSE IF(ind.eq.10)THEN
       WRITE(fmtdat,1100)Indec
      ELSE IF(ind.eq.11)THEN
       WRITE(fmtdat,1110)Indec
      ELSE IF(ind.eq.12)THEN
       WRITE(fmtdat,1120)Indec
      END IF
      lenx11=xfmptr(Xfmind)-xfmptr(Xfmind-1)
c-----------------------------------------------------------------------
      lab=' '
      havsrs=F
      i=1
      n=6
      IF(ind.eq.5.or.ind.eq.6.or.ind.eq.7.or.ind.eq.12)n=8
      Last(MO)=Freq
c-----------------------------------------------------------------------
      IF(ind.eq.6.or.ind.eq.7)THEN
       ic1=12/Freq
       ny1=12
      ELSE
       ic1=1
       ny1=Freq
      END IF
c-----------------------------------------------------------------------
      DO WHILE (T)
       IF((ind.ge.3.and.ind.le.7).or.ind.ge.10)
     &    THEN
        READ(Chnl,fmtdat(1:lenx11),END=20,ERR=10)lab(1:n),year,
     &       (Y(j),j=i,i+ny1-1)
       ELSE
        READ(Chnl,fmtdat(1:lenx11),END=20,ERR=10)(Y(j),j=i,i+ny1-1),
     &       year,lab(1:n)
       END IF
       IF(ind.lt.5.or.ind.eq.6.or.(ind.gt.7.and.ind.lt.12))THEN
        IF(Yr2000.and.(year.le.PCUT2K))THEN
         year=year+2000
        ELSE
         year=year+1900
        END IF
       END IF
       IF(lab(1:n).eq.Srsnam(1:n))THEN
        havsrs=T
        IF(i.eq.1)THEN
         Start(YR)=year
         Start(MO)=1
         Nobs=ny1
         nyy=year
        ELSE
         Last(YR)=year
         Nobs=Nobs+ny1
         nyy=nyy+ic1
         IF(year.ne.nyy)THEN
          WRITE(STDERR,1150)nyy,Srsnam(1:n),year
          WRITE(Mt2,1151)nyy,Srsnam(1:n),year,Cbr
          Argok=F
          Nobs=0
          RETURN
         END IF
        END IF
        i=i+ny1
       ELSE IF(havsrs)THEN
        DO j=Nobs+1,Nobs+ny1
         Y(j)=DNOTST
        END DO
        RETURN
       END IF
      END DO
   10 CALL eWritln('Problem reading '//File(1:Nfil)//'.',STDERR,Mt2,T,F)
      CALL writln('        Check your input file and format.',
     &            STDERR,Mt2,F,T)
      Argok=F
      Nobs=0
      RETURN
   20 IF(.not.havsrs)THEN
       CALL eWritln('Cannot find series '//Srsnam(1:n)//' in file '//
     &              File(1:Nfil)//'.',STDERR,Mt2,T,F)
       CALL writln('        Check series name, input file, and format.',
     &             STDERR,Mt2,F,T)
       Argok=F
       Nobs=0
      ELSE
       DO WHILE((.not.dpeq(Y(i),DNOTST)).and.i.le.Probs)
        Y(i)=DNOTST
        i=i+1
       END DO
      END IF
c-----------------------------------------------------------------------
 1010 FORMAT('(12f6.',i1,',i2,a6)')
 1020 FORMAT('(6f12.',i1,',/,6f12.',i1,',i2,a6)')
 1030 FORMAT('(a6,i2,12f6.',i1,')')
 1040 FORMAT('(a6,i2,6f12.',i1,',/,8x,6f12.',i1,')')
 1050 FORMAT('(a8,i4,6f11.',i1,',2x,/,12x,6f11.',i1,',2x)')
 1060 FORMAT('(a8,i2,10x,12e16.10,18x)')
 1070 FORMAT('(a8,i4,12x,12e16.10,13x)')
 1080 FORMAT('(4(12x,f6.',i1,'),i2,a6)')
 1090 FORMAT('(4f12.',i1,',24x,i2,a6)')
 1100 FORMAT('(a6,i2,4(12x,f6.',i1,'))')
 1110 FORMAT('(a6,i2,4f12.',i1,')')
 1120 FORMAT('(a8,i4,4f11.',i1,',2x)')
 1150 FORMAT(' ERROR: Expected to find year ',i4,' of series ',a,
     &       ' not ',i4,'.',/,'        Check input file and format.',/)
 1151 FORMAT('<p><strong>ERROR:</strong> Expected to find year ',i4,
     &       ' of series ',a,' not ',i4,'.',a,/,
     &       ' Check input file and format.</p>',/)
c-----------------------------------------------------------------------
      RETURN
      END
