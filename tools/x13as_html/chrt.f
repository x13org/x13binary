C     Last change:  BCM  25 Nov 97    2:57 pm
      SUBROUTINE chrt(Title,Ntitle,Icodeo,Noser,Nyc)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'tbltitle.prm'
      INCLUDE 'error.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'chrt.cmn'
c-----------------------------------------------------------------------
      CHARACTER Title*(PTTLEN),caver*1
      INTEGER icod2,icode,Icodeo,jx,l,Noser,Nyc,Ntitle
      DIMENSION Title(2),Ntitle(2)
c   --------------------------------------------------------------------
      icode=Icodeo
      I3='+'
      I4='I'
      I8='I'
      I9='I'
      I10=I4
      I11=I10
      I12='@'
      DO l=1,10
       Ip(l)=I3
      END DO
      Nseas=Nyc
      IF(Ymax.le.Ymin)RETURN
      icod2=0
      IF(icode.eq.15)THEN
       icod2=icode
       icode=5
      END IF
      IF(icode.eq.20.or.icode.eq.21)THEN
       icod2=icode-20
       icode=0
      END IF
      IF(icode.eq.17)THEN
       icod2=icode
       icode=7
      END IF
      IF(icode.eq.29)THEN
       IF(Muladd.eq.0)THEN
        icod2=icode
       ELSE
        icod2=19
       END IF
       icode=9 
      END IF
      CALL setup(icode,icod2)
      jx=2
      IF(icode.le.6)CALL yrly(icode,icod2,jx,Noser)
      IF(icode.eq.7)THEN
c       write(*,*) ' Enter month, icode = ',icode
       CALL month(icode,jx)
      END IF
      IF(icode.eq.9)THEN
       caver='I'
c       write(*,*) ' Enter aver, icode,N1 = ',icode,N1
       CALL aver(Y1,N1,caver,icode,icod2,jx)
      END IF
      IF(Lfatal.or.icode.lt.0)RETURN
c     ------------------------------------------------------------------
c     Change 9/96 to handle more than one title
c     ------------------------------------------------------------------
      CALL outchr(Title,Ntitle,icode,icod2)
      RETURN
      END
