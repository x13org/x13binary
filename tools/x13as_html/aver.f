C     Last change:  BCM  25 Nov 97    2:57 pm
**==aver.f    processed by SPAG 4.03F  at 09:47 on  1 Mar 1994
      SUBROUTINE aver(Xval,N,I,Icode,Icod2,Jx)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      INCLUDE 'srslen.prm'
      INCLUDE 'units.cmn'
      INCLUDE 'chrt.cmn'
c     ------------------------------------------------------------------
      LOGICAL T
      PARAMETER(T=.true.)
c     ------------------------------------------------------------------
      DOUBLE PRECISION Xval,summ
      INTEGER Icod2,Icode,imnth,istep,j,Jx,jyave,jyval,k,l,N
C*** End of declarations inserted by SPAG
C PLOT BY MONTH AROUND MEAN OF EACH MONTH
C************************
      DIMENSION Xval(*)
      CHARACTER*1 itype
      CHARACTER*1 I
c     ------------------------------------------------------------------
      imnth=Ifrst
      itype=Ialpha(imnth)
      IF(Icode.eq.9)I=itype
      IF(Icod2.eq.19)THEN
       Xyvec=0D0
      ELSE IF(Icod2.eq.29)THEN
       Xyvec=100D0
      ELSE
       summ=0D0
       DO l=1,N
        summ=summ+Xval(l)
       END DO
       Xyvec=summ/dble(N)
      END IF
c      write(Mtprof,*)' Xyvec,Ymin,Ydiff = ', Xyvec,Ymin,Ydiff
c      write(Mtprof,*)' Fact1,Ifact2,N = ', Fact1,Ifact2,N
      CALL value
c      write(Mtprof,*)' Ixy = ', Ixy
      IF(Ixy.gt.110.or.Ixy.lt.1)THEN
       CALL nWritln('Cannot generate plot since expected value of '//
     &              'average not in plotting range.',Mt1,Mt2,T,T)
       Icode=-1
       RETURN
      END IF
      jyave=Ixy
      DO k=1,N
       Xyvec=Xval(k)
c       write(Mtprof,*)' Xyvec,Ymin,Ydiff = ', Xyvec,Ymin,Ydiff
c       write(Mtprof,*)' Fact1,Ifact2,k = ', Fact1,Ifact2,k
       CALL value
c       write(Mtprof,*)' Ixy = ', Ixy
       jyval=Ixy
       istep=1
       IF(jyval.lt.jyave)istep=-1
c       write(*,*)' jyave,jyval = ',jyave,jyval
       DO j=jyave,jyval,istep
        Ia(Jx,j)=I
       END DO
       IF((Icode-7)*(Icode-9).eq.0)Ia(Jx,jyave)=I1
       Jx=Jx+1
       IF(Icode.eq.9)THEN
        imnth=imnth+1
        IF(imnth.eq.Nseas+1)imnth=1
        I=Ialpha(imnth)
       END IF
      END DO
c     ------------------------------------------------------------------
      RETURN
      END

