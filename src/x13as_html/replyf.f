C     Last change:  BCM  13 Jul 2005    3:05 pm
      SUBROUTINE replyf()
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Replace leap year february regressor with appropriate length of
c     month/quarter regressor if kfulsm=2.
c     Brian Monsell, July 2005
c-----------------------------------------------------------------------
      LOGICAL F
      PARAMETER(F=.false.)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'error.cmn'
c-----------------------------------------------------------------------
      CHARACTER thisg*(PGRPCR),thisc*(PCOLCR),perstr*(7)
      DOUBLE PRECISION thisb
      LOGICAL thisf
      INTEGER icol,igrp,begcol,endcol,iper,nchr,thisty,ncol,idsp
c-----------------------------------------------------------------------
      perstr='Month  '
      iper=5
      idsp=2
      IF(Sp.eq.4)then
       perstr='Quarter'
       iper=7
       idsp=1
      END IF
c-----------------------------------------------------------------------
      DO igrp=1,Ngrp
       begcol=Grp(igrp-1)
       endcol=Grp(igrp)-1
       icol=endcol
       DO WHILE(icol.ge.begcol)
        IF(Rgvrtp(icol).eq.PRGTLY.or.Rgvrtp(icol).eq.PRRTLY.or.
     &     Rgvrtp(icol).eq.PRATLY)THEN
         thisb=B(icol)
         thisf=Regfx(icol)
         thisty=Rgvrtp(icol)
         IF(Rgvrtp(icol).eq.PRRTLY.or.Rgvrtp(icol).eq.PRATLY)THEN
          CALL getstr(Grpttl,Grpptr,Ngrp,igrp,thisg,nchr)
          IF(Lfatal)RETURN
         END IF
         CALL dlrgef(icol,Nspobs,1)
         IF(Lfatal)RETURN
         IF(thisty.eq.PRGTLY)THEN
          CALL adrgef(thisb,'Length-of-'//perstr(1:iper),
     &                'Length-of-'//perstr(1:iper),thisty-idsp,thisf,F)
         ELSE
          IF(thisty.eq.PRRTLY)THEN
           ncol=iper+12
           thisc(1:ncol)='Length-of-'//perstr(1:iper)//' I'
          ELSE
           ncol=iper+13
           thisc='Length-of-'//perstr(1:iper)//' II'
          ENDIF
          CALL adrgef(thisb,thisc(1:ncol),
     &                'Length-of-'//perstr(1:iper)//thisg(10:nchr),
     &                thisty-idsp,thisf,F)
         END IF
        END IF
        icol=icol-1
       END DO
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
