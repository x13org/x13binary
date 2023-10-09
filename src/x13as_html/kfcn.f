C     Last change:  BCM  24 Nov 97   12:48 pm
      SUBROUTINE kfcn(Begdat,Nrxy,Xdev,Xelong)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     This subroutine computes a function that removes the easter mean
c     effect from combined calendar runs.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'model.prm'
      INCLUDE 'model.cmn'
      INCLUDE 'mdldat.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'xtdtyp.cmn'
c-----------------------------------------------------------------------
      CHARACTER icoltl*(PCOLCR)
      LOGICAL Xelong
      INTEGER Begdat,ndays,predat,Nrxy,i,ipos,Xdev,irow,ctoi,nchr,ir2,m,
     &        mnindx,idate
      DOUBLE PRECISION xemean,lmeans,tmeans
      DIMENSION Begdat(2),predat(2),idate(2),xemean(PSP),lmeans(25,8:9),
     &          tmeans(25,11:12)
      EXTERNAL ctoi
c-----------------------------------------------------------------------
      DATA(lmeans(i,8),i=1,25)/
     & .8800D0,.8750D0,.8696D0,.8636D0,.8571D0,.8500D0,.8421D0,.8333D0,
     & .8235D0,.8125D0,.8000D0,.7857D0,.7692D0,.7500D0,.7273D0,.7000D0,
     & .6667D0,.6250D0,.5714D0,.5000D0,.4286D0,.3571D0,.2857D0,.2143D0,
     & .1429D0/
      DATA(lmeans(i,9),i=1,25)/
     & .1200D0,.1250D0,.1304D0,.1364D0,.1429D0,.1500D0,.1579D0,.1667D0,
     & .1765D0,.1875D0,.2000D0,.2143D0,.2308D0,.2500D0,.2727D0,.3000D0,
     & .3333D0,.3750D0,.4286D0,.5000D0,.5714D0,.6429D0,.7143D0,.7857D0,
     & .8571D0/
      DATA(tmeans(i,11),i=1,25)/
     & .4884D0,.4773D0,.4656D0,.4534D0,.4406D0,.4273D0,.4132D0,.3985D0,
     & .3830D0,.3667D0,.3494D0,.3313D0,.3120D0,.2917D0,.2700D0,.2471D0,
     & .2226D0,.1684D0,.1384D0,.1062D0,.0776D0,.0530D0,.0326D0,.0167D0,
     & .0057D0/
      DATA(tmeans(i,12),i=1,25)/
     & .5116D0,.5227D0,.5344D0,.5466D0,.5594D0,.5727D0,.5868D0,.6015D0,
     & .6170D0,.6333D0,.6506D0,.6687D0,.6880D0,.7083D0,.7300D0,.7529D0,
     & .7774D0,.8316D0,.8616D0,.8938D0,.9224D0,.9470D0,.9674D0,.9833D0,
     & .9943D0/
c-----------------------------------------------------------------------
      DO i=1,Nb
       IF(Rgvrtp(i).eq.PRGTEA.or.Rgvrtp(i).eq.PRGTLD.or.
     &    Rgvrtp(i).eq.PRGTTH)THEN
c-----------------------------------------------------------------------
c     Get holiday window length
c-----------------------------------------------------------------------
        CALL getstr(Colttl,Colptr,Ncoltl,i,icoltl,nchr)
        IF(Lfatal)RETURN
        ipos=index(icoltl(1:nchr),'[')+1
        ndays=ctoi(icoltl(1:nchr),ipos)
        IF(Rgvrtp(i).eq.PRGTEA)
     &     CALL estrmu(Begdat,Nrxy,Sp,ndays,Xelong,xemean,.false.)
c-----------------------------------------------------------------------
c     Set index for holiday means 
c-----------------------------------------------------------------------
        mnindx=25-ndays+1
        IF(Rgvrtp(i).eq.PRGTTH)THEN
         mnindx=17+ndays
         IF(ndays.lt.0)mnindx=mnindx+1
        END IF
c-----------------------------------------------------------------------
c     For each observation, compute K
c-----------------------------------------------------------------------
        CALL addate(Begdat,Sp,-1,predat)
        DO irow=1,Nrxy
         CALL addate(predat,Sp,irow,idate)
         Kvec(irow)=1D0
         ir2=irow+Xdev-1
         m=idate(MO)
         IF(Rgvrtp(i).eq.PRGTEA.and.Sp.eq.4)THEN
c-----------------------------------------------------------------------
c     Compute K for special case of Quarterly easter adjustment
c-----------------------------------------------------------------------
          IF(m.le.2)Kvec(irow)=(B(i)*xemean(m))*(Daybar/Xnstar(ir2))+
     &                          Kvec(irow)
c-----------------------------------------------------------------------
         ELSE 
          IF(Rgvrtp(i).eq.PRGTEA.and.m.ge.2.and.m.le.4)THEN
           Kvec(irow)=(B(i)*xemean(m))*(Daybar/Xnstar(ir2))+Kvec(irow)
          ELSE IF(Rgvrtp(i).eq.PRGTLD.and.(m.eq.8.or.m.eq.9))THEN
           Kvec(irow)=(B(i)*lmeans(mnindx,m))*(Daybar/Xnstar(ir2))+
     &                Kvec(irow)
          ELSE IF(Rgvrtp(i).eq.PRGTTH.and.(m.eq.11.or.m.eq.12))THEN
           Kvec(irow)=(B(i)*tmeans(mnindx,m))*(Daybar/Xnstar(ir2))+
     &                Kvec(irow)
          END IF
         END IF
        END DO
       END IF
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
