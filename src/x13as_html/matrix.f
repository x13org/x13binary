      subroutine setPat(avalue)
        character avalue
        include 'matrix1.i'
        Pat=avalue
        return
      end
      subroutine setTmcs(avalue)
        character avalue
        include 'matrix1.i'
        Tmcs=avalue
*        if ((value .eq. 'Y') .or. (value .eq. 'y')) then
*          call usrentry(1.0d0,1,1,1,1,1020)
*        else                          
*          call usrentry(0.0d0,1,1,1,1,1020)
*        end if
        return
      end
      subroutine setAna(avalue)
        character avalue
        include 'matrix1.i'
        Ana=avalue
        return
      end
      subroutine setNmmu(avalue)
        Integer avalue
        include 'matrix1.i'
        Nmmu=avalue
        return
      end
      subroutine setNmp(avalue)
        Integer avalue
        include 'matrix1.i'
        Nmp=avalue
        return
      end
      subroutine setNmd(avalue)
        Integer avalue
        include 'matrix1.i'
        Nmd=avalue
        return
      end
      subroutine setNmq(avalue)
        Integer avalue
        include 'matrix1.i'
        Nmq=avalue
        return
      end
      subroutine setNmBp(avalue)
        Integer avalue
        include 'matrix1.i'
        NmBp=avalue
        return
      end
      subroutine setNmBd(avalue)
        Integer avalue
        include 'matrix1.i'
        NmBd=avalue
        return
      end
      subroutine setNmBq(avalue)
        Integer avalue
        include 'matrix1.i'
        NmBq=avalue
        return
      end
      subroutine setSf(avalue)
        character avalue
        include 'matrix1.i'
        Sf=avalue
*        if ((value .eq. 'E').or.(value .eq. 'e')) then
*          call usrentry(1.0d0,1,1,1,1,1021)
*        else                          
*          call usrentry(0.0d0,1,1,1,1,1021)
*        end if
        return
      end
      subroutine setCvar(avalue)
        character avalue
        include 'matrix1.i'
        Cvar=avalue
*        if ((value .eq. 'E').or.(value .eq. 'e')) then
*          call usrentry(1.0d0,1,1,1,1,1022)
*        else                          
*          call usrentry(0.0d0,1,1,1,1,1022)
*        end if
        return
      end
      subroutine setCcc(avalue)
        character avalue
        include 'matrix1.i'
        Ccc=avalue
*        if ((value .eq. 'E').or.(value .eq. 'e')) then
*          call usrentry(1.0d0,1,1,1,1,1039)
*        else                          
*          call usrentry(0.0d0,1,1,1,1,1039)
*        end if
        return
      end
      subroutine setCmtTc(avalue)
        character avalue
        include 'matrix1.i'
        CmtTc=avalue
        return
      end
      subroutine setCmtS(avalue)
        character avalue
        include 'matrix1.i'
        CmtS=avalue
        return
      end
      subroutine setCmtIR(avalue)
        character avalue
        include 'matrix1.i'
        CmtIR=avalue
        return
      end
      subroutine setCmtTs(avalue)
        character avalue
        include 'matrix1.i'
        CmtTs=avalue
        return
      end
      subroutine setCmtSA(avalue)
        character avalue
        include 'matrix1.i'
        CmtSA=avalue
        return
      end
      subroutine setSd(avalue)
        real*8 avalue
        include 'matrix1.i'
        Sd=avalue
        return
      end


      character Function getPat()
        include 'matrix1.i'
        getPat=Pat
        return
      end
      character Function getTmcs()
        include 'matrix1.i'
        getTmcs=Tmcs
        return
      end
      character Function getAna()
        include 'matrix1.i'
        getAna=Ana
        return
      end
      Integer Function getNmmu()
        include 'matrix1.i'
        getNmmu=Nmmu
        return
      end
      Integer Function getNmp()
        include 'matrix1.i'
        getNmp=Nmp
        return
      end
      Integer Function getNmd()
        include 'matrix1.i'
        getNmd=Nmd
        return
      end
      Integer Function getNmq()
        include 'matrix1.i'
        getNmq=Nmq
        return
      end
      Integer Function getNmBp()
        include 'matrix1.i'
         getNmBp=NmBp
        return
      end
      Integer Function getNmBd()
        include 'matrix1.i'
         getNmBd=NmBd
        return
      end
      Integer Function getNmBq()
        include 'matrix1.i'
         getNmBq=NmBq
        return
      end
      character Function getSf()
        include 'matrix1.i'
         getSf=Sf
        return
      end
      character Function getCvar()
        include 'matrix1.i'
         getCvar=Cvar
        return
      end
      character Function getCcc()
        include 'matrix1.i'
         getCcc=Ccc
        return
      end
      character Function getCmtTc()
        include 'matrix1.i'
         getCmtTc=CmtTc
        return
      end
      character Function getCmtS()
        include 'matrix1.i'
         getCmtS=CmtS
        return
      end
      character Function getCmtIR()
        include 'matrix1.i'
         getCmtIR=CmtIR
        return
      end
      character Function getCmtTs()
        include 'matrix1.i'
         getCmtTs=CmtTs
        return
      end
      character Function getCmtSA()
        include 'matrix1.i'
         getCmtSA=CmtSA
        return
      end
      real*8 Function getSd()
        include 'matrix1.i'
        getSd=Sd
        return
      end
      subroutine Mtx1Reset()
        include 'matrix1.i'
        Pat='N'
        Tmcs='N'
        Ana='N'
        Nmmu=0
        Nmp=0
        Nmd=0
        Nmq=0
        Nmbp=0
        Nmbd=0
        Nmbq=0
        Sf='0'
        Cvar='0'
        Ccc='0'
        CmtTc='N'
        CmtS='N'
        CmtIR='N'
        CmtTs='N'
        CmtSA='N'
        Sd=0.0d0
        return
      end
C
C Matrix2
C
      subroutine setSdt(avalue)
      include 'matrix2.i'
      real*8 avalue
       Sdt=avalue
       return
      end
      real*8 function getSdt()
      include 'matrix2.i'
       getSdt=Sdt
       return
      end
      subroutine setSds(avalue)
      include 'matrix2.i'
      real*8 avalue
       Sds=avalue
       return
      end
      real*8 function getSds()
      include 'matrix2.i'
       getSds=Sds
       return
      end
      subroutine setSdc(avalue)
      include 'matrix2.i'
      real*8 avalue
       Sdc=avalue
       return
      end
      real*8 function getSdc()
      include 'matrix2.i'
       getSdc=Sdc
       return
      end
      subroutine setSdi(avalue)
      include 'matrix2.i'
      real*8 avalue
       Sdi=avalue
       return
      end
      real*8 function getSdi()
      include 'matrix2.i'
       getSdi=Sdi
       return
      end
      subroutine setSdsa(avalue)
      include 'matrix2.i'
      real*8 avalue
       Sdsa=avalue
       return
      end
      real*8 function getSdsa()
      include 'matrix2.i'
       getSdsa=Sdsa
       return
      end
      subroutine setSeCect(avalue)
      include 'matrix2.i'
      real*8 avalue
       SeCect=avalue
       return
      end
      real*8 function getSeCect()
      include 'matrix2.i'
       getSeCect=SeCect
       return
      end
      subroutine setSeCecSa(avalue)
      include 'matrix2.i'
      real*8 avalue
       SeCecSa=avalue
       return
      end
      real*8 function getSeCecSa()
      include 'matrix2.i'
       getSeCecSa=SeCecSa
       return
      end
      subroutine setRseCect(avalue)
      include 'matrix2.i'
      real*8 avalue
       RseCect=avalue
       return
      end
      real*8 function getRseCect()
      include 'matrix2.i'
       getRseCect=RseCect
       return
      end
      subroutine setRseCecSa(avalue)
      include 'matrix2.i'
      real*8 avalue
       RseCecSa=avalue
       return
      end
      real*8 function getRseCecSa()
      include 'matrix2.i'
       getRseCecSa=RseCecSa
       return
      end
      subroutine setCovt1(avalue)
      include 'matrix2.i'
      real*8 avalue
       Covt1=avalue
       return
      end
      real*8 function getCovt1()
      include 'matrix2.i'
       getCovt1=Covt1
       return
      end
      subroutine setCovsa1(avalue)
      include 'matrix2.i'
      real*8 avalue
       Covsa1=avalue
       return
      end
      real*8 function getCovsa1()
      include 'matrix2.i'
       getCovsa1=Covsa1
       return
      end
      subroutine setCovt5(avalue)
      include 'matrix2.i'
      real*8 avalue
       Covt5=avalue
       return
      end
      real*8 function getCovt5()
      include 'matrix2.i'
       getCovt5=Covt5
       return
      end
      subroutine setCovsa5(avalue)
      include 'matrix2.i'
      real*8 avalue
       Covsa5=avalue
       return
      end
      real*8 function getCovsa5()
      include 'matrix2.i'
       getCovsa5=Covsa5
       return
      end
      subroutine setSsh(avalue)
      include 'matrix2.i'
      integer avalue
       Ssh=avalue
       return
      end
      integer function getSsh()
      include 'matrix2.i'
       getSsh=Ssh
       return
      end
C   LINES OF CODE COMMENTED FOR X-13A-S : 1      
C      subroutine setSSp(avalue)
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 1
      subroutine setSSp2(avalue)
C   END OF CODE BLOCK
      include 'matrix2.i'
      integer avalue
       SSp=avalue
       return
      end
C   LINES OF CODE COMMENTED FOR X-13A-S : 3
C      integer function getSSp()
C      include 'matrix2.i'
C       getSSp=SSp
C   END OF CODE BLOCK
C   LINES OF CODE ADDED FOR X-13A-S : 3
      integer function getSSp2()
      include 'matrix2.i'
       getSSp2=SSp
C   END OF CODE BLOCK
       return
      end
      subroutine setSSf(avalue)
      include 'matrix2.i'
      integer avalue
       SSf=avalue
       return
      end
      integer function getSSf()
      include 'matrix2.i'
       getSSf=SSf
       return
      end
      subroutine setT11t(avalue)
      include 'matrix2.i'
      real*8 avalue
       T11t=avalue
       return
      end
      real*8 function getT11t()
      include 'matrix2.i'
       getT11t=T11t
       return
      end
      subroutine setT11sa(avalue)
      include 'matrix2.i'
      real*8 avalue
       T11sa=avalue
       return
      end
      real*8 function getT11sa()
      include 'matrix2.i'
       getT11sa=T11sa
       return
      end
      subroutine setT112t(avalue)
      include 'matrix2.i'
      real*8 avalue
       T112t=avalue
       return
      end
      real*8 function getT112t()
      include 'matrix2.i'
       getT112t=T112t
       return
      end
      subroutine setT112sa(avalue)
      include 'matrix2.i'
      real*8 avalue
       T112sa=avalue
       return
      end
      real*8 function getT112sa()
      include 'matrix2.i'
       getT112sa=T112sa
       return
      end
      subroutine setT112x(avalue)
      include 'matrix2.i'
      real*8 avalue
       T112x=avalue
       return
      end
      real*8 function getT112x()
      include 'matrix2.i'
       getT112x=T112x
       return
      end
      subroutine setDaat(avalue)
      include 'matrix2.i'
      real*8 avalue
       Daat=avalue
       return
      end
      real*8 function getDaat()
      include 'matrix2.i'
       getDaat=Daat
       return
      end
      subroutine setDaasa(avalue)
      include 'matrix2.i'
      real*8 avalue
       Daasa=avalue
*       call usrentry(daasa,1,1,1,1,1038)
       return
      end
      real*8 function getDaasa()
      include 'matrix2.i'
       getDaasa=Daasa
       return
      end
      subroutine Mtx2Reset()
      include 'matrix2.i'
       Sdt = 0.0d0
       Sds = 0.0d0
       Sdc = 0.0d0
       Sdi = 0.0d0
       Sdsa = 0.0d0
       SeCect = 0.0d0
       SeCecSa = 0.0d0
       RseCect = 0.0d0
       RseCecSa = 0.0d0
       Covt1 = 0.0d0
       Covsa1 = 100.0d0
       Covt5 = 0.0d0
       Covsa5 = 100.0d0
       Ssh = 0.0d0
       SSp = 0.0d0
       SSf = 0.0d0
       T11t = 0.0d0
       T11sa = 0.0d0
       T112t = 0.0d0
       T112sa = 0.0d0
       T112x = 0.0d0
       Daat = 0.0d0
       Daasa = 0.0d0
       return
      end
c
      subroutine inicSumS()
      include 'sums.i'
       tTMCS=0
       tANA=0
       tScomp=0
       tCycComp=0
       tStocTD=0
       tSpecFac=0
       tACF=0
       tCCF=0
       tUnstSa=0
       tUnrSa=0
       tRevSa=0
       tSeasNoSig=0
       tBias=0
       tCrQs=0
       tCrSNP=0
       tCrPeaks=0
       tX11=0
       tSeats=0
       tNSA=0
      return
      end 
c
      subroutine addToSumS(mq,IsCloseToTD,crQs,crSNP,crPeaks,IsPureMA)
      include 'sums.i'
      character wStr
c
      real*8 getSdS,getSdc,getSdSa,getSeCecSa,getRSeCecSa,getSd,
     &       getDaaSa,dvec(1)
      character gettmcs,getANA,getSf,getCvar,getCcc
      integer getssh,crQs,crSNP,crPeaks,mq,getESS
      logical IsCloseToTD,IsPureMA
      EXTERNAL getSdS,getSdc,getSdSa,getSeCecSa,getRSeCecSa,getSd,
     &         getDaaSa,gettmcs,getANA,getSf,getCvar,getCcc,getssh,
     &         getESS
c
      wStr=getTmcs()
      if ((wStr.eq.'Y') .or.(wStr.eq.'y')) then
       tTMCS=tTMCS+1
      end if
      if ((getANA().eq.'Y').or.(getANA().eq.'y')) then
       tANA=tANA+1
      end if
      if (isPureMA .eqv. .false.) then
       tmpreal=0.0d0
       if (getSdS().gt.0) then
        tScomp=tScomp+1
        if (getSdS().gt.(0.75d0*getSd())) then
         tUnstSa=tUnstSa+1
         tmpreal=1.0d0
        end if
        if (getSeCecSa().gt.(0.95d0*getSd())) then
         tUnrSa=tUnrSa+1
        end if
        dvec(1)=tmpreal
        call usrentry(dvec,1,1,1,1,1601)
        tmpreal=0.0d0 
        if (getRSeCecSa().gt.(0.80d0*getSd())) then
         tRevSa=tRevSa+1
         tmpreal=1.0d0   
        end if
        dvec(1)=tmpreal
        call usrentry(dvec,1,1,1,1,1603)
        if (getESS().lt.1) then
c      if (getSSh().lt.2) then
         tSeasNosig=tSeasNosig+1
        end if
       end if
       if (.not.IsCloseToTD) then
        if (getSdc().gt.0) then
         tCycComp=tCycComp+1
        end if
       else
        if (getSdc().gt.0) then
         tStocTD=tStocTD+1
        end if
       end if 
       if ((getSf().eq.'E').or.(getSf().eq.'e')) then
        tSpecFac=tSpecFac+1
       end if
       if ((getCvar().eq.'E').or.(getCvar().eq.'e')) then
        tACF=tACF+1
       end if
       if ((getCcc().eq.'E').or.(getCcc().eq.'e')) then
        tCCF=tCCF+1
       end if
       if (getDaaSa().gt.1.0d0) then
        tBias=tBias+1
       end if
       if ((mq.eq.4).or.(mq.eq.12)) then
        if (tCrQs.ne.-1) then
         tCrQs=tCrQs+CrQs
        end if
        if (tCrSNP.ne.-1) then
         tCrSNP=tCrSNP+tCrSNP
        end if
        if (tCrPeaks.ne.-1) then 
         tCrPeaks=tCrPeaks+tCrPeaks
        end if
       else
        tCrQs=-1
        tCrSNP=-1
        tCrPeaks=-1
       end if
      end if
      return
      end 
      subroutine setESS(nTh95,nTh3,MQ)
      integer nTh95,nTh3,MQ
      include 'matrix2.i'
       if (nTh3.ge.1 .or. nTh95.ge.2 .or. (MQ.lt.12.and.nTh95.ge.1))then
        ESS=1
       else
        ESS=0
       endif
       return
      end
      integer function getESS()
      include 'matrix2.i'
       getESS=ESS
       return
      end
