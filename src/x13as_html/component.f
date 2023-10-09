c           File Component.F
c
c      subroutine AddComp
c      Add the component PHIc(1:nPHIc)Ct=THc(1:nTHc)Act Act~niid(0,Vc) 
c      To the list of components save in {ARs(1:nComp,*),MAs(1:nComp,*),Vs(1:nComp)}
c         with dimensions {ARsDim(1:nComp),MAsDim(1:nComp)}
c      So the k component of the list will be:
c           ARs(k,1:ARsDim(k)) Ckt=MAs(k,1:MAsDim(k)))Akt     Akt~niid(0,Vs(k))
c
      subroutine AddComp(PHIc,nPHIc,THc,nTHc,Vc,
     $                ARs,ARsDim,MAs,MAsDim,Vs,NComp)
	implicit none
	include 'component.i'
c     INPUT
      real*8 Vc,PHIc(*),THc(*)
	integer nPHIc,nTHc
c     INPUT/OUTPUT
      real*8 ARs(MaxComp,MaxCompDim),MAs(MaxComp,MaxCompDim),
     $      Vs(MaxComp)
	integer ARsDim(MaxComp),MAsDim(MaxComp),nComp
c     Local variables
      integer i
c
      if (nComp.ge.MaxComp) then
	  call ABORTA('nComp pass the MaxComp components')
	end if
	if (Vc.le.0.0d0) return
	nComp=nComp+1
	Do i=1,nPHIc
	  ARs(nComp,i)=PHIc(i)
	endDo
	ARsDim(nComp)=nPHIc
	Do i=1,nTHc
	  MAs(nComp,i)=THc(i)
	endDo
	MAsDim(nComp)=nTHc
	Vs(nComp)=Vc
	end
c
c
c     subroutine CopyAddComp
c     given a groups of arrays {ARs1(1:nComp,*),ARsDim1(1:nComp),MAs1(1:nComp,*),MAsDim1(1:nComp),Vs1(1:nComp)}
c      that keep a group of components add them to another group of components
      subroutine CopyAddComp(ARs1,ARsDim1,MAs1,MAsDim1,Vs1,nComp1,
     $                  ARs,ARsDim,MAs,MAsDim,Vs,nComp)
      implicit NONE
	include 'component.i'
c     INPUT
      real*8 ARs1(MaxComp,MaxCompDim),MAs1(MaxComp,MaxCompDim),
     $        Vs1(MaxComp)
	integer ARsDim1(MaxComp),MAsDim1(MaxComp),nComp1
c     INPUT/OUTPUT
      real*8 ARs(MaxComp,MaxCompDim),MAs(MAxComp,MaxCompDim),
     $        Vs(MaxComp)
	integer ARsDim(MaxComp),MAsDim(MaxComp),nComp
c     Local Variables
      integer i,j
c
      j=0
      Do while (j.lt.nComp1)
	  j=j+1
	  if ((j+nComp).gt.MaxComp) then
	    j=j-1
	    EXIT
	  end if
	  Do i=1,ARsDim1(j)
	    ARs(nComp+j,i)=ARs1(j,i)
	  endDo
	  ARsDim(nComp+j)=ARsDim1(j)
	  Do i=1,MAsDim1(j)
	    MAs(nComp+j,i)=MAs1(j,i)
	  EndDo
	  MAsDim(nComp+j)=MAsDim1(j)
	  Vs(nComp+j)=Vs1(j)
	endDo
	nComp=nComp+j
      end
c
c     Subroutine GetComp
c    Given a group of component return the component equivalent to the addition of all of them
      subroutine GetComp(ARs,ARsDim,MAs,MAsDim,Vs,nComp,
     $               PHIcomp,nPHIcomp,THcomp,nTHcomp,Vcomp,toterr)
	implicit none
	include 'component.i'
c     INPUT
      real*8 ARs(MaxComp,MaxCompDim),MAs(MaxComp,MaxCompDim),
     $        Vs(MaxComp),toterr
	integer ARsDim(MaxComp),MAsDim(MaxComp),nComp
c     OUTPUT
      real*8 PHIcomp(MaxCompDim),THcomp(MaxCompDim),Vcomp
	integer nPHIcomp,nTHcomp
c     Local Parameters
      real*8 Utmp(50),Utmp2(50),V(50),
     $     ARMAtmp(MaxCompDim),ARMAtmp2(MaxCompDim),ARStmp(MaxCompDim)
      integer i,j,l,nARMAtmp,nARMAtmp2,nUtmp,nUtmp2,nV,k
	integer nounit
      character caption0*1,id0*1
c
      toterr=0.0d0
      if (nComp.eq.0) then
	  Vcomp=0.0D0
	  PHIcomp(1)=1.0D0
	  nPHIcomp=1
	  THcomp(1)=1.0D0
	  nTHcomp=1
	  return
	end if
      if (nComp.eq.1) then
	  do i=1,ARsDim(1)
	    PHIcomp(i)=ARs(1,i)
	  enddo
	  nPHIcomp=ARsDim(1)
	  do i=1,MAsDim(1)
	    THcomp(i)=MAs(1,i)
	  enddo
	  nTHcomp=MAsDim(1)
	  Vcomp=Vs(1)
	  return
	end if
      do i=1,50
	  Utmp(i)=0.0d0
	endDo
	nUtmp=0
	do j=1,nComp
	  do i=1,MAsDim(j)
	    ARMAtmp(i)=MAs(j,i)
	  enddo
	  nARMAtmp=MAsDim(j)
	  do i=1,nComp
	    if (i.ne.j) then
	      DO k=1,ARsDim(i)
	        ARStmp(k)=ARs(i,k)
	      endDO
	      call CONV(ARMAtmp,nARMAtmp,ARstmp,ARsDim(i),
     $             ARMAtmp2,nARMAtmp2)
	      Do l=1,nARMAtmp2
	        ARMAtmp(l)=ARMAtmp2(l)
	      enddo
            nARMAtmp=nARMAtmp2
	    end if
	  enddo
	  call CONJ(ARMAtmp,nARMAtmp,ARMAtmp,nARMAtmp,Utmp2,nUtmp2)
	  nUtmp=max(nUtmp,nUtmp2)
	  do i=nUtmp2+1,nUtmp
	    Utmp2(i)=0.0d0
	  enddo
        do i=1,nUtmp
	    Utmp(i)=Utmp(i)+Utmp2(i)*Vs(j)
	  enddo
	enddo
	nounit=0
	Do while(abs(Utmp(nUtmp))<1.0D-20)
	  nUtmp=nUtmp-1;
	  if (nUtmp.eq.0) exit
	enddo
	if (nUtmp.gt.0)then
        caption0=' '
        id0=' '
        call MAK1(Utmp,nUtmp,THcomp,nTHcomp,Vcomp,nounit,
     $        1,caption0,0,toterr,id0,0)
	else
	  nTHcomp=0
	  Vcomp=0.0D0
	  THcomp(1)=1.0D0
	end if
c     Computing the total squared error
*      call CONJ(THcomp,nTHcomp,THcomp,nTHComp,V,nV)
*	Do i=1,nV
*	  toterr=toterr+(V(i)*Vcomp-Utmp(i))**2
*	enddo
c     End of computing the total squared error
	PHIcomp(1)=1.0d0
	nPHIcomp=1
	do j=1,nComp
	      DO k=1,ARsDim(j)
	        ARStmp(k)=ARs(j,k)
	      endDO
	  call CONV(PHIcomp,nPHIcomp,ARstmp,ARsDim(j),
     $          ARMAtmp2,nARMAtmp2)
	  do l=1,nARMAtmp2
	    PHIcomp(l)=ARMAtmp2(l)
	  enddo
	  nPHIcomp=nARMAtmp2
	enddo
      end
      