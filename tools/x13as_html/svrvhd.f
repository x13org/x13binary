      SUBROUTINE svrvhd(Endall,Ny,Irevsa)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     If summary output produced, save relevant information to file.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'rev.prm'
      INCLUDE 'rev.cmn'
      INCLUDE 'revtrg.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'mq3.cmn'
c-----------------------------------------------------------------------
      INTEGER MO,YR
      PARAMETER(MO=2,YR=1)
c-----------------------------------------------------------------------
      INTEGER Endall,i,j,Ny,Irevsa
      DIMENSION Endall(2)
C-----------------------------------------------------------------------
      INTEGER nblank
      EXTERNAL nblank
c-----------------------------------------------------------------------
      CHARACTER num*(2)
      DIMENSION num(4)
      DATA num/'st','nd','rd','th'/
c-----------------------------------------------------------------------
      IF(.not.(Lrvsa.or.Lrvsf.or.Lrvch.or.Lrvtrn.or.Lrvtch.or.Lrvaic.or.
     &   Lrvfct))RETURN
c-----------------------------------------------------------------------
      WRITE(Nform,2000)'yes'
      IF(Irevsa.gt.0)THEN
       WRITE(Nform,2002)'yes'
      ELSE IF(Irevsa.eq.0)THEN
       WRITE(Nform,2002)'no'
      ELSE
       WRITE(Nform,2002)'failed'
      END IF
      IF(Lrvfct.and.Nfctlg.gt.0)WRITE(Nform,2001)'nfctlag: ',Nfctlg
      IF(Ntarsa.gt.0)THEN
       WRITE(Nform,2001)'nsalag: ',Ntarsa
       WRITE(Nform,2001)'nsalags: ',(Targsa(i),i=1,Ntarsa)
      ELSE IF(Lrvsa.or.Lrvch)THEN
       WRITE(Nform,2001)'nsalag: ',0
      END IF
      IF(Ntartr.gt.0)THEN
       WRITE(Nform,2001)'ntrnlag: ',Ntartr
       WRITE(Nform,2001)'ntrnlags: ',(Targtr(i),i=1,Ntartr)
      ELSE IF(Lrvtrn.or.Lrvtch)THEN
       WRITE(Nform,2001)'ntrnlag: ',0
      END IF
 2000 FORMAT('history: ',a)
 2001 FORMAT(a,10i3)
 2002 FORMAT('historysa: ',a)
c-----------------------------------------------------------------------
c     Save the starting and ending date of revisions
c-----------------------------------------------------------------------
      i=Rvstrt(MO)
      IF(i.gt.4)i=4
      j=endall(MO)
      IF(j.gt.4)j=4
      IF(Ny.eq.12.or.Ny.eq.4)THEN
       WRITE(Nform,1000)'revspan: ',Rvstrt(MO),num(i),
     &                  Moqu(1:nblank(Moqu)),Rvstrt(YR),endall(MO),
     &                  num(j),Moqu(1:nblank(Moqu)),endall(YR)
      ELSE IF(Ny.eq.1)THEN
       WRITE(Nform,1001)'revspan: ',Rvstrt(YR),endall(YR)
      ELSE
       WRITE(Nform,1000)'revspan: ',Rvstrt(MO),num(i),'period',
     &                   Rvstrt(YR),endall(MO),num(j),'period',
     &                   endall(YR)
      END IF
c-----------------------------------------------------------------------
      IF(Lrvsa.or.Lrvsf.or.Lrvch.or.Lrvtrn.or.Lrvtch)THEN
       IF(Cnctar)THEN
        WRITE(Nform,1002)'concurrent'
       ELSE
        WRITE(Nform,1002)'final'
       END IF
      END IF
c-----------------------------------------------------------------------
 1000 FORMAT(a,i2,a2,1x,a,',',i4,' to ',i2,a2,1x,a,',',i4)
 1001 FORMAT(a,i4,' to ',i4)
 1002 FORMAT('historytarget: ',a)
c-----------------------------------------------------------------------
      RETURN
      END

