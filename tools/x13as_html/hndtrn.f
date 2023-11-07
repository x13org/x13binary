C     Last change:  SRD  25 Jan 100    2:14 pm
**==hndtrn.f    processed by SPAG 4.03F  at 15:38 on 18 May 1994
      SUBROUTINE hndtrn(Stc,Stci,Lfda,Lldaf,Nterm,Tic,Lend,Lsame)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     This routine applys a henderson filter of length Nterm to a
c     series (Stci), returning the trend (Stc).
c-----------------------------------------------------------------------
      INCLUDE 'hender.prm'
      INCLUDE 'x11msc.cmn'
c-----------------------------------------------------------------------
      LOGICAL Lend,Lsame
      DOUBLE PRECISION apply,Stc,Stci,W,Tic,rbeta
      INTEGER Lfda,Lldaf,Nterm,i,ib,ie
      DIMENSION Stc(*),Stci(*),W(PMXHN2)
c-----------------------------------------------------------------------
      DOUBLE PRECISION PI
      PARAMETER(PI=3.14159265358979d0)
c-----------------------------------------------------------------------
      IF(.not.Lsame)THEN
c-----------------------------------------------------------------------
c     Apply symmetric henderson filter to series
c-----------------------------------------------------------------------
       CALL hender(W,Nterm)
       ib=Lfda+Nterm/2
       ie=Lldaf-Nterm/2
       DO i=ib,ie
        Stc(i)=apply(Stci,i,W,Nterm)
       END DO
c-----------------------------------------------------------------------
c     If end filters not applied, exit
c-----------------------------------------------------------------------
       IF(.not.Lend)RETURN
      END IF
      i=Nterm
      DO WHILE (i.eq.7.and.(.not.Tru7hn))
c-----------------------------------------------------------------------
C --- REDUCE SPAN OF HENDERSON AT THE ENDS OF THE SERIES FOR 7 TERM.
c-----------------------------------------------------------------------
       i=i-2
       CALL hender(W,i)
       ib=ib-1
       ie=ie+1
       Stc(ib)=apply(Stci,ib,W,i)
       Stc(ie)=apply(Stci,ie,W,i)
       Tic=0.001D0
      END DO
c-----------------------------------------------------------------------
c     Set value of rbeta depending on what the length of the trend
c     filter is.
c-----------------------------------------------------------------------
      rbeta=4/(Tic*Tic*PI)
      CALL ends(Stc,Stci,Lfda,Lldaf,i,rbeta)
      RETURN
      END
