C     Last change:  BCM  29 Sep 1998   10:48 am
**==getrev.f    processed by SPAG 4.03F  at 10:39 on 20 Oct 1994
      SUBROUTINE getrev(Srs,Lstobs,Muladd,Itype,Ny,Iag,Iagr)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     This routine gets the concurrent and final values for the
c     seasonal factors and seasonally adjusted series to be used in the
c     revisions analysis.
c-----------------------------------------------------------------------
      LOGICAL F,T
      PARAMETER(F=.false.,T=.true.)
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'stdio.i'
      INCLUDE 'model.prm'
      INCLUDE 'rev.prm'
      INCLUDE 'rev.cmn'
      INCLUDE 'revtrg.cmn'
      INCLUDE 'revsrs.cmn'
      INCLUDE 'units.cmn'
c-----------------------------------------------------------------------
      INTEGER MONE
      PARAMETER(MONE=-1)
c-----------------------------------------------------------------------
      DOUBLE PRECISION Srs,tmp
      LOGICAL ltemp
      INTEGER i,i1,i2,Lstobs,Itype,Muladd,Ny,pptr,lli,rvd,Iag,Iagr
      DIMENSION Srs(*)
c-----------------------------------------------------------------------
c     Store concurrent value
c-----------------------------------------------------------------------
      ltemp=F
      rvd=Rvdiff
      IF(Itype.eq.0)THEN
       IF(Revptr.gt.0)CALL putrev(Srs,Cncsf(Revptr),tmp,tmp,Lstobs,
     &                            Lrvsf,ltemp,Muladd,Itype,rvd,Indrev)
c-----------------------------------------------------------------------
c     Store value of projected seasonal factors
c-----------------------------------------------------------------------
       IF(mod(Lstobs,Ny).eq.0)THEN
        DO i=1,Ny
         pptr=Revptr+i
         IF(pptr.gt.0)CALL putrev(Srs,Cncsfp(pptr),tmp,tmp,Lstobs+i,
     &                            Lrvsf,ltemp,Muladd,Itype,rvd,Indrev)
        END DO
       END IF
c-----------------------------------------------------------------------
c     Save concurrent seasonal adjustment and changes
c-----------------------------------------------------------------------
      ELSE IF(Revptr.gt.0)THEN
       IF(Itype.eq.1)THEN
        CALL putrev(Srs,Cncsa(Revptr),Cncch(Revptr),Cncisa(Revptr),
     &              Lstobs,Lrvsa,Lrvch,Muladd,Itype,rvd,Indrev)
c-----------------------------------------------------------------------
c     If alternate targets were also specified, store the final values 
c     for each target now, if available.
c-----------------------------------------------------------------------
        IF(Ntarsa.gt.0)THEN
         i=1
         DO WHILE(i.le.Ntarsa)
          IF(Revptr.gt.Targsa(i))THEN
           i1=Revptr-Targsa(i)
           i2=Lstobs-Revptr+i1
           CALL putrev(Srs,Finsa(i,i1),Finch(i,i1),Finisa(i,i1),i2,
     &                 Lrvsa,Lrvch,Muladd,Itype,rvd,Indrev)
           i=i+1
          ELSE
           i=Ntarsa+1
          END IF
         END DO
        END IF
        IF(rvd.lt.0)THEN
         CALL wWritln('Revisions history analysis of the percent '//
     &                'changes of the',STDERR,Mt2,T,F)
         CALL writln('         seasonally adjusted series has ceased '//
     &               'due to negative values',STDERR,Mt2,F,F)
         CALL writln('         in the seasonally adjusted series.',
     &               STDERR,Mt2,F,T)
        END IF
c-----------------------------------------------------------------------
c     Save concurrent trend and changes
c-----------------------------------------------------------------------
       ELSE
        CALL putrev(Srs,Cnctrn(Revptr),Cnctch(Revptr),tmp,Lstobs,Lrvtrn,
     &              Lrvtch,Muladd,Itype,rvd,Indrev)
c-----------------------------------------------------------------------
c     If alternate targets were also specified, store the final values 
c     for each target now, if available.
c-----------------------------------------------------------------------
        IF(Ntartr.gt.0)THEN
         i=1
         DO WHILE(i.le.Ntartr)
          IF(Revptr.gt.Targtr(i))THEN
           i1=Revptr-Targtr(i)
           i2=Lstobs-Revptr+i1
           CALL putrev(Srs,Fintrn(i,i1),Fintch(i,i1),tmp,i2,Lrvtrn,
     &                 Lrvtch,Muladd,Itype,rvd,Indrev)
           i=i+1
          ELSE
           i=Ntartr+1
          END IF
         END DO
        END IF
        IF(rvd.lt.0)THEN
         CALL wWritln('Revisions history analysis of the percent '//
     &                'changes of the',STDERR,Mt2,T,F)
         CALL writln('         trend has ceased due to negative '//
     &               'values in the trend.',STDERR,Mt2,F,T)
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
c     If this is the original seasonal adjustment, store the final
c     values now.
c-----------------------------------------------------------------------
      IF(Revptr.lt.(Endrev-Begrev+1).or.rvd.lt.0)RETURN
      IF(Itype.eq.1.and.Iagr.eq.2.and.Iag.ge.0)Nrcomp=Nrcomp+1
      DO i=1,Revptr
       lli=Lstobs-Revptr+i
       IF(Itype.eq.0)THEN
        CALL putrev(Srs,Finsf(i),tmp,tmp,lli,Lrvsf,ltemp,Muladd,Itype,
     &              rvd,Indrev)
       ELSE IF(Itype.eq.1)THEN
        CALL putrev(Srs,Finsa(0,i),Finch(0,i),Finisa(0,i),lli,Lrvsa,
     &              Lrvch,Muladd,Itype,rvd,Indrev)
        IF(rvd.eq.MONE)THEN
         CALL wWritln('Revisions history analysis of the percent '//
     &                'changes of the',STDERR,Mt2,T,F)
         CALL writln('         seasonally adjusted series has ceased '//
     &               'due to negative values',STDERR,Mt2,F,F)
         CALL writln('         in the seasonally adjusted series.',
     &               STDERR,Mt2,F,T)
         rvd=rvd-1
        END IF
       ELSE
        CALL putrev(Srs,Fintrn(0,i),Fintch(0,i),tmp,lli,Lrvtrn,
     &              Lrvtch,Muladd,Itype,rvd,Indrev)
        IF(rvd.eq.MONE)THEN
         CALL wWritln('Revisions history analysis of the percent '//
     &                'changes of the',STDERR,Mt2,T,F)
         CALL writln('         trend has ceased due to negative '//
     &               'values in the trend.',STDERR,Mt2,F,T)
         rvd=rvd-1
        END IF
       END IF
      END DO
c-----------------------------------------------------------------------
      RETURN
      END
