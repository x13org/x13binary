C     Last change:  BCM  12 Mar 98    9:56 am
      SUBROUTINE prtadj(Sprior,Pos1,Pos2,Nspobs,Lgraf)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     This subroutine prints out all the prior factors, and generates
c     the original series adjusted for the permanent prior factors.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'priadj.cmn'
      INCLUDE 'priusr.cmn'
      INCLUDE 'mdltbl.i'
      INCLUDE 'tbllog.prm'
      INCLUDE 'tbllog.cmn'
      INCLUDE 'error.cmn'
      INCLUDE 'lzero.cmn'
      INCLUDE 'orisrs.cmn'
c-----------------------------------------------------------------------
      LOGICAL F
      PARAMETER(F=.false.)
c-----------------------------------------------------------------------
      LOGICAL Lgraf
      INTEGER Pos1,Pos2,Nspobs
      DOUBLE PRECISION Sprior,dvec
      DIMENSION Sprior(PLEN),dvec(PLEN)
c-----------------------------------------------------------------------
c     Print out the combined prior factors
c-----------------------------------------------------------------------
      IF(Prttab(LTRNPA))THEN
       IF(Nustad.gt.0.and.Nuspad.gt.0)THEN
        CALL table(Sprior,Pos1,Pos2,2,1,1,dvec,LTRNPA)
       ELSE IF(Nustad.gt.0)THEN
        CALL table(Sprior,Pos1,Pos2,2,2,1,dvec,LTRNPA)
       ELSE
        CALL table(Sprior,Pos1,Pos2,2,3,1,dvec,LTRNPA)
       END IF
      END IF
      IF(.not.Lfatal.and.Savtab(LTRNPA))
     &   CALL punch(Sprior,Pos1,Pos2,LTRNPA,F,F)
      IF(.not.Lfatal.and.Lgraf)
     &   CALL punch(Sprior,Pos1,Pos2,LTRNPA,Lgraf,F)
      IF(Lfatal)RETURN
c-----------------------------------------------------------------------
c     Print out the temporary prior factors
c-----------------------------------------------------------------------
      IF(Nustad.gt.0)THEN
       CALL copy(Usrtad(Frstat+Lsp-1),Nspobs,1,dvec(Pos1))
       CALL divsub(Stoap,Sto,dvec,Pos1,Pos2)
       IF(Prttab(LTRA2T))
     &    CALL table(dvec,Pos1,Pos2,2,2,1,dvec,LTRA2T)
       IF(.not.Lfatal.and.Savtab(LTRA2T))
     &    CALL punch(dvec,Pos1,Pos2,LTRA2T,F,F)
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(dvec,Pos1,Pos2,LTRA2T,Lgraf,F)
       IF(Lfatal)RETURN
      END IF
c-----------------------------------------------------------------------
c     Print out the Permanent prior factors
c-----------------------------------------------------------------------
      IF(Nuspad.gt.0)THEN
       CALL copy(Usrpad(Frstap+Lsp-1),Nspobs,1,dvec(Pos1))
       CALL divsub(Stopp,Sto,dvec,Pos1,Pos2)
       CALL divsub(Stoap,Stoap,dvec,Pos1,Pos2)
       IF(Prttab(LTRA2P))
     &    CALL table(dvec,Pos1,Pos2,2,3,1,dvec,LTRA2P)
       IF(.not.Lfatal.and.Savtab(LTRA2P))
     &    CALL punch(dvec,Pos1,Pos2,LTRA2P,F,F) 
       IF(.not.Lfatal.and.Lgraf)
     &    CALL punch(dvec,Pos1,Pos2,LTRA2P,Lgraf,F)
      END IF
c-----------------------------------------------------------------------
      RETURN
      END
