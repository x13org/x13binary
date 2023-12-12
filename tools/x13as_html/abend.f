**==abend.f    processed by SPAG 4.03F  at 09:46 on  1 Mar 1994
      SUBROUTINE abend
      IMPLICIT NONE
c-----------------------------------------------------------------------
c abend - abnormal termination of program, Lahey pc version
c Does a trace of the line number and routines the programs stopped in,
c then the program stops.
c-----------------------------------------------------------------------
c   Author     - Larry Bobbitt
c                Statistical Research Division
c                U.S. Census Bureau
c                Room 3000-4
c                Washington, D.C.    20233
c                (301) 763-3957
c-----------------------------------------------------------------------
c   Revised by Brian C. Monsell for X-12, X-13ARIMA-SEATS
c-----------------------------------------------------------------------
c     INCLUDE 'stdio.i'
c-----------------------------------------------------------------------
c     call fclose(IALL)
c     call abort()
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'error.cmn'
      INCLUDE 'units.cmn'
      INCLUDE 'hiddn.cmn'
      INCLUDE 'svllog.prm'
      INCLUDE 'svllog.cmn'
      INCLUDE 'dgnsvl.i'
c-----------------------------------------------------------------------
      LOGICAL istrue
      EXTERNAL istrue
c-----------------------------------------------------------------------
c   if running a sliding spans or revisions history analysis,
c   generate diagnostic or savelog output indicating the analysis
c   has failed (BCM - March 2005)
c-----------------------------------------------------------------------
      IF(Issap.eq.2.and.(Lsumm.gt.0.or.Svltab(LSLPCT)))THEN
       IF(Lsumm.gt.0)WRITE(Nform,1025)'failed'
       IF(Svltab(LSLPCT))
     &    CALL mkPOneLine(Ng,'center',
     &              'Sliding spans analysis failed : check error file.')
      ELSE IF(Irev.eq.4.and.(istrue(Svltab,LSLASA,LSLASP).or.
     &        Lsumm.gt.0))THEN
       IF(Lsumm.gt.0)THEN
        WRITE(Nform,1015)'failed'
        IF(Irevsa.gt.0)WRITE(Nform,1005)'failed'
       END IF
       IF(istrue(Svltab,LSLASA,LSLASP))
     &    CALL mkPOneLine(Ng,'center',
     &               'History analysis failed : check error file.')
      END IF
      IF(Lsumm.gt.0.and.Opnudg)WRITE(Nform,1055)
c-----------------------------------------------------------------------
*      CALL fstop()
      Lfatal=.true.
c-----------------------------------------------------------------------
 1005 FORMAT('historysa: ',a)
 1015 FORMAT('history: ',a)
 1025 FORMAT('sspans: ',a)
 1055 FORMAT('errorstop: yes')
      RETURN
c-----------------------------------------------------------------------
      END
