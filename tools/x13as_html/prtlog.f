C     Last change:  BCM  26 Jan 98    1:50 pm
      SUBROUTINE prtlog(Ng,Insrs,Outsrs,Nopen,Unopnd,Nfail,Failed,
     &                  Mtafil,Nmfil,Samepth)
      IMPLICIT NONE
C-----------------------------------------------------------------------
c     Print out summary error messages into log file.
C-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
      INCLUDE 'htmlout.cmn'
C-----------------------------------------------------------------------
      LOGICAL T,F
      PARAMETER(T=.true.,F=.false.)
C-----------------------------------------------------------------------
      LOGICAL lhdr,Samepth,Lexist
      CHARACTER Insrs*(PFILCR),Outsrs*(PFILCR),Mtafil*(PFILCR)
      INTEGER i,n1,n2,Ng,Nopen,Unopnd,Nfail,Failed,Nmfil
      DIMENSION Insrs(*),Outsrs(*),Unopnd(*),Failed(*)
C-----------------------------------------------------------------------
      INTEGER nblank
      EXTERNAL nblank
c-----------------------------------------------------------------------
      IF(Nopen.gt.0.or.Nfail.gt.0)THEN
       CALL writTagOneLine(Ng,'h2','@',
     &                     'Errors related to input files in '//
     &                     Mtafil(1:Nmfil))
       WRITE(STDERR,1020)Logfil(1:Nlgfil),Mtafil(1:Nmfil)
 1020  FORMAT(//,'   Check ',a,' to see which input files defined ',
     &        'in ',a,/,'   were terminated due to errors.')
      END IF
C-----------------------------------------------------------------------
      IF(Nfail.gt.0)THEN
       lhdr=T
C-----------------------------------------------------------------------
       DO i=1,Nfail
        n1=nblank(Insrs(Failed(i)))
        n2=nblank(Outsrs(Failed(i)))
        IF(n1.gt.0.and.n2.gt.0)THEN
         IF(lhdr)THEN
          CALL mkPOneLine(Ng,'@','Input or runtime errors were found '//
     &                           'in the following files:')
          CALL writTagClass(Ng,'ul','indent')
          lhdr=F
         END IF
         INQUIRE(FILE=Outsrs(Failed(i))(1:n2)//'_err.html',
     &           EXIST=Lexist)
         IF(Samepth)THEN
          IF(Lexist)THEN
           WRITE(Ng,1010)Insrs(Failed(i))(1:n1)//'.spc',
     &                   Outsrs(Failed(i))(1:n2)//'_err.html',
     &                   Outsrs(Failed(i))(1:n2)//'_err.html'
 1010      FORMAT('<li>',a,' (Error messages stored in <a href="',a,
     &            '">',a,'</a>)</li>')
          ELSE
           CALL writTagOneLine(Ng,'li','@',Insrs(Failed(i))(1:n1)//
     &                         '.spc')
          END IF
         ELSE
          IF(Lexist)THEN
           CALL writTagOneLine(Ng,'li','@',Insrs(Failed(i))(1:n1)//
     &                         '.spc (Error messages stored in '//
     &                         Outsrs(Failed(i))(1:n2)//'_err.html)')
          ELSE
           CALL writTagOneLine(Ng,'li','@',Insrs(Failed(i))(1:n1)//
     &                         '.spc')
          END IF
         END IF
        END IF
       END DO
       IF(.not.lhdr)THEN
        CALL writTag(Ng,'</ul>')
       END IF
C-----------------------------------------------------------------------
      END IF
C-----------------------------------------------------------------------
      IF(Nopen.gt.0)THEN
       CALL mkPOneLine(Ng,'@',PRGNAM//
     &                 ' is unable to open input/output files for '//
     &                 'the following sets of filenames:')
       CALL writTagClass(Ng,'ol','indent')
C-----------------------------------------------------------------------
       DO i=1,Nopen
        n1=nblank(Insrs(Unopnd(i)))
        n2=nblank(Outsrs(Unopnd(i)))
        IF(n1.gt.0.and.n2.gt.0)THEN
         CALL writTagOneLine(Ng,'li','@','Input filename:  '//
     &                       Insrs(Unopnd(i))(1:n1)//Cbr//
     &                     'Output filename: '//Outsrs(Unopnd(i))(1:n2))
        ELSE IF(n1.eq.0.and.n2.eq.0)THEN
         CALL writTagOneLine(Ng,'li','@',
     &                       'Input filename: NOT SPECIFIED '//Cbr//
     &                       'Output filename: NOT SPECIFIED')
        ELSE IF(n1.eq.0)THEN
         CALL writTagOneLine(Ng,'li','@',
     &                       'Input filename: NOT SPECIFIED '//Cbr//
     &                       'Output filename: '//
     &                       Outsrs(Unopnd(i))(1:n2))
        ELSE
         CALL writTagOneLine(Ng,'li','@','Input filename:  '//
     &                       Insrs(Unopnd(i))(1:n1)//Cbr//
     &                       'Output filename: NOT SPECIFIED')
        END IF
       END DO
       CALL writTag(Ng,'</ol>')
      END IF
C-----------------------------------------------------------------------
      RETURN
      END
