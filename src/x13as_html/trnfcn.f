C     Last change:  BCM  14 May 1998    8:45 am
      SUBROUTINE trnfcn(Y,Nsrs,Fcntyp,Lam,Trny)
c-----------------------------------------------------------------------
c     trnfcn.f, Release 1, Subroutine Version 1.5, Modified 07 Nov 1994.
c-----------------------------------------------------------------------
c     Box-Cox Transformation, of y, 1 to nsrs, and putting in the
c result in trny.  Transformation is:
c     trny=y                  , lam=1
c     trny=ln(y)              , lam=0, y>0
c     trny=lam^2+(y^lam-1)/lam, lam<>0 or 1, y>0
c-----------------------------------------------------------------------
c Name    Type Description
c-----------------------------------------------------------------------
c i       i    Local do loop index
c lam     d    Box-Cox transformation parameter
c lstop   l    Logical to call abend()
c nsrs    i    Length of the vectors
c trny    d    Transformed vector of length nsrs
c y       d    Vector to be transformed length nsrs
c-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'stdio.i'
      INCLUDE 'units.cmn'
c     ------------------------------------------------------------------
      LOGICAL T,F
      INTEGER PSTOP
      DOUBLE PRECISION ZO,ONE
      PARAMETER(ZO=0.0D0,ONE=1.0D0,T=.true.,F=.false.,PSTOP=10)
c     ------------------------------------------------------------------
      LOGICAL lstop
      INTEGER Fcntyp,i,Nsrs,nstop
      DOUBLE PRECISION Lam,tmp,Trny(Nsrs),Y(Nsrs)
c     ------------------------------------------------------------------
      LOGICAL dpeq
      EXTERNAL dpeq
c-----------------------------------------------------------------------
c     Initialize lstop to call abend()
c are found.
c-----------------------------------------------------------------------
      lstop=F
      nstop=0
c-----------------------------------------------------------------------
c     Lam=0, log transformation if y>0
c-----------------------------------------------------------------------
      IF(Fcntyp.eq.3)THEN
       DO i=1,Nsrs
        tmp=Y(i)
        IF(tmp.gt.ZO.and.tmp.lt.ONE)THEN
         Trny(i)=log(tmp/(ONE-tmp))
c     ------------------------------------------------------------------
        ELSE
         WRITE(STDERR,1010)'take the logit of',i,tmp
         WRITE(Mt2,1011)'take the logit of',i,tmp
         lstop=T
         nstop=nstop+1
         IF(nstop.gt.PSTOP)THEN
          CALL eWritln('Maximum number of errors printed.  '//
     &                 'More errors may exist, but',STDERR,Mt2,T,F)
          CALL writln(' will not be specified.  The above values '//
     &                'cannot be processed.',STDERR,Mt2,F,T)
          CALL abend()
          RETURN
         END IF
        END IF
       END DO
c-----------------------------------------------------------------------
c     Lam=1, no transformation, just copy the vector
c-----------------------------------------------------------------------
      ELSE IF(dpeq(Lam,ONE))THEN
       DO i=1,Nsrs
        Trny(i)=Y(i)
       END DO
c-----------------------------------------------------------------------
c     Lam=0, log transformation if y>0
c-----------------------------------------------------------------------
      ELSE IF(dpeq(Lam,ZO))THEN
       DO i=1,Nsrs
        IF(Y(i).gt.ZO)THEN
         Trny(i)=log(Y(i))
c     ------------------------------------------------------------------
        ELSE
         IF(Y(i).lt.ZO)THEN
          WRITE(STDERR,1020)'log of a negative number',i,Y(i)
          WRITE(Mt2,1021)'log of a negative number',i,Y(i)
         ELSE
          WRITE(STDERR,1020)'log of zero',i,Y(i)
          WRITE(Mt2,1021)'log of a zero',i,Y(i)
         END IF
         lstop=T
         nstop=nstop+1
         IF(nstop.gt.PSTOP)THEN
          CALL eWritln('Maximum number of errors printed.  '//
     &                 'More errors may exist, but',STDERR,Mt2,T,F)
          CALL writln(' will not be specified.  The above values '//
     &                'cannot be processed.',STDERR,Mt2,F,T)
          CALL abend()
          RETURN
         END IF
        END IF
       END DO
c-----------------------------------------------------------------------
c     Lam not equal to 1 or 0
c-----------------------------------------------------------------------
      ELSE
       DO i=1,Nsrs
        IF(Y(i).gt.ZO)THEN
         Trny(i)=Lam**2+(Y(i)**Lam-ONE)/Lam
c     ------------------------------------------------------------------
        ELSE
         WRITE(STDERR,1020)'BoxCox transform',i,Y(i)
         WRITE(Mt2,1021)'BoxCox transform',i,Y(i)
         lstop=T
         nstop=nstop+1
         IF(nstop.gt.PSTOP)THEN
          CALL eWritln('Maximum number of errors printed.  '//
     &                 'More errors may exist, but',STDERR,Mt2,T,F)
          CALL writln(' will not be specified.  The above values '//
     &                'cannot be processed.',STDERR,Mt2,F,T)
          CALL abend()
          RETURN
         END IF
        END IF
       END DO
      END IF
c     ------------------------------------------------------------------
      IF(lstop)CALL abend()
c     ------------------------------------------------------------------
 1010 FORMAT(/,' ERROR: Cannot ',a,' a proportion not in the range ',
     &         '(0,1), y(',i5,')=',1p,g16.8,'.',/)
 1011 FORMAT(/,'<p><strong>ERROR:</strong> Cannot ',a,
     &         ' a proportion not in the range ',
     &         '(0,1), y(',i5,')=',1p,g16.8,'.</p>',/)
 1020 FORMAT(' ERROR: Do not take ',a,', y(',i5,')=',1p,g16.8,'.')
 1021 FORMAT(' <p><strong>ERROR:</strong> Do not take ',a,
     &       ', y(',i5,')=',1p,g16.8,'.</p>')
c     ------------------------------------------------------------------
      RETURN
      END
