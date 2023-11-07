C     Last change:  BCM  15 Apr 2005   12:40 pm
      SUBROUTINE xtrm(Xi,Kfda,Klda,Kfdax,Kldax)
      IMPLICIT NONE
c-----------------------------------------------------------------------
C --- THIS ROUTINE COMPUTES WEIGHTS FOR THE IRREGULAR COMPONENT
C --- AND IDENTIFIES EXTREME IRREGULAR VALUES
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'notset.prm'
c      INCLUDE 'x11ptr.cmn'
      INCLUDE 'lzero.cmn'
      INCLUDE 'x11opt.cmn'
      INCLUDE 'xtrm.cmn'
c-----------------------------------------------------------------------
      INTEGER PY1
      DOUBLE PRECISION ZERO,ONE
      LOGICAL F,T
      PARAMETER(PY1=PYRS+1,ZERO=0D0,ONE=1D0,F=.false.,T=.true.)
c-----------------------------------------------------------------------
      DOUBLE PRECISION sdev1,sdev2,xbar,Xi
      INTEGER i,inx,istep,j,jfda,jlda,k,Kfda,Klda,l,m,lx,
     &        mx,n,n1,n2,Kfdax,Kldax
      INTEGER n3,nfda,nlda
      DIMENSION Xi(PLEN)
c-----------------------------------------------------------------------
      DOUBLE PRECISION sdxtrm,wtxtrm
      LOGICAL dpeq
      EXTERNAL sdxtrm,wtxtrm,dpeq
c-----------------------------------------------------------------------
      n1=2*Ny
      n2=n1+Ny-1
      n3=n2+n1
      istep=1
      jfda=(Kfdax+Ny-2)/Ny*Ny+1
      jlda=Kldax/Ny*Ny-n3
c  changes suggested by NBB May 2004
      nfda=(Kfdax-1)/Ny*Ny+1
      nlda=(((Kldax-1)/Ny)+1)*Ny-n3
c  end of changes
      IF(jlda.lt.jfda)nlda=nfda
c-----------------------------------------------------------------------
C --- SET ALL WEIGHTS EQUAL TO 1.0 TO START
c-----------------------------------------------------------------------
      CALL setdp(ONE,PLEN,Stwt)
      CALL setdp(ZERO,Ny,Stdper)
      xbar=ONE
      IF(Muladd.ne.0)xbar=ZERO
      DO WHILE (istep.le.2)
c-----------------------------------------------------------------------
c	Check to see if a grouping of periods has been done.  If so,
c	calculate extremes using standard errors generated for the
c     grouped irregulars.
c-----------------------------------------------------------------------
       IF(Ksdev.eq.4)THEN
        sdev1=sdxtrm(Xi,xbar,Kfdax,Kldax,1,Imad,istep,Ny,T)
        sdev2=sdxtrm(Xi,xbar,Kfdax,Kldax,1,Imad,istep,Ny,F)
c-----------------------------------------------------------------------
c     Store sdev/MAD for each group
c-----------------------------------------------------------------------
        DO i=1,Ny
         Stdper(i)=sdev2
         IF(Csigvc(i))Stdper(i)=sdev1
        END DO
c-----------------------------------------------------------------------
        DO k=Kfda,Klda
c-----------------------------------------------------------------------
C --- COMPUTE DEVIATION OF EACH IRREGULAR VALUE.
c-----------------------------------------------------------------------
         i=mod(k,Ny)
         IF(i.eq.0)i=Ny
         IF(Stdper(i).gt.ZERO)Stwt(k)=wtxtrm(Xi(k),xbar,Stdper(i),Sigmu,
     &                                       Sigml,istep,Stwt(k))
        END DO
c-----------------------------------------------------------------------
c     Check to see if test for heterskedastic irregular has been
c     accepted.  If so, calculate extremes using standard errors
c     calculated for each month/quarter.
c-----------------------------------------------------------------------
       ELSE IF(Ksdev.gt.0)THEN
        DO l=Kfda,Kfda+Ny-1
         i=mod(l,Ny)
         IF(i.eq.0)i=Ny
c-----------------------------------------------------------------------
c     Calculate s.dev./MAD for a given calendar month/quarter.
c-----------------------------------------------------------------------
         lx=l
         IF(l.lt.Kfdax)THEN
          j=mod(lx,Ny)
          IF(j.eq.0)j=Ny
          IF(i.ge.j)THEN
           lx=Kfdax+(j-i)
          ELSE
           lx=Kfdax+Ny+(i-j)
          END IF
         END IF
         m=((Klda-l)/Ny)*Ny+l
         mx=((Kldax-l)/Ny)*Ny+l
         sdev1=sdxtrm(Xi,xbar,lx,mx,Ny,Imad,istep,Ny,T)
c-----------------------------------------------------------------------
c     Store sdev/MAD for month/quarter i
c-----------------------------------------------------------------------
         Stdper(i)=sdev1
c-----------------------------------------------------------------------
         IF(.not.dpeq(sdev1,ZERO))THEN
          DO k=l,m,Ny
           Stwt(k)=wtxtrm(Xi(k),xbar,sdev1,Sigmu,Sigml,istep,Stwt(k))
          END DO
         END IF
        END DO
c-----------------------------------------------------------------------
        IF(istep.eq.2)CALL setdp(DNOTST,PY1,Stdev)
       ELSE
c-----------------------------------------------------------------------
c     Else, identify extreme values and weights using standard X-11
c     Method
c-----------------------------------------------------------------------
        inx=3+((Lsp-1)/Ny)
        DO i=nfda,nlda,Ny
         IF(nlda.le.nfda)THEN
c-----------------------------------------------------------------------
C --- LESS THAN 5 YEARS AVAILABLE.
c-----------------------------------------------------------------------
          j=Kfda
          k=Klda
          l=Kfdax
          m=Kldax
         ELSE IF(i.le.nfda)THEN
c-----------------------------------------------------------------------
C --- BEGINNING OF SERIES
c-----------------------------------------------------------------------
          j=Kfda
          k=nfda+n2
          l=Kfdax
          m=jfda+n3
         ELSE IF(i.lt.nlda)THEN
c-----------------------------------------------------------------------
C --- CENTRAL YEARS
c-----------------------------------------------------------------------
          j=i+n1
          k=i+n2
          l=i
          m=n3+i
         ELSE
c-----------------------------------------------------------------------
C --- END OF SERIES
c-----------------------------------------------------------------------
          j=nlda+n1
          k=Klda
          l=jlda
          m=Kldax
         END IF
c-----------------------------------------------------------------------
C --- COMPUTE FIVE YEAR STANDARD DEVIATION (OR MEDIAN ABSOLUTE
C     DEVIATION) OF THE IRREGULARS.
c-----------------------------------------------------------------------
         sdev1=sdxtrm(Xi,xbar,l,m,1,Imad,istep,Ny,T)
c         write(*,*)' istep, l, m, sdev1 = ', istep, l, m, sdev1
c-----------------------------------------------------------------------
C --- STORE STANDARD DEVIATIONS FOR PRINTING IN TABLE OF WEIGHTS.
c-----------------------------------------------------------------------
         Stdev(inx)=sdev1
         inx=inx+1
         IF(.not.dpeq(sdev1,ZERO))THEN
c          write(*,*)' istep, k, Xi(',k,'), stwt(',k,'), sdev1 = ', 
c     &                istep, k, Xi(k), stwt(k), sdev1
          DO n=j,k
           Stwt(n)=wtxtrm(Xi(n),xbar,sdev1,Sigmu,Sigml,istep,Stwt(n))
          END DO
         END IF
        END DO
c-----------------------------------------------------------------------
        IF(istep.eq.2)THEN
         DO i=1,3
          Stdev(i-1+inx)=sdev1
          Stdev(i)=Stdev(3+((Lsp-1)/Ny))
         END DO
        END IF
c-----------------------------------------------------------------------
       END IF
       istep=istep+1
      END DO
c-----------------------------------------------------------------------
c      DO i=Kfda,Klda
      DO i=Kfdax,Kldax
       IF((Stwt(i)+ONE).le.ZERO)Stwt(i)=ZERO
      END DO
      RETURN
      END
