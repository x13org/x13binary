C     Last change:  BCM  26 Feb 1999    3:52 pm
      SUBROUTINE ssrit(X,L1,L2,Isec,Series)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c  *****  stores results from x-11.2 runs into sliding spans variables.
c  *****  convert results from double to single precision.
c-----------------------------------------------------------------------
      INCLUDE 'srslen.prm'
      INCLUDE 'notset.prm'
      INCLUDE 'agr.cmn'
      INCLUDE 'agrsrs.cmn'
      INCLUDE 'lzero.cmn'
      INCLUDE 'ssap.prm'
      INCLUDE 'ssap.cmn'
      INCLUDE 'sspinp.cmn'
      INCLUDE 'sspdat.cmn'
      INCLUDE 'ssft.cmn'
      INCLUDE 'x11opt.cmn'
c-----------------------------------------------------------------------
      DOUBLE PRECISION ONEHND
      PARAMETER(ONEHND=100D0)
c-----------------------------------------------------------------------
      INTEGER i,i0,Isec,L1,l10,L2,l20,ll0
      DOUBLE PRECISION Series,X
      DIMENSION X(PLEN),Series(PLEN)
c-----------------------------------------------------------------------
      IF(Isec.eq.0)L0=L0+Nsea
c-----------------------------------------------------------------------
c  set options or check for indirect sliding spans, as necessary  
c-----------------------------------------------------------------------
      IF(Iagr.eq.2.and.Isec.eq.3.and.Icol.eq.1.and.Iag.ge.0)THEN
       Nscomp=Nscomp+1
       IF(Indssp.eq.NOTSET)THEN
        Indssp=1
        Indcol=Ncol
        Indlen=Nlen
       ELSE IF (Indssp.gt.0)THEN
        IF(Nlen.ne.Indlen)THEN
         Indssp=-1
        ELSE IF(Ncol.ne.Indcol)THEN
         Indssp=-2
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
      l10=L1-L0+1
      IF(l10.ne.1)THEN
       DO i=1,l10-1
        IF(Isec.le.1)Td(i,Icol)=DNOTST
        IF(Isec.eq.2)S(i,Icol)=DNOTST
        IF(Isec.eq.3)THEN
         Sa(i,Icol)=DNOTST
         IF(Muladd.eq.1)Isfadd(i,Icol)=DNOTST
         IF(Iagr.eq.2.and.Iag.ge.0)THEN
          Saind(i,Icol)=DNOTST
          Sfind(i,Icol)=DNOTST
          Sfinda(i,Icol)=DNOTST
         END IF
        END IF
       END DO
      END IF
      DO i=L1,L2
       i0=i-L0+1
c         xss = sngl(x(i))
       IF(Isec.le.1)THEN
        IF(Muladd.eq.1)THEN
         Td(i0,Icol)=X(i)
        ELSE
         Td(i0,Icol)=X(i)*ONEHND
        END IF
       END IF
       IF(Isec.eq.2)THEN
        IF(Muladd.eq.1)THEN
         S(i0,Icol)=X(i)
        ELSE
         S(i0,Icol)=X(i)*ONEHND
        END IF
       END IF
       IF(Isec.eq.3)THEN
        Sa(i0,Icol)=X(i)
        IF(Muladd.eq.1.and.Kfulsm.eq.0.and.(.not.Ssdiff))THEN
         IF(X(i).gt.0D0)THEN
          Isfadd(i0,Icol)=(Series(i)/X(i))*ONEHND
         ELSE
          Ssdiff=.true.
         END IF
        END IF
c-----------------------------------------------------------------------
c     update variables of sliding spans analysis of 
c-----------------------------------------------------------------------
        IF(Indssp.gt.0)THEN
         IF(Iagr.eq.2)THEN
          IF(Iag.eq.0)Saind(i0,Icol)=Saind(i0,Icol)+(X(i)*W)
          IF(Iag.eq.1)Saind(i0,Icol)=Saind(i0,Icol)-(X(i)*W)
          IF(Iag.eq.2)Saind(i0,Icol)=Saind(i0,Icol)*(X(i)*W)
          IF(Iag.eq.3)Saind(i0,Icol)=Saind(i0,Icol)/(X(i)*W)
         ELSE IF(Iagr.gt.2)THEN
          IF(Muladd.eq.1)THEN
           Sfind(i0,Icol)=O2(i)-Saind(i0,Icol)
           IF(.not.Ssidif)THEN
            IF(Saind(i0,Icol).gt.0D0)THEN
             Sfinda(i0,Icol)=(O2(i)/Saind(i0,Icol))*ONEHND
            ELSE
             Ssidif=.true.
            END IF
           END IF
          ELSE
           Sfind(i0,Icol)=(O2(i)/Saind(i0,Icol))*ONEHND
          END IF
         END IF
        END IF
       END IF
      END DO
c      la=mod(Lfda,Nsea)
c      IF(la.eq.0)la=Nsea
      ll0=Sslen+Im+L0-2
      l20=ll0
      IF(mod(ll0,Nsea).ne.0)l20=((ll0/Nsea)+1)*Nsea
      IF(l20.eq.L2.and.Lstmo.lt.Nsea)l20=(Nsea-Lstmo)+l20
      IF(l20.ne.L2)THEN
       DO i=L2+1,l20
        i0=i-L0+1
        IF(Isec.le.1)Td(i0,Icol)=DNOTST
        IF(Isec.eq.2)S(i0,Icol)=DNOTST
        IF(Isec.eq.3)THEN
         Sa(i0,Icol)=DNOTST
         IF(Muladd.eq.1)Isfadd(i0,Icol)=DNOTST
         IF(Iagr.eq.2)THEN
          Saind(i0,Icol)=DNOTST
          Sfind(i0,Icol)=DNOTST
          Sfinda(i0,Icol)=DNOTST
         END IF
        END IF
       END DO
      END IF
      IF(Isec.eq.0)L0=L0-Nsea
      RETURN
      END
