      SUBROUTINE svtukp(Iagr,Lsumm,csPeak,ctPeak,csPk90,ctPk90,Lsadj)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'notset.prm'
      INCLUDE 'units.cmn'
      INCLUDE 'tukey.cmn'
      INCLUDE 'spctbl.i'
c-----------------------------------------------------------------------
      LOGICAL Lsadj
      CHARACTER thisLb*(9),csPeak*(35),ctPeak*(35),csPk90*(35),
     &          ctPk90*(35)
      INTEGER Iagr,i,k,nLb,iLb,nsPeak,ntPeak,nsPk90,ntPk90,p1,p2,npk,
     &        npk90,oriIdx,Lsumm
      DOUBLE PRECISION thisPk,thisTd
      DIMENSION thisPk(6)
c-----------------------------------------------------------------------
c   Initialize variables
c-----------------------------------------------------------------------
      oriIdx=NOTSET
      csPeak=' '
      ctPeak=' '
      csPk90=' '
      ctPk90=' '
      nsPeak=0
      ntPeak=0
      nsPk90=0
      ntPk90=0
c-----------------------------------------------------------------------
      DO i=1,Ntukey
c-----------------------------------------------------------------------
c   Set up labels, peak vectors
c-----------------------------------------------------------------------
       thisLb(1:3)='spc      '
       IF(Itukey(i).eq.LSPCRS)THEN
        CALL copy(Ptsr,6,1,thisPk)
        thisTD=Pttdr
        nLb=6
        thisLb(4:6)='rsd'
        iLb=4
       ELSE IF(Itukey(i).eq.LSPTS0.or.Itukey(i).eq.LSPT0C)THEN
        CALL copy(Ptso,6,1,thisPk)
        thisTD=Pttdo
        nLb=6
        thisLb(4:6)='ori'
        IF(.not.Lsadj)oriIdx=i
        iLb=4
       ELSE IF(Itukey(i).eq.LSPTS1.or.Itukey(i).eq.LSPT1I.or.
     &         Itukey(i).eq.LSPT1S)THEN
        CALL copy(Ptsa,6,1,thisPk)
        thisTD=Pttda
        IF(Itukey(i).eq.LSPTS1.or.Itukey(i).eq.LSPT1S)THEN
         nLb=5
         thisLb(4:5)='sa'
        ELSE IF(Itukey(i).eq.LSPT1I)THEN
         nLb=8
         thisLb(4:8)='indsa'
        END IF
        iLb=4
        IF(Iagr.gt.3)iLb=7
       ELSE IF(Itukey(i).eq.LSPTS2.or.Itukey(i).eq.LSPT2I.or.
     &         Itukey(i).eq.LSPT2S)THEN
        CALL copy(Ptsi,6,1,thisPk)
        thisTD=Pttdi
        IF(Itukey(i).eq.LSPTS2.or.Itukey(i).eq.LSPT2S)THEN
         nLb=6
         thisLb(4:6)='irr'
        ELSE IF(Itukey(i).eq.LSPT2I)THEN
         nLb=9
         thisLb(4:9)='indirr'
        END IF
        iLb=4
        IF(Iagr.gt.3)iLb=7
       END IF
c-----------------------------------------------------------------------
c   Write out peak probabilities
c-----------------------------------------------------------------------
       IF(Lsumm.gt.0)THEN
        DO k=1,6
         WRITE(Nform,1010)thisLb(1:nLb),'.tukey.s',k,': ',thisPk(k)
        END DO
        WRITE(Nform,1020)thisLb(1:nLb),'.tukey.td: ',thisTD
       END IF
c-----------------------------------------------------------------------
c   Set up inidication is tukey spectra of given estimate has a peak
c-----------------------------------------------------------------------
        npk=0
        npk90=0
        DO k=1,6
         IF(k.ne.oriIdx)THEN
          IF(thisPk(k).gt.0.90D0)npk90=npk90+1
          IF(thisPk(k).gt.0.99D0)npk=npk+1
         END IF
        END DO
        IF(npk90.gt.0)THEN
          p1=nsPk90+1
          p2=p1+(nLb-iLb)
          csPk90(p1:p2)=thisLb(iLb:nLb)
          nsPk90=p2+1
        END IF
        IF(npk.gt.0)THEN
          p1=nsPeak+1
          p2=p1+(nLb-iLb)
          csPeak(p1:p2)=thisLb(iLb:nLb)
          nsPeak=p2+1
        END IF
        IF(thisTD.gt.0.90D0)THEN
         p1=ntPk90+1
         p2=p1+(nLb-iLb)
         ctPk90(p1:p2)=thisLb(iLb:nLb)
         ntPk90=p2+1
        END IF
        IF(thisTD.gt.0.99D0)THEN
         p1=ntPeak+1
         p2=p1+(nLb-iLb)
         ctPeak(p1:p2)=thisLb(iLb:nLb)
         ntPeak=p2+1
        END IF
      END DO
c-----------------------------------------------------------------------
c   If no tukey peaks, set peak indicator to none
c-----------------------------------------------------------------------
      IF(nsPeak.eq.0)THEN
        nsPeak=4
        csPeak(1:nsPeak)='none'
      END IF
      IF(ntPeak.eq.0)THEN
        ntPeak=4
        ctPeak(1:ntPeak)='none'
      END IF
      IF(nsPk90.eq.0)THEN
        nsPk90=4
        csPk90(1:nsPk90)='none'
      END IF
      IF(ntPk90.eq.0)THEN
        ntPk90=4
        ctPk90(1:ntPk90)='none'
      END IF
c-----------------------------------------------------------------------
 1010 FORMAT(a,a,i1,a,f9.4)
 1020 FORMAT(a,a,f9.4)
c-----------------------------------------------------------------------
      RETURN
      END
