      SUBROUTINE mkstlb(Itbl,Spcstr,Nspstr,Spcsrs)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INTEGER Itbl,Nspstr,Spcsrs,Lsumm
      CHARACTER Spcstr*(36)
c-----------------------------------------------------------------------
      INCLUDE 'spctbl.i'
c-----------------------------------------------------------------------
       IF(Itbl.eq.LSPTS0)THEN
        IF(Spcsrs.eq.0)THEN
         Nspstr=19
         Spcstr(1:Nspstr)='Tukey(Spectrum_Ori)'
        ELSE IF(Spcsrs.eq.1)THEN
         Nspstr=25
         Spcstr(1:Nspstr)='Tukey(Spectrum_OtlAdjOri)'
        ELSE IF(Spcsrs.eq.2)THEN
         Nspstr=22
         Spcstr(1:Nspstr)='Tukey(Spectrum_AdjOri)'
        ELSE IF(Spcsrs.eq.3)THEN
         Nspstr=22
         Spcstr(1:Nspstr)='Tukey(Spectrum_ModOri)'
        END IF
       ELSE IF(Itbl.eq.LSPT0C)THEN
        IF(Spcsrs.eq.0)THEN
         Nspstr=20
         Spcstr(1:Nspstr)='Tukey(Spectrum_Comp)'
        ELSE IF(Spcsrs.eq.1)THEN
         Nspstr=26
         Spcstr(1:Nspstr)='Tukey(Spectrum_OtlAdjComp)'
        ELSE IF(Spcsrs.eq.2)THEN
         Nspstr=23
         Spcstr(1:Nspstr)='Tukey(Spectrum_AdjComp)'
        ELSE IF(Spcsrs.eq.3)THEN
         Nspstr=23
         Spcstr(1:Nspstr)='Tukey(Spectrum_ModComp)'
        END IF
       ELSE IF(Itbl.eq.LSPTER)THEN
        Nspstr=22
        Spcstr(1:Nspstr)='Tukey(Spectrum_ExtRsd)'
       ELSE IF(Itbl.eq.LSPTRS)THEN
        Nspstr=19
        Spcstr(1:Nspstr)='Tukey(Spectrum_Rsd)'
       ELSE IF(Itbl.eq.LSPT1I)THEN
        Nspstr=21
        Spcstr(1:Nspstr)='Tukey(Spectrum_IndSA)'
       ELSE IF(Itbl.eq.LSPT1S)THEN
        Nspstr=24
        Spcstr(1:Nspstr)='Tukey(Spectrum_SA_SEATS)'
       ELSE IF(Itbl.eq.LSPTS1)THEN
        Nspstr=18
        Spcstr(1:Nspstr)='Tukey(Spectrum_SA)'
       ELSE IF(Itbl.eq.LSPT2I)THEN
        Nspstr=22
        Spcstr(1:Nspstr)='Tukey(Spectrum_IndIrr)'
       ELSE IF(Itbl.eq.LSPT2S)THEN
        Nspstr=25
        Spcstr(1:Nspstr)='Tukey(Spectrum_Irr_SEATS)'
       ELSE IF(Itbl.eq.LSPTS2)THEN
        Nspstr=19
        Spcstr(1:Nspstr)='Tukey(Spectrum_Irr)'
       ELSE
        Nspstr=15
        Spcstr(1:Nspstr)='Tukey(Spectrum)'
       END IF
c-----------------------------------------------------------------------
      RETURN
      END