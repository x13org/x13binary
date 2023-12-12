      SUBROUTINE mksplb(Itbl,Spcstr,Nspstr,Spcsrs,Ldecbl)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INTEGER Itbl,Nspstr,Spcsrs,Lsumm
      LOGICAL Ldecbl
      CHARACTER Spcstr*(36)
c-----------------------------------------------------------------------
      INCLUDE 'spctbl.i'
c-----------------------------------------------------------------------
      IF(Ldecbl)THEN
       IF(Itbl.eq.LSPCS0)THEN
        IF(Spcsrs.eq.0)THEN
         Nspstr=20
         Spcstr(1:Nspstr)='10*Log(Spectrum_Ori)'
        ELSE IF(Spcsrs.eq.1)THEN
         Nspstr=26
         Spcstr(1:Nspstr)='10*Log(Spectrum_OtlAdjOri)'
        ELSE IF(Spcsrs.eq.2)THEN
         Nspstr=23
         Spcstr(1:Nspstr)='10*Log(Spectrum_AdjOri)'
        ELSE IF(Spcsrs.eq.3)THEN
         Nspstr=23
         Spcstr(1:Nspstr)='10*Log(Spectrum_ModOri)'
        END IF
       ELSE IF(Itbl.eq.LSPS0C)THEN
        IF(Spcsrs.eq.0)THEN
         Nspstr=21
         Spcstr(1:Nspstr)='10*Log(Spectrum_Comp)'
        ELSE IF(Spcsrs.eq.1)THEN
         Nspstr=27
         Spcstr(1:Nspstr)='10*Log(Spectrum_OtlAdjComp)'
        ELSE IF(Spcsrs.eq.2)THEN
         Nspstr=24
         Spcstr(1:Nspstr)='10*Log(Spectrum_AdjComp)'
        ELSE IF(Spcsrs.eq.3)THEN
         Nspstr=24
         Spcstr(1:Nspstr)='10*Log(Spectrum_ModComp)'
        END IF
       ELSE IF(Itbl.eq.LSPERS)THEN
        Nspstr=23
        Spcstr(1:Nspstr)='10*Log(Spectrum_ExtRsd)'
       ELSE IF(Itbl.eq.LSPCRS)THEN
        Nspstr=20
        Spcstr(1:Nspstr)='10*Log(Spectrum_Rsd)'
       ELSE IF(Itbl.eq.LSPS1I)THEN
        Nspstr=22
        Spcstr(1:Nspstr)='10*Log(Spectrum_IndSA)'
       ELSE IF(Itbl.eq.LSPS1S)THEN
        Nspstr=25
        Spcstr(1:Nspstr)='10*Log(Spectrum_SA_SEATS)'
       ELSE IF(Itbl.eq.LSPCS1)THEN
        Nspstr=19
        Spcstr(1:Nspstr)='10*Log(Spectrum_SA)'
       ELSE IF(Itbl.eq.LSPS2I)THEN
        Nspstr=23
        Spcstr(1:Nspstr)='10*Log(Spectrum_IndIrr)'
       ELSE IF(Itbl.eq.LSPS2S)THEN
        Nspstr=26
        Spcstr(1:Nspstr)='10*Log(Spectrum_Irr_SEATS)'
       ELSE IF(Itbl.eq.LSPCS2)THEN
        Nspstr=20
        Spcstr(1:Nspstr)='10*Log(Spectrum_Irr)'
       ELSE
        Nspstr=16
        Spcstr(1:Nspstr)='10*Log(Spectrum)'
       END IF
      ELSE
       IF(Itbl.eq.LSPCS0)THEN
        IF(Spcsrs.eq.0)THEN
         Nspstr=12
         Spcstr(1:Nspstr)='Spectrum_Ori'
        ELSE IF(Spcsrs.eq.1)THEN
         Nspstr=18
         Spcstr(1:Nspstr)='Spectrum_OtlAdjOri'
        ELSE IF(Spcsrs.eq.2)THEN
         Nspstr=15
         Spcstr(1:Nspstr)='Spectrum_AdjOri'
        ELSE IF(Spcsrs.eq.3)THEN
         Nspstr=15
         Spcstr(1:Nspstr)='Spectrum_ModOri'
        END IF
       ELSE IF(Itbl.eq.LSPS0C)THEN
        IF(Spcsrs.eq.0)THEN
         Nspstr=13
         Spcstr(1:Nspstr)='Spectrum_Comp'
        ELSE IF(Spcsrs.eq.1)THEN
         Nspstr=19
         Spcstr(1:Nspstr)='Spectrum_OtlAdjComp'
        ELSE IF(Spcsrs.eq.2)THEN
         Nspstr=16
         Spcstr(1:Nspstr)='Spectrum_AdjComp'
        ELSE IF(Spcsrs.eq.3)THEN
         Nspstr=16
         Spcstr(1:Nspstr)='Spectrum_ModComp'
        END IF
       ELSE IF(Itbl.eq.LSPERS)THEN
        Spcstr='Spectrum_ExtRsd'
        Nspstr=15
       ELSE IF(Itbl.eq.LSPCRS)THEN
        Spcstr='Spectrum_Rsd'
        Nspstr=12
       ELSE IF(Itbl.eq.LSPS1I)THEN
        Nspstr=14
        Spcstr(1:Nspstr)='Spectrum_IndSA'
       ELSE IF(Itbl.eq.LSPS1S)THEN
        Nspstr=17
        Spcstr(1:Nspstr)='Spectrum_SA_SEATS'
       ELSE IF(Itbl.eq.LSPCS1)THEN
        Nspstr=11
        Spcstr(1:Nspstr)='Spectrum_SA'
       ELSE IF(Itbl.eq.LSPS2I)THEN
        Nspstr=15
        Spcstr(1:Nspstr)='Spectrum_IndIrr'
       ELSE IF(Itbl.eq.LSPS2S)THEN
        Nspstr=18
        Spcstr(1:Nspstr)='Spectrum_Irr_SEATS'
       ELSE IF(Itbl.eq.LSPCS2)THEN
        Nspstr=12
        Spcstr(1:Nspstr)='Spectrum_Irr'
       ELSE
        Nspstr=8
        Spcstr(1:Nspstr)='Spectrum'
       END IF
      END IF
      RETURN
      END