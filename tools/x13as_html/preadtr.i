C
C... Variables in Common Block /preadtr/ ...
      integer NOUTR,NOUIR,NEAST,NPATD,NPAREG,TRAMO,KUNITS,SUNITS
      integer NOUS,NDS
      integer NEFF(0:7)
      real*8 TRAM(MPKP),PAOUTR(MPKP),PAOUIR(MPKP),PAEAST(MPKP),
     $       PAOUS(MPKP),PATD(MPKP),PAREG(MPKP,0:7),TSE(KL),
     $       TramLin(MPKP)
      real*8 DETSEAS(12)
      common /preadtr/ TRAM,PAOUTR,PAOUS,PAOUIR,PAEAST,
     $                 PATD,PAREG,TSE,DETSEAS,TramLin
	  common /ipradtr/ NOUTR,NOUIR,NOUS,NEAST,NPATD,NPAREG,TRAMO,
     $                 NDS,KUNITS,SUNITS,NEFF
