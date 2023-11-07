C 
C... Variables in Common Block /calc/ ... 
      integer TYPE,P,D,Q,BP,BD,BQ,PBP,PQ,NW,INIT,BPQ,IMEAN,IPR
      real*8 DETPRI
      real*8 Wd(MPKP),PHI(3*N1),TH(3*N1),BPHI(3*N1),BTH(3*N1),
     $       PHIST(2*N12+3*N1),THSTAR(40)
      common /calc/ Wd,PHI,TH,BPHI,BTH,PHIST,THSTAR,DETPRI,TYPE,P,D,Q,
     $              BP,BD,BQ,PBP,PQ,NW,INIT,BPQ,IMEAN,IPR
