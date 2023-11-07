C 
C... Variables in Common Block /fft/ ... 
      integer maxnz_1,nz
      double precision pi2
      parameter( maxnz_1=1099,pi2=6.28318530717959d0)
      double precision wgr(0:maxnz_1),wgi(0:maxnz_1)
      common /FFT_block/ wgr,wgi,nz
