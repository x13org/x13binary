C 
C... Variables in Common Block /LogTrace/ ... 
      integer ntrace
	  character*80 TrTitle(50000)
      real*8 Dstdres(50000)
      common /logtrace/  Dstdres,TrTitle,ntrace
