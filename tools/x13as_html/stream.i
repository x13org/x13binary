C 
C... Variables in Common Block /stream/ ... 
      integer MaxStrLength,MaxLineLength
      Parameter(MaxStrLength=2000,MaxLineLength=120)
      integer NIO,Nprof
      common /stream/ NIO,Nprof
