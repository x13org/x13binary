C 
C... Variables in Common Block /serrlev/ ... 
      real*8 SEFES
      real*8 SETA(-kp:kp),SETP(-kp:kp),SETS(-kp:kp),SETC(-kp:kp),
     $       SESER(kp),SERA(-kp:kp),SERP(-kp:kp),SERC(-kp:kp),
     $       SERS(-kp:kp)
      common /serrlev/ SETA,SETP,SETS,SETC,SESER,SEFES,SERA,SERP,SERC,
     $                 SERS
