C 
C    Created by REG on 12 Aug 2005
C
C... Variables in Common Block /acfast/ 
C    Full statistics ...
      real*8 FACFPER(0:12),FACFPEM(0:12),FACFAER(0:12),FACFAEM(0:12),
     $       FACFSER(0:12),FACFSEM(0:12),FACFIER(0:12),FACFIEM(0:12)
C    Noend statistics ...
      real*8 NACFPER(0:12),NACFPEM(0:12),NACFAER(0:12),NACFAEM(0:12),
     $       NACFSER(0:12),NACFSEM(0:12),NACFIER(0:12),NACFIEM(0:12)
C    Weighted statistics ...
      real*8 WACFPER(0:12),WACFPEM(0:12),WACFAER(0:12),WACFAEM(0:12),
     $       WACFSER(0:12),WACFSEM(0:12),WACFIER(0:12),WACFIEM(0:12)
C    Full Diagnostics
      real*8 FACFPDG(0:12),FACFADG(0:12),FACFSDG(0:12),FACFIDG(0:12)
C    Noend Diagnostics
      real*8 NACFPDG(0:12),NACFADG(0:12),NACFSDG(0:12),NACFIDG(0:12)
C    Weighted Diagnostics
      real*8 WACFPDG(0:12),WACFADG(0:12),WACFSDG(0:12),WACFIDG(0:12)
C    Full Diagnostic Pvalues
      real*8 FACFPDP(0:12),FACFADP(0:12),FACFSDP(0:12),FACFIDP(0:12)
C    Noend Diagnostic Pvalues
      real*8 NACFPDP(0:12),NACFADP(0:12),NACFSDP(0:12),NACFIDP(0:12)
C    Weighted Diagnostic Pvalues
      real*8 WACFPDP(0:12),WACFADP(0:12),WACFSDP(0:12),WACFIDP(0:12)
C    Full Diagnostic Classes: 'ok', '+ ', '- ', '++', '--'
      character*2 FACFPDC(0:12),FACFADC(0:12),FACFSDC(0:12),
     $            FACFIDC(0:12)
C    Noend Diagnostic Classes: 'ok', '+ ', '- ', '++', '--'
      character*2 NACFPDC(0:12),NACFADC(0:12),NACFSDC(0:12),
     $            NACFIDC(0:12)
C    Weighted Diagnostic Classes: 'ok', '+ ', '- ', '++', '--'
      character*2 WACFPDC(0:12),WACFADC(0:12),WACFSDC(0:12),
     $            WACFIDC(0:12)
      common /acfast/ FACFPER,FACFPEM,FACFAER,FACFAEM,
     $                FACFSER,FACFSEM,FACFIER,FACFIEM,
     $                NACFPER,NACFPEM,NACFAER,NACFAEM,
     $                NACFSER,NACFSEM,NACFIER,NACFIEM,
     $                FACFPDG,FACFADG,FACFSDG,FACFIDG,
     $                NACFPDG,NACFADG,NACFSDG,NACFIDG,
     $                FACFPDP,FACFADP,FACFSDP,FACFIDP,
     $                NACFPDP,NACFADP,NACFSDP,NACFIDP,
     $                FACFPDC,FACFADC,FACFSDC,FACFIDC,
     $                NACFPDC,NACFADC,NACFSDC,NACFIDC,
     $                WACFPER,WACFPEM,WACFAER,WACFAEM,
     $                WACFSER,WACFSEM,WACFIER,WACFIEM,
     $                WACFPDG,WACFADG,WACFSDG,WACFIDG,
     $                WACFPDP,WACFADP,WACFSDP,WACFIDP,
     $                WACFPDC,WACFADC,WACFSDC,WACFIDC
