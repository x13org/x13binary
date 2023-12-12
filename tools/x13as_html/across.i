C 
C    Created by REG on 12 Aug 2005
C 
C... Variables in Common Block /altcrosscov/ ... 
C    Cross Covariance Estimates
      real*8 seaIrrEst, seaTreEst, treIrrEst
C    Cross Covariance Estimators
      real*8 seaIrrEso, seaTreEso, treIrrEso
C    Cross Covariance Variances
      real*8 seaIrrVar, seaTreVar, treIrrVar
C    Cross Covariance Diagnostics
      real*8 seaIrrDia, seaTreDia, treIrrDia
C    Cross Covariance Pvalues
      real*8 seaIrrDgP, seaTreDgP, treIrrDgP
C    Cross Covariance Classes: 'ok', '+ ', '- ', '++', '--'
      character*2 seaIrrDgC, seaTreDgC, treIrrDgC
C    The Cross Covariance common block
      common /altcrosscov/
     &  seaIrrEst, seaTreEst, treIrrEst,
     &  seaIrrEso, seaTreEso, treIrrEso,
     &  seaIrrVar, seaTreVar, treIrrVar,
     &  seaIrrDia, seaTreDia, treIrrDia,
     &  seaIrrDgP, seaTreDgP, treIrrDgP,
     &  seaIrrDgC, seaTreDgC, treIrrDgC
