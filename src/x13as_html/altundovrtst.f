      subroutine altUnderOverTest( Mq, Out )
c-----------------------------------------------------------------------
c     altUnderOverTest.f, Release 1, Subroutine Version 1.5,
c      Modified 27 Apr 2005.
c-----------------------------------------------------------------------
c Changes:
c     Created by REG, on 16 Aug 2005.
c     Modified by REG, on 13 Sep 2005, to set character class to '??'
c       if applicable component is not used.
c     Modified by REG, on 16 Sep 2005, to add SA diagnostics 
c       and to correct tab usage.
c     Modified by REG, on 21 Oct 2005, to add SEATS Out parameter
c       in order to qualify write statements.
c     Modified by REG, on 04 Apr 2006, to add weighted version
c       of lag diagnostics.
c     Modified by REG, on 27 Apr 2006, to redefine the .05 significance
c       level as .0227 and the .01 significance level as .0013,
c       and to handle special case of no seasonal component.
c-----------------------------------------------------------------------
c     This subroutine provides a alternative to the SEATS UnderOverTest
c     by using the alternative diagnostics calculated by getDiag() 
c     and stored in the common block referenced by acfast.i.
c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c Mq      i   Seasonal period.
c Out     i   SEATS output parameter.
c-----------------------------------------------------------------------
c Name   Type Description (Local Variables)
c-----------------------------------------------------------------------
c nstar   i   number of diagnostics that could not be classified.
c cpval1  c   character version of a pvalue if diagnostic class not 'ok'
c cpval2  c   character version of a pvalue if diagnostic class not 'ok'
c cpval3  c   character version of a pvalue if diagnostic class not 'ok'
c lOutput l   logical when true alows output to .out file
c-----------------------------------------------------------------------
      implicit none
c-----------------------------------------------------------------------
c     Declare input/output variables
c-----------------------------------------------------------------------
      integer Mq, Out
c     ------------------------------------------------------------------
c     Declare local variables
c     ------------------------------------------------------------------
      integer nstar, inc
      character cpval1*8, cpval2*8, cpval3*8
      logical lOutput
c     ------------------------------------------------------------------
c     Include files
c     ------------------------------------------------------------------
      include 'stream.i'
      include 'acfast.i'
      include 'across.i'
      include 'models.i'
      include 'htmlout.cmn'

cc
cc Decide on whether to output to .out file (or not).
cc
      lOutput = (out .eq. 0) .or. (out .eq. 2)
cc
cc Output over/under title
cc
      if (lOutput) then
       CALL genSkip(1000)
       CALL writTagOneLine(Nio,'h3','@','SECOND ORDER MOMENTS OF '//
     $                     'THE (STATIONARY) COMPONENTS OVER / UNDER '//
     $                     'ESTIMATION TESTS')
cc
cc Output subtest title for Variance test
cc
       CALL writTagOneLine(Nio,'h4','@','1. VARIANCE')
       CALL mkTableTag(Nio,'w50','@')
       CALL mkCaption(Nio,'1. VARIANCE')
      end if
cc
c Trend Variance Over/Under estimation
cc
      nstar = 0
      if (nthetp .gt. 1) then
       call altUnderOverClass( nstar, Facfpdg(0), Facfpdp(0),
     &                         Facfpdc(0), cpval1 )
       call altUnderOverClass( nstar, Nacfpdg(0), Nacfpdp(0),
     &                         Nacfpdc(0), cpval2 )
       call altUnderOverClass( nstar, Wacfpdg(0), Wacfpdp(0),
     &                         Wacfpdc(0), cpval3 )
       if (lOutput) then
        write (Nio, 1000) 'TREND-CYCLE (Full)    ', Facfpdc(0), cpval1
        write (Nio, 1000) 'TREND-CYCLE (Noend)   ', Nacfpdc(0), cpval2
        write (Nio, 1000) 'TREND-CYCLE (Weighted)', Wacfpdc(0), cpval3
       end if
 1000  FORMAT('<tr><th>',a,'</th><td class="center">',a,' (',a,
     &        ')</td></tr>')
      else
       Facfpdc(0) = '??'
       Nacfpdc(0) = '??'
       Wacfpdc(0) = '??'
      end if
cc
c Seasonal Variance Over/Under estimation
cc
      if (nthets .gt. 1) then
       call altUnderOverClass( nstar, Facfsdg(0), Facfsdp(0),
     &                         Facfsdc(0), cpval1 )
       call altUnderOverClass( nstar, Nacfsdg(0), Nacfsdp(0),
     &                         Nacfsdc(0), cpval2 )
       call altUnderOverClass( nstar, Wacfsdg(0), Wacfsdp(0),
     &                         Wacfsdc(0), cpval3 )
       if (lOutput) then
        write (Nio, 1000) 'SEASONAL (Full)       ', Facfsdc(0), cpval1
        write (Nio, 1000) 'SEASONAL (Noend)      ', Nacfsdc(0), cpval2
        write (Nio, 1000) 'SEASONAL (Weighted)   ', Wacfsdc(0), cpval3
       end if
      else
       Facfsdc(0) = '??'
       Nacfsdc(0) = '??'
       Wacfsdc(0) = '??'
      end if
cc
c Irregular Variance Over/Under estimation
cc
      call altUnderOverClass( nstar, Facfidg(0), Facfidp(0),
     &                        Facfidc(0), cpval1 )
      call altUnderOverClass( nstar, Nacfidg(0), Nacfidp(0),
     &                        Nacfidc(0), cpval2 )
      call altUnderOverClass( nstar, Wacfidg(0), Wacfidp(0),
     &                        Wacfidc(0), cpval3 )
      if (lOutput) then
       write (Nio, 1000) 'IRREGULAR (Full)      ', Facfidc(0), cpval1
       write (Nio, 1000) 'IRREGULAR (Noend)     ', Nacfidc(0), cpval2
       write (Nio, 1000) 'IRREGULAR (Weighted)  ', Wacfidc(0), cpval3
      end if
cc
c SA Variance Over/Under estimation
cc
      if (nthets .gt. 1) then
       call altUnderOverClass( nstar, Facfadg(0), Facfadp(0),
     &                         Facfadc(0), cpval1 )
       call altUnderOverClass( nstar, Nacfadg(0), Nacfadp(0),
     &                         Nacfadc(0), cpval2 )
       call altUnderOverClass( nstar, Wacfadg(0), Wacfadp(0),
     &                         Wacfadc(0), cpval3 )
       if (lOutput) then
        write (Nio, 1000) 'ADJUSTED (Full)       ', Facfadc(0), cpval1
        write (Nio, 1000) 'ADJUSTED (Noend)      ', Nacfadc(0), cpval2
        write (Nio, 1000) 'ADJUSTED (Weighted)   ', Wacfadc(0), cpval3
       end if
      else
       Facfadc(0) = '??'
       Nacfadc(0) = '??'
       Wacfadc(0) = '??'
      end if
cc
cc Output table of class definitions
cc
      if (lOutput) then
       CALL writTag(nio,'</table>')
       CALL mkPOneLine(Nio,'@','&nbsp;')
       CALL writTagClass(Nio,'ul','nob')
       write (Nio,1020)'++','Overestimation of component.'//
     $                     ' Strong evidence (p&lt;.0013).'
       write (Nio,1020)' +','Overestimation of component.'//
     $                     ' Mild evidence (.0013&lt;p&lt;.0227).'
       write (Nio,1020)'--','Underestimation of component.'//
     $                     ' Strong evidence (p&lt;.0013).'
       write (Nio,1020)' -','Underestimation of component.'//
     $                     ' Mild evidence (.0013&lt;p&lt;.0227).'
       if (nstar .gt. 0) write (Nio,1020)'**','unreliable test.'
       CALL writTag(nio,'</ul>')
      end if
 1020 FORMAT('<li><em>',a,' :</em> ',a,'</li>')
cc
cc Output subtest title for Autocovariance test
cc
      if (lOutput) then
       CALL writTagOneLine(Nio,'h4','@','2. AUTOCORRELATION')
       CALL mkTableTag(Nio,'w50','@')
       CALL mkCaption(Nio,'2. AUTOCORRELATION')

       write (Nio,1030)Cbr,Cbr
 1030  FORMAT('<tr><th>&nbsp;</th><th scope="col">FIRST ORDER ',a,
     $        ' AUTOCORRELATION</th>',/,
     $        '<th scope="col">SEASONAL ORDER ',a,' AUTOCORRELATION',
     $        '</th></tr>')
      end if
cc
c Trend first and seasonal orders (full data)
cc
      nstar = 0
      if (nthetp .gt. 1) then
       call altUnderOverClass( nstar, Facfpdg(1),  Facfpdp(1),
     &                         Facfpdc(1),  cpval1 )
       call altUnderOverClass( nstar, Facfpdg(Mq), Facfpdp(Mq),
     &                         Facfpdc(Mq), cpval2 )
       if (lOutput) then
        write (Nio, 2000)'TREND-CYCLE (Full)    ',
     &                   Facfpdc(1), cpval1, Facfpdc(Mq), cpval2
       end if
      else
       Facfpdc(1)  = '??'
       Facfpdc(Mq) = '??'
      end if
 2000 FORMAT('<tr><th scope="row">',a,'</th>',
     &       2('<td class="center">',a,' (',a,')</td>'),'</tr>')
cc
c Trend first and seasonal orders (noend data)
cc
      if (nthetp .gt. 1) then
       call altUnderOverClass( nstar, Nacfpdg(1),  Nacfpdp(1),
     &                         Nacfpdc(1),  cpval1 )
       call altUnderOverClass( nstar, Nacfpdg(Mq), Nacfpdp(Mq),
     &                         Nacfpdc(Mq), cpval2 )
       if (lOutput) then
        write (Nio, 2000)'TREND-CYCLE (Noend)   ',
     &                   Nacfpdc(1), cpval1, Nacfpdc(Mq), cpval2
       end if
      else
       Nacfpdc(1)  = '??'
       Nacfpdc(Mq) = '??'
      end if
cc
c Trend first and seasonal orders (weighted data)
cc
      if (nthetp .gt. 1) then
       call altUnderOverClass( nstar, Wacfpdg(1),  Wacfpdp(1),
     &                         Wacfpdc(1),  cpval1 )
       call altUnderOverClass( nstar, Wacfpdg(Mq), Wacfpdp(Mq),
     &                         Wacfpdc(Mq), cpval2 )
       if (lOutput) then
        write (Nio, 2000)'TREND-CYCLE (Weighted)',
     &                   Wacfpdc(1), cpval1, Wacfpdc(Mq), cpval2
       end if
      else
       Wacfpdc(1)  = '??'
       Wacfpdc(Mq) = '??'
      end if
cc
c Seasonal first and seasonal orders (full data)
cc
      if (nthets .gt. 1) then
       call altUnderOverClass( nstar, Facfsdg(1),  Facfsdp(1),
     &                         Facfsdc(1),  cpval1 )
       call altUnderOverClass( nstar, Facfsdg(Mq), Facfsdp(Mq),
     &                         Facfsdc(Mq), cpval2 )
       if (lOutput) then
        write (Nio, 2000)'SEASONAL (Full)       ',
     &                   Facfsdc(1), cpval1, Facfsdc(Mq), cpval2
       end if
      else
       Facfsdc(1)  = '??'
       Facfsdc(Mq) = '??'
      end if
cc
c Seasonal first and seasonal orders (noend data)
cc
      if (nthets .gt. 1) then
       call altUnderOverClass( nstar, Nacfsdg(1),  Nacfsdp(1),
     &                         Nacfsdc(1),  cpval1 )
       call altUnderOverClass( nstar, Nacfsdg(Mq), Nacfsdp(Mq),
     &                         Nacfsdc(Mq), cpval2 )
       if (lOutput) then
        write (Nio, 2000)'SEASONAL (Noend)      ',
     &                   Nacfsdc(1), cpval1, Nacfsdc(Mq), cpval2
       end if
      else
       Nacfsdc(1)  = '??'
       Nacfsdc(Mq) = '??'
      end if
cc
c Seasonal first and seasonal orders (weighted data)
cc
      if (nthets .gt. 1) then
       call altUnderOverClass( nstar, Wacfsdg(1),  Wacfsdp(1),
     &                         Wacfsdc(1),  cpval1 )
       call altUnderOverClass( nstar, Wacfsdg(Mq), Wacfsdp(Mq),
     &                         Wacfsdc(Mq), cpval2 )
       if (lOutput) then
        write (Nio, 2000)'SEASONAL (Weighted)   ',
     &                   Wacfsdc(1), cpval1, Wacfsdc(Mq), cpval2
       end if
      else
       Wacfsdc(1)  = '??'
       Wacfsdc(Mq) = '??'
      end if
cc
c Irregular first and seasonal orders (full data)
cc
      call altUnderOverClass( nstar, Facfidg(1), Facfidp(1),
     &                        Facfidc(1),  cpval1 )
      call altUnderOverClass( nstar, Facfidg(Mq), Facfidp(Mq),
     &                        Facfidc(Mq), cpval2 )
      if (lOutput) then
       write (Nio, 2000)'IRREGULAR (Full)      ',
     &                  Facfidc(1), cpval1, Facfidc(Mq), cpval2
      end if
cc
c Irregular first and seasonal orders (noend data)
cc
      call altUnderOverClass( nstar, Nacfidg(1),  Nacfidp(1),
     &                        Nacfidc(1),  cpval1 )
      call altUnderOverClass( nstar, Nacfidg(Mq), Nacfidp(Mq),
     &                        Nacfidc(Mq), cpval2 )
      if (lOutput) then
       write (Nio, 2000)'IRREGULAR (Noend)     ',
     &                  Nacfidc(1), cpval1, Nacfidc(Mq), cpval2
      end if
cc
c Irregular first and seasonal orders (weighted data)
cc
      call altUnderOverClass( nstar, Wacfidg(1),  Wacfidp(1),
     &                        Wacfidc(1),  cpval1 )
      call altUnderOverClass( nstar, Wacfidg(Mq), Wacfidp(Mq),
     &                        Wacfidc(Mq), cpval2 )
      if (lOutput) then
       write (Nio, 2000)'IRREGULAR (Weighted)  ',
     &                  Wacfidc(1), cpval1, Wacfidc(Mq), cpval2
      end if
cc
c SA first and seasonal orders (full data)
cc
      if (nthets .gt. 1) then
       call altUnderOverClass( nstar, Facfadg(1), Facfadp(1),
     &                         Facfadc(1),  cpval1 )
       call altUnderOverClass( nstar, Facfadg(Mq), Facfadp(Mq),
     &                         Facfadc(Mq), cpval2 )
       if (lOutput) then
        write (Nio, 2000)'ADJUSTED (Full)       ',
     &                   Facfadc(1), cpval1, Facfadc(Mq), cpval2
       end if
      else
       Facfadc(1)  = '??'
       Facfadc(Mq) = '??'
      end if
cc
c SA first and seasonal orders (noend data)
cc
      if (nthets .gt. 1) then
       call altUnderOverClass( nstar, Nacfadg(1),  Nacfadp(1),
     &                         Nacfadc(1),  cpval1 )
       call altUnderOverClass( nstar, Nacfadg(Mq), Nacfadp(Mq),
     &                         Nacfadc(Mq), cpval2 )
       if (lOutput) then
        write (Nio, 2000)'ADJUSTED (Noend)      ',
     &                   Nacfadc(1), cpval1, Nacfadc(Mq), cpval2
       end if
      else
       Nacfadc(1)  = '??'
       Nacfadc(Mq) = '??'
      end if
cc
c SA first and seasonal orders (weighted data)
cc
      if (nthets .gt. 1) then
       call altUnderOverClass( nstar, Wacfadg(1),  Wacfadp(1),
     &                         Wacfadc(1),  cpval1 )
       call altUnderOverClass( nstar, Wacfadg(Mq), Wacfadp(Mq),
     &                         Wacfadc(Mq), cpval2 )
       if (lOutput) then
        write (Nio, 2000)'ADJUSTED (Weighted)   ',
     &                   Wacfadc(1), cpval1, Wacfadc(Mq), cpval2
       end if
      else
       Wacfadc(1)  = '??'
       Wacfadc(Mq) = '??'
      end if
cc
cc Output table of class definitions
cc
      if (lOutput) then
       CALL writTag(nio,'</table>')
       CALL mkPOneLine(Nio,'@','&nbsp;')
       CALL writTagClass(Nio,'ul','nob')
       write (Nio,1020)'+&nbsp;+','Too much positive covariance.'//
     $                     ' Strong evidence (p&lt;.0013).'
       write (Nio,1020)'+','Too much positive covariance.'//
     $                     ' Mild evidence (.0013&lt;p&lt;.0227).'
       write (Nio,1020)'-&nbsp;-','Too much negative covariance.'//
     $                     ' Strong evidence (p&lt;.0013).'
       write (Nio,1020)'-','Too much negative covariance.'//
     $                     ' Mild evidence (.0013&lt;p&lt;.0227).'
       if (nstar .gt. 0) write (Nio,1020)'**','unreliable test.'
       CALL writTag(nio,'</ul>')
      end if
cc
cc Output subtest title for Cross-covariance test
cc
      nstar = 0
      if (lOutput) then
       CALL writTagOneLine(Nio,'h4','@','3. CROSSCORRELATION')
       CALL mkTableTag(Nio,'w50','@')
       CALL mkCaption(Nio,'3. CROSSCORRELATION')
       CALL writTag(Nio,'<tr>')
       CALL mkTableCell(Nio,'head','&nbsp;')
       inc=1
       if (Nthets .gt. 1) then
        CALL mkHeaderCellScope(Nio,0,0,'col','@','SEASONAL')
        inc=inc+1
       endif
       CALL mkHeaderCellScope(Nio,0,0,'col','@','IRREGULAR')
       CALL writTag(Nio,'</tr>')
      end if

      if (Nthetp .gt. 1) then
       call altUnderOverClass( nstar, treIrrDia, treIrrDgP,
     &                                     treIrrDgC,  cpval1 )
       CALL writTag(Nio,'<tr>')
       CALL mkHeaderCellScope(Nio,0,0,'row','@','TREND-CYCLE')
       if (Nthets .gt. 1) then
        call altUnderOverClass( nstar, seaTreDia, seaTreDgP,
     &                                      seaTreDgC, cpval2 )
        CALL mkTableCell(Nio,'center',seaTreDgC//' ('//cpval2//')')
       else
        seaTreDgC = '??'
       end if
       CALL mkTableCell(Nio,'center', treIrrDgC//' ('//cpval1//')')
       CALL writTag(Nio,'</tr>')
      else
       treIrrDgC = '??'
       seaTreDgC = '??'
      end if

      if (Nthets .gt. 1) then
       call altUnderOverClass( nstar, seaIrrDia, seaIrrDgP,
     &                                     seaIrrDgC, cpval1 )
       if (lOutput) then
        CALL writTag(Nio,'<tr>')
        CALL mkHeaderCellScope(Nio,0,0,'row','@','IRREGULAR')
        CALL mkTableCell(Nio,'center', seaIrrDgC//' ('//cpval1//')')
        CALL mkTableCell(Nio,'@','&nbsp;')
        IF (inc.gt.1)CALL writTag(Nio,'</tr>')
       end if
      else
       seaIrrDgc = '??'
      end if
cc
cc Output table of class definitions
cc
      if (lOutput) then
       CALL writTag(nio,'</table>')
       CALL mkPOneLine(Nio,'@','&nbsp;')
       CALL writTagClass(Nio,'ul','nob')
       write (Nio,1020)'+&nbsp;+','Too much positive crosscovariance.'//
     $                     ' Strong evidence (p&lt;.0013).'
       write (Nio,1020)'+','Too much positive crosscovariance.'//
     $                     ' Mild evidence (.0013&lt;p&lt;.0227).'
       write (Nio,1020)'-&nbsp;-','Too much negative crosscovariance.'//
     $                     ' Strong evidence (p&lt;.0013).'
       write (Nio,1020)'-','Too much negative crosscovariance.'//
     $                     ' Mild evidence (.0013&lt;p&lt;.0227).'
       if (nstar .gt. 0) write (Nio,1020)'**','unreliable test.'
       CALL writTag(nio,'</ul>')
      end if
      return
      end

c-----------------------------------------------------------------------
c     The following subroutine supports the altUnderOverTest subroutine
c     by determining the class (one of five) of a diagnostic 
c     from its p-value by assuming an underlying asymtotic Gaussian
c     distribution. 
c-----------------------------------------------------------------------
      subroutine altUnderOverClass( nstar, diagnostic, pvalue, uotest,
     &                                            cpval )

c-----------------------------------------------------------------------
c Name   Type Description (Input/Output Variables)
c-----------------------------------------------------------------------
c diagnostic d input diagnostic to be classified
c cpval     c     character version of pvalue
c nstart    i     not used
c pvalue    d     one-sided p-value calculated for diagnostic
c uotest    c     output class from 'ok', '-', '+', '--', '++'
c                  if class not 'ok', then p-value  also included.
c-----------------------------------------------------------------------
      implicit none
      integer nstar
      real*8 diagnostic, pvalue
      character uotest*2, cpval*8

c-----------------------------------------------------------------------
c Name   Type Description (Local Variables)
c-----------------------------------------------------------------------
c gauss     d     external function reference
c ONE       d     constant parameter value
c TWO       d     constant parameter value
c TC1       d     denotes p-value class of .0227
c TC2       d     denotes p-value class of .0013
c ZERO      d     constant parameter value
c-----------------------------------------------------------------------
      double precision gauss
      double precision ZERO, ONE, TWO, TC1, TC2
      parameter ( ZERO = 0.0d0, ONE = 1.0d0, TWO = 2.0d0 )
      parameter ( TC1 = .0227d0, TC2 = .0013d0 )

c-----------------------------------------------------------------------
c     Calculate one-sided p-value.
c-----------------------------------------------------------------------
      pvalue = (ONE - gauss( diagnostic ))/TWO
      
c-----------------------------------------------------------------------
c     Use p-value and sign of diagnostic to classify the diagnostic
c      using .0227 and .0013 class boundaries.
c     If class of p-value not 'ok' then 
c      create character version of p-value
c-----------------------------------------------------------------------
      if ( pvalue .gt. TC1 ) then
       uotest = 'OK'
       cpval = ' &nbsp; '
      else
       write(cpval,1000)pvalue
 1000  format( '(p=', f6.3, ')' )
       if ( pvalue .gt. TC2 ) then
        if ( diagnostic .gt. ZERO ) then
         uotest='+ '
        else
         uotest='- '
        end if
       else
        if ( diagnostic .gt. ZERO ) then
         uotest='++'
        else
         uotest='--'
        end if
       end if
      end if
      end