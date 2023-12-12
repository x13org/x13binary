      subroutine mkHead(Fh,outName,outDesc,cssFull,iDoc,icss,Lindx)
c-----------------------------------------------------------------------
c     Generate top of HTML output
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
c-----------------------------------------------------------------------
      INTEGER Fh,iDoc,icss
      LOGICAL cssFull,Lindx
      CHARACTER outName*(*),outDesc*(*)
c-----------------------------------------------------------------------
      IF(Lxhtml)THEN
       CALL WriteDoctype(Fh,0)
       WRITE(Fh,1010)'<html xmlns="http://www.w3.org/1999/xhtml" '//
     &               'xml:lang="en" lang="en">'
      else
       CALL WriteDoctype(Fh,iDoc)
       WRITE(Fh,1010)'<html lang="en">'
      END IF
      WRITE(Fh,1010)'<head>'
      WRITE(Fh,1020)outName
c-----------------------------------------------------------------------
      CALL mkMetaTag(Fh,'keywords','@',
     &        'seasonal adjustment, '//PRGNAM//', regARIMA modeling',
     &        Lxhtml)
      CALL mkMetaTag(Fh,'description','@',outDesc,Lxhtml)
      CALL mkMetaTag(Fh,'generator','@',PRGNAM,Lxhtml)
      CALL mkMetaTag(Fh,'author','@',
     &             'Time Series Research Staff, CSRM, US Census Bureau',
     &             Lxhtml)
      CALL mkMetaTag(Fh,'@','Content-Type',
     &               'text/html; charset=ISO-8859-1',Lxhtml)
c-----------------------------------------------------------------------
c    Create CSS information
c-----------------------------------------------------------------------
      IF(icss.ge.0)THEN
       call writeCSS(Fh,icss,Lxhtml)
c-----------------------------------------------------------------------
c   End head section of file
c-----------------------------------------------------------------------
       WRITE(Fh,1010)'</head>'
       WRITE(Fh,1010)'<body>'
c-----------------------------------------------------------------------
       RETURN
c-----------------------------------------------------------------------
      END IF
      WRITE(Fh,1010)'<style type="text/css">'
      IF(Lxhtml)WRITE(Fh,1010)'/*<![CDATA[*/'
      IF(Lindx)THEN
       WRITE(Fh,1010)'  #rightnavigation {'
       WRITE(Fh,1010)'   position : absolute;'
       WRITE(Fh,1010)'   top : 0px;'
       WRITE(Fh,1010)'   right : 0;'
       WRITE(Fh,1010)'   width : 300px;'
       WRITE(Fh,1010)'   margin-left : 10px;'
       WRITE(Fh,1010)'   margin-top : 20px;'
       WRITE(Fh,1010)'   color : #000000;'
       WRITE(Fh,1010)'   padding : 3px;'
       WRITE(Fh,1010)'      background-color: #CCFFFF;'
       WRITE(Fh,1010)'   border: 1px solid #666;'
       WRITE(Fh,1010)'  }'
       WRITE(Fh,1010)'  #content{'
       WRITE(Fh,1010)'   top : 0px;'
       WRITE(Fh,1010)'    margin : 0px 350px 0px 0;'
       WRITE(Fh,1010)'    padding : 3px;'
       WRITE(Fh,1010)'    color : #000000;'
       WRITE(Fh,1010)'  }'
      END IF
      WRITE(Fh,1010)
     &   '  p,ul,h1,h2,h3,h4,h5 {margin-left : 5%; margin-right : 5%;}'
      WRITE(Fh,1010)
     &   '  .indent {  margin-left : 7.5%;  margin-right : 7.5%;  }'
      WRITE(Fh,1010)
     &   '  .indentnav {  margin-left : 7.5%;  margin-right : 7.5%; '//
     &   'font-size: 75%; }'
      WRITE(Fh,1010)'  .bold {   font-weight: bold;   }'
      WRITE(Fh,1010)'  .em {   font-style:italic;   }'
      WRITE(Fh,1010)'  .center { text-align : center; }'
      WRITE(Fh,1010)'  .left {   text-align: left;    }'
      WRITE(Fh,1010)'  .right {   text-align: right;    }'
      WRITE(Fh,1010)'  pre {margin-left : 5%; margin-right : 5%;}'
      WRITE(Fh,1010)'  ul.nob { list-style:none }'
      WRITE(Fh,1010)
     &   '  .ub {text-decoration:underline; font-weight: bold; }'
c-----------------------------------------------------------------------
c   Table CSS
c-----------------------------------------------------------------------
      WRITE(Fh,1010)'  table {'
      WRITE(Fh,1010)'    border-width: thin thin thin thin;'
      WRITE(Fh,1010)'    border-spacing: 0px;'
      WRITE(Fh,1010)'    border-color: black black black black;'
      WRITE(Fh,1010)'    border-collapse: collapse;'
      WRITE(Fh,1010)'    background-color: white;'
      WRITE(Fh,1010)'    font-size: 80%;'
      WRITE(Fh,1010)'  }'
      WRITE(Fh,1010)'  th {'
      WRITE(Fh,1010)'    border-width: thin thin thin thin;'
      WRITE(Fh,1010)'    padding: 3px 3px 3px 3px;'
      WRITE(Fh,1010)'    border-style: inset inset inset inset;'
      WRITE(Fh,1010)'    border-color: gray gray gray gray;'
      WRITE(Fh,1010)'    background-color: #BDBDBD;'
      WRITE(Fh,1010)'  }'
      WRITE(Fh,1010)'  td {'
      WRITE(Fh,1010)'    border-width: thin thin thin thin;'
      WRITE(Fh,1010)'    padding: 3px 3px 3px 3px;'
      WRITE(Fh,1010)'    border-style: inset inset inset inset;'
      WRITE(Fh,1010)'    border-color: gray gray gray gray;'
      WRITE(Fh,1010)'    background-color: white;'
      WRITE(Fh,1010)'    text-align: right;'
      WRITE(Fh,1010)'  }'
      WRITE(Fh,1010)'  table.x11 {'
      WRITE(Fh,1010)'    margin-left : 7.5%;'
      WRITE(Fh,1010)'    margin-right : 7.5%;'
      WRITE(Fh,1010)'    width : 85%;'
      WRITE(Fh,1010)'  }'
      WRITE(Fh,1010)'  td.nowrap {  white-space: nowrap;  }'
      WRITE(Fh,1010)'  td.nowrapcenter {'
      WRITE(Fh,1010)'    white-space: nowrap;'
      WRITE(Fh,1010)'    text-align : center;'
      WRITE(Fh,1010)'  }'
      WRITE(Fh,1010)'  td.head {'
      WRITE(Fh,1010)'    background-color: #BDBDBD;'
      WRITE(Fh,1010)'    text-align : center;'
      WRITE(Fh,1010)'    font-weight: bold;'
      WRITE(Fh,1010)'  }'
      WRITE(Fh,1010)'  td.headleft {'
      WRITE(Fh,1010)'    background-color: #BDBDBD;'
      WRITE(Fh,1010)'    text-align : left;'
      WRITE(Fh,1010)'    font-weight: bold;'
      WRITE(Fh,1010)'  }'
      WRITE(Fh,1010)'  table td:hover,th:hover {'
      WRITE(Fh,1010)'    background:#FFCC99; cursor: crosshair;'
      WRITE(Fh,1010)'    font-weight:bold; font-size:1.5em;'
      WRITE(Fh,1010)'  }'
      WRITE(Fh,1030)'w65','17.5%','17.5%','65%'
c-----------------------------------------------------------------------
c   CSS for table classes
c-----------------------------------------------------------------------
      IF(cssFull)THEN
       WRITE(Fh,1010)'  th.iter {  width : 35%;  text-align: left;  }'
       WRITE(Fh,1030)'w90','5%','5%','90%'
       WRITE(Fh,1030)'w80','10%','10%','80%'
       WRITE(Fh,1030)'w70','15%','15%','70%'
       WRITE(Fh,1030)'w60','20%','20%','60%'
       WRITE(Fh,1030)'w50','25%','25%','50%'
       WRITE(Fh,1030)'w40','30%','30%','40%'
       WRITE(Fh,1030)'w30','35%','35%','30%'
       WRITE(Fh,1030)'w20','40%','40%','20%'
c-----------------------------------------------------------------------
c    CSS for links, footnotes
c-----------------------------------------------------------------------
      END IF
      WRITE(Fh,1010)
     &   '  a.skiplinks {color: #FFFFFF; font-size: xx-small;}'
      WRITE(Fh,1010)'  a.longdesc {'
      WRITE(Fh,1010)'    position : absolute; left : -1010em; '
      WRITE(Fh,1010)'    width : 900em; font-size : 10%;'
      WRITE(Fh,1010)'  }'
      WRITE(Fh,1010)'  dl#content-footnotes {'
      WRITE(Fh,1010)'    position : absolute; left : -9999px;'
      WRITE(Fh,1010)'  }'
      WRITE(Fh,1010)'  dt {'
      WRITE(Fh,1010)'    clear : both; padding : .5em 0 0 0;'
      WRITE(Fh,1010)'    margin : 0; font-weight : bold; '
      WRITE(Fh,1010)'    font-size : 10%; '
      WRITE(Fh,1010)'  }'
      WRITE(Fh,1010)'  dd {'
      WRITE(Fh,1010)'    padding : 0; margin : 0; font-size : 10%; '
      WRITE(Fh,1010)'  }'
c-----------------------------------------------------------------------
c   End head section of file
c-----------------------------------------------------------------------
      IF(Lxhtml)WRITE(Fh,1010)'/*]]>*/'
      WRITE(Fh,1010)'</style>'
      WRITE(Fh,1010)'</head>'
      WRITE(Fh,1010)'<body>'
c-----------------------------------------------------------------------
 1010 FORMAT(a)
 1020 FORMAT('<title>',a,'</title>')
 1030 FORMAT(' .',a,' {  margin-left : ',a,';  margin-right : ',a,
     &       ';  width : ',a,'  }')
c-----------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE mkMetaTag(Fh,thisName,thisHttp,thisContent,Lxhtml)
c-----------------------------------------------------------------------
      INTEGER Fh
      LOGICAL Lxhtml
      CHARACTER thisName*(*),thisHttp*(*),thisContent*(*),endtag*(3)
c-----------------------------------------------------------------------
      IF(Lxhtml)THEN
       endtag=' />'
      ELSE
       endtag='>  '
      END IF
c-----------------------------------------------------------------------
      IF(thisHttp(1:1).eq.'@')THEN
       WRITE(Fh,1000)'name',thisName,thisContent,endtag
      ELSE
       WRITE(Fh,1000)'http-equiv',thisHttp,thisContent,endtag
      END IF
c-----------------------------------------------------------------------
 1000 FORMAT('<meta ',a,'="',a,'"  content="',a,'"',a)
c-----------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE mkTableTag(Fh,thisClass,thisSummary)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Generate table tag of a given CSS class
c-----------------------------------------------------------------------
      INTEGER Fh
      CHARACTER thisClass*(*),thisSummary*(*)
c-----------------------------------------------------------------------
      IF(thisSummary(1:1).eq.'@')THEN
       IF(thisClass(1:1).eq.'@')THEN
        WRITE(Fh,1030)
       ELSE
        WRITE(Fh,1000)thisClass
       END IF
      ELSE
       IF(thisClass(1:1).eq.'@')THEN
        WRITE(Fh,1020)thisSummary
       ELSE
        WRITE(Fh,1010)thisClass,thisSummary
       END IF
      END IF
c-----------------------------------------------------------------------
 1000 FORMAT('<table class="',a,'">')
 1010 FORMAT('<table class="',a,'"',/,'  summary ="',a,'" >')
 1020 FORMAT('<table ',/,'  summary ="',a,'" >')
 1030 FORMAT('<table>')
c-----------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      subroutine mkCaption(Fh,thisText)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Generate caption tag of a given CSS class with given text
c-----------------------------------------------------------------------
      INTEGER Fh
      CHARACTER thisText*(*)
c-----------------------------------------------------------------------
      WRITE(Fh,1000)thisText
 1000 FORMAT('<caption><strong>',a,'</strong></caption>')
c-----------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      subroutine mkHeaderCell(Fh,NrSpan,NcSpan,thisAbb,thisText)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Generate header cell tag for a given scope with given text and an
c     abbreviation.
c-----------------------------------------------------------------------
      INTEGER Fh,NrSpan,NcSpan
      CHARACTER thisAbb*(*),thisText*(*)
c-----------------------------------------------------------------------
      IF(Nrspan.gt.0)THEN
       IF(thisAbb(1:1).eq.'@')THEN
        WRITE(Fh,1000)'row',Nrspan,thisText
       ELSE
        WRITE(Fh,1010)'row',Nrspan,thisAbb,thisText
       END IF
      ELSE IF(Ncspan.gt.0)THEN
       IF(thisAbb(1:1).eq.'@')THEN
        WRITE(Fh,1000)'col',Ncspan,thisText
       ELSE
        WRITE(Fh,1010)'col',Ncspan,thisAbb,thisText
       END IF
      ELSE
       IF(thisAbb(1:1).eq.'@')THEN
        CALL writTagOneLine(Fh,'td','@',thisText)
       ELSE
        CALL writTag(Fh,'<td>')
        CALL writAbb(Fh,thisAbb,thisText)
        CALL writTag(Fh,'</td>')
       END IF
      END IF
c-----------------------------------------------------------------------
 1000 FORMAT('<th ',a,'span="',i1,'">',a,'</th>')
 1010 FORMAT('<th ',a,'span="',i1,'"><abbr title="',a,'">',a,
     &       '</abbr></th>')
c-----------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      subroutine mkHeaderCellId(Fh,Nrspan,Ncspan,thisId,thisClass,
     &                          thisAbb,thisText)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Generate header cell tag for a given scope with given text and an
c     abbreviation.
c-----------------------------------------------------------------------
      INTEGER Fh,NrSpan,NcSpan,i1
      CHARACTER thisId*(*),thisClass*(*),thisAbb*(*),thisText*(*)
c-----------------------------------------------------------------------
      IF(thisClass(1:1).eq.'@')THEN
       IF(Nrspan.gt.0)then
        IF(thisAbb(1:1).eq.'@')THEN
         WRITE(Fh,1000)'row',Nrspan,thisId,thisText
        ELSE
         WRITE(Fh,1010)'row',Nrspan,thisId,thisAbb,thisText
        END IF
       ELSE IF (Ncspan.gt.0) then
        IF(thisAbb(1:1).eq.'@')THEN
         WRITE(Fh,1000)'col',Ncspan,thisId,thisText
        ELSE
         WRITE(Fh,1010)'col',Ncspan,thisId,thisAbb,thisText
        END IF
       ELSE
        IF(thisAbb(1:1).eq.'@')THEN
         WRITE(Fh,1020)thisId,thisText
        ELSE
         WRITE(Fh,1030)thisId,thisAbb,thisText
        END IF
       END IF
      ELSE
       IF(Nrspan.gt.0)then
        IF(thisAbb(1:1).eq.'@')THEN
         WRITE(Fh,1100)'row',Nrspan,thisId,thisClass,thisText
        ELSE
         WRITE(Fh,1110)'row',Nrspan,thisId,thisClass,thisAbb,thisText
        END IF
       ELSE IF (Ncspan.gt.0) then
        IF(thisAbb(1:1).eq.'@')THEN
         WRITE(Fh,1100)'col',Ncspan,thisId,thisClass,thisText
        ELSE
         WRITE(Fh,1110)'col',Ncspan,thisId,thisClass,thisAbb,thisText
        END IF
       ELSE
        IF(thisAbb(1:1).eq.'@')THEN
         WRITE(Fh,1120)thisId,thisClass,thisText
        ELSE
         WRITE(Fh,1130)thisId,thisClass,thisAbb,thisText
        END IF
       END IF
      END IF
c-----------------------------------------------------------------------
 1000 FORMAT('<th ',a,'span="',i1,'" id="',a,'">',a,'</th>')
 1010 FORMAT('<th ',a,'span="',i1,'" id="',a,'"><abbr title="',a,'">',a,
     &       '</abbr></th>')
 1020 FORMAT('<th id="',a,'">',a,'</th>')
 1030 FORMAT('<th id="',a,'"><abbr title="',a,'">',a,'</abbr></th>')
 1100 FORMAT('<th ',a,'span="',i1,'" id="',a,'" class="',a,'">',a,
     &       '</th>')
 1110 FORMAT('<th ',a,'span="',i1,'" id="',a,'" class="',a,
     &       '"><abbr title="',a,'">',a,'</abbr></th>')
 1120 FORMAT('<th id="',a,'" class="',a,'">',a,'</th>')
 1130 FORMAT('<th id="',a,'" class="',a,'"><abbr title="',a,'">',a,
     &       '</abbr></th>')
c-----------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      subroutine mkHeaderCellScope(Fh,Nrspan,Ncspan,thisScope,thisAbb,
     &                             thisText)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Generate header cell tag for a given scope with given text and an
c     abbreviation.
c-----------------------------------------------------------------------
      INTEGER Fh,NrSpan,NcSpan,i1
      CHARACTER thisScope*(*),thisAbb*(*),thisText*(*)
c-----------------------------------------------------------------------
      IF(Nrspan.gt.0)then
       IF(thisAbb(1:1).eq.'@')THEN
        WRITE(Fh,1000)'row',Nrspan,thisScope,thisText
       ELSE
        WRITE(Fh,1010)'row',Nrspan,thisScope,thisAbb,thisText
       END IF
      ELSE IF (Ncspan.gt.0) then
       IF(thisAbb(1:1).eq.'@')THEN
        WRITE(Fh,1000)'col',Ncspan,thisScope,thisText
       ELSE
        WRITE(Fh,1010)'col',Ncspan,thisScope,thisAbb,thisText
       END IF
      ELSE
       IF(thisAbb(1:1).eq.'@')THEN
        WRITE(Fh,1020)thisScope,thisText
       ELSE
        WRITE(Fh,1030)thisScope,thisAbb,thisText
       END IF
      END IF
c-----------------------------------------------------------------------
 1000 FORMAT('<th ',a,'span="',i1,'" scope="',a,'">',a,'</th>')
 1010 FORMAT('<th ',a,'span="',i1,'" scope="',a,'"><abbr title="',a,
     &       '">',a,'</abbr></th>')
 1020 FORMAT('<th scope="',a,'">',a,'</th>')
 1030 FORMAT('<th scope="',a,'"><abbr title="',a,'">',a,'</abbr></th>')
c-----------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      subroutine mkTableCell(Fh,thisClass,thisText)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Generate cell tag for a given class with given text 
c-----------------------------------------------------------------------
      INTEGER Fh
      CHARACTER thisClass*(*),thisText*(*)
c-----------------------------------------------------------------------
      IF(thisClass(1:1).eq.'@')THEN
       WRITE(Fh,1000)thisText
      ELSE
       WRITE(Fh,1010)thisClass,thisText
      ENDIF
c-----------------------------------------------------------------------
 1000 FORMAT('<td>',a,'</td>')
 1010 FORMAT('<td class="',a,'">',a,'</td>')
c-----------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      subroutine mkTableCellSpan(Fh,thisSpan,nSpan,thisClass,thisText)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Generate cell tag for a given class with given text 
c-----------------------------------------------------------------------
      INTEGER Fh,nSpan
      CHARACTER thisSpan*(*),thisClass*(*),thisText*(*)
c-----------------------------------------------------------------------
      IF(thisSpan(1:1).eq.'@')THEN
       IF(thisClass(1:1).eq.'@')THEN
        WRITE(Fh,1000)thisText
       ELSE
        WRITE(Fh,1010)thisClass,thisText
       ENDIF
      ELSE
       IF(thisClass(1:1).eq.'@')THEN
        WRITE(Fh,1020)thisSpan,nSpan,thisText
       ELSE
        WRITE(Fh,1030)thisSpan,nSpan,thisClass,thisText
       ENDIF
      END IF
c-----------------------------------------------------------------------
 1000 FORMAT('<td>',a,'</td>')
 1010 FORMAT('<td class="',a,'">',a,'</td>')
 1020 FORMAT('<td ',a,'span="',i1,'">',a,'</td>')
 1030 FORMAT('<td ',a,'span="',i1,'" class="',a,'">',a,'</td>')
c-----------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      subroutine mkTableCellHeader(Fh,thisHeader,thisClass,thisText)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Generate cell tag for a given class with given text 
c-----------------------------------------------------------------------
      INTEGER Fh
      CHARACTER thisHeader*(*),thisClass*(*),thisText*(*)
c-----------------------------------------------------------------------
      IF(thisClass(1:1).eq.'@')THEN
       WRITE(Fh,1000)thisHeader,thisText
      ELSE
       WRITE(Fh,1010)thisHeader,thisClass,thisText
      ENDIF
c-----------------------------------------------------------------------
 1000 FORMAT('<td headers="',a,'">',a,'</td>')
 1010 FORMAT('<td headers="',a,'" class="',a,'">',a,'</td>')
c-----------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      subroutine mkTableCellHeaderSpan(Fh,thisSpan,nSpan,thisHeader,
     &                                 thisClass,thisText)
      IMPLICIT NONE
c-----------------------------------------------------------------------
c     Generate cell tag for a given class with given text 
c-----------------------------------------------------------------------
      INTEGER Fh,nSpan
      CHARACTER thisSpan*(*),thisHeader*(*),thisClass*(*),thisText*(*)
c-----------------------------------------------------------------------
      IF(thisClass(1:1).eq.'@')THEN
       WRITE(Fh,1020)thisSpan,nSpan,thisHeader,thisText
      ELSE
       WRITE(Fh,1030)thisSpan,nSpan,thisClass,thisHeader,thisText
      ENDIF
c-----------------------------------------------------------------------
 1020 FORMAT('<td ',a,'span="',i1,' headers="',a,'">',a,'</td>')
 1030 FORMAT('<td ',a,'span="',i1,'" class="',a,' headers="',a,'">',a,
     &       '</td>')
c-----------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE writTag(Flhdnl,Otag)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INTEGER Flhdnl
      CHARACTER Otag*(*)
c     -----------------------------------------------------------------
      WRITE(Flhdnl,1010)Otag
c-----------------------------------------------------------------------
 1010 FORMAT(a)
c-----------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE mkPClass(Flhdnl,thisClass)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INTEGER Flhdnl,Flhdn2
      CHARACTER thisClass*(*)
c     -----------------------------------------------------------------
      WRITE(Flhdnl,1010)thisClass
c-----------------------------------------------------------------------
 1010 FORMAT(/,'<p class="',a,'">')
c-----------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE writTagClass(Flhdnl,Otag,thisClass)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INTEGER Flhdnl
      CHARACTER Otag*(*),thisClass*(*)
c     -----------------------------------------------------------------
      WRITE(Flhdnl,1010)Otag,thisClass
c-----------------------------------------------------------------------
 1010 FORMAT('<',a,' class="',a,'">')
c-----------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE mkPOneLine(Flhdnl,thisClass,thisText)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INTEGER Flhdnl,Flhdn2
      CHARACTER thisClass*(*),thisText*(*)
c     -----------------------------------------------------------------
      IF(thisClass(1:1).eq.'@')THEN
       WRITE(Flhdnl,1000)thisText
      ELSE
       WRITE(Flhdnl,1010)thisClass,thisText
      END IF
c-----------------------------------------------------------------------
 1000 FORMAT(/,'<p>',a,'</p>')
 1010 FORMAT(/,'<p class="',a,'">',a,'</p>')
c-----------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE writTagOneLine(Flhdnl,Otag,thisClass,thisText)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INTEGER Flhdnl
      CHARACTER Otag*(*),thisClass*(*),thisText*(*)
c     -----------------------------------------------------------------
      IF(thisClass(1:1).eq.'@')THEN
       WRITE(Flhdnl,1000)Otag,thisText,Otag
      ELSE
       WRITE(Flhdnl,1010)Otag,thisClass,thisText,Otag
      END IF
c-----------------------------------------------------------------------
 1000 FORMAT(/,'<',a,'>',a,'</',a,'>')
 1010 FORMAT(/,'<',a,' class="',a,'">',a,'</',a,'>')
c-----------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE makeAnchor(Flhdnl,Itab,Abase)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      CHARACTER Abase*(*)
      INTEGER Flhdnl,Itab
c-----------------------------------------------------------------------
      IF(Itab.lt.0)THEN
       WRITE(Flhdnl,1010)Abase
      ELSE
       WRITE(Flhdnl,1000)Abase,Itab
      END IF
c-----------------------------------------------------------------------
 1000 FORMAT(/,'<a name="',a,i5.5,'"></a>')
 1010 FORMAT(/,'<a name="',a,'"></a>')
c-----------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE makeSkipLink(Flhdnl,Itab,thisText,plotSkip)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      CHARACTER thisText*(*)
      INTEGER Flhdnl,Itab
      LOGICAL plotSkip
c-----------------------------------------------------------------------
      IF(plotSkip)THEN
       WRITE(Flhdnl,1000)'pos',Itab,thisText,'</p>'
      ELSE
       WRITE(Flhdnl,1000)'skip',Itab,'navagation link',' '
       IF(Itab.eq.0)THEN
        WRITE(Flhdnl,1010)Itab+1,thisText
       ELSE
        WRITE(Flhdnl,1020)Itab-1,thisText,Itab,Itab+1,thisText
       END IF
      END IF
c-----------------------------------------------------------------------
 1000 FORMAT(/,'<p class="right"><a href="#',a,i5.5,'" ',
     &         'title="Skip ',a,'" class="skiplinks">',
     &         '&nbsp;</a>',a)
 1010 FORMAT('<a href="#pos',i5.5,'">Next ',a,'</a></p>')
 1020 FORMAT('<a href="#pos',i5.5,'">Previous ',a,'</a> | ',
     &       '<a href="#index',i5.5,'">Index</a> | ',
     &       '<a href="#pos',i5.5,'">Next ',a,'</a></p>')
c-----------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE makeIndexLink(Flhdnl,Itab,thisFile,thisTable,upTab,
     &                         useFile)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      CHARACTER thisFile*(*),thisTable*(*)
      INTEGER Flhdnl,Itab
      LOGICAL upTab,useFile
c-----------------------------------------------------------------------
      IF(Itab.lt.0)THEN
       WRITE(Flhdnl,1020)thisFile,thisTable
      ELSE
       IF(useFile)THEN
        WRITE(Flhdnl,1000)thisFile,Itab,thisTable
       ELSE
        WRITE(Flhdnl,1010)Itab,Itab,thisTable
       END IF
       IF(upTab)Itab=Itab+1
      END IF
c-----------------------------------------------------------------------
 1000 FORMAT('<li><a href="',a,'#pos',i5.5,'">',a,'</a></li>')
 1010 FORMAT('<li><a name="index',i5.5,'" href="#pos',i5.5,'">',a,
     &       '</a></li>')
 1020 FORMAT('<li><a href="',a,'">',a,'</a></li>')
c-----------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE makColgroup(Flhdnl,thisSpan)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INCLUDE 'stdio.i'
c     ------------------------------------------------------------------
      INTEGER Flhdnl,thisSpan
c     ------------------------------------------------------------------
      IF(thisSpan.gt.0)THEN
       IF (Lxhtml) THEN
        WRITE(Flhdnl,1000)thisSpan
       ELSE
        WRITE(Flhdnl,1010)thisSpan
       END IF
      ELSE
       WRITE(Flhdnl,1020)
      END IF
c-----------------------------------------------------------------------
 1000 FORMAT('<colgroup span="',i1,'"></colgroup>')
 1010 FORMAT('<colgroup span=',i1,'></colgroup>')
 1020 FORMAT('<colgroup></colgroup>')
c-----------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE makDivId(Flhdnl,thisId,thisClass)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INTEGER Flhdnl
      CHARACTER thisId*(*),thisClass*(*)
c     ------------------------------------------------------------------
      IF(thisClass(1:1).eq.'@')THEN
       WRITE(Flhdnl,1000)thisId
      ELSE
       WRITE(Flhdnl,1010)thisClass,thisId
      END IF
c-----------------------------------------------------------------------
 1000 FORMAT(/,'<div id="',a,'">')
 1010 FORMAT(/,'<div class="',a,'" id="',a,'">')
c-----------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE prtwkf(Nio,wkf,nwkf,Chdr,Nhdr,thisId,nid)
c-----------------------------------------------------------------------
      implicit none
c-----------------------------------------------------------------------
      CHARACTER Chdr*(32),thisId*(24)
      INTEGER Nio,nwkf,j,i,i2,Nhdr,nid
      DOUBLE PRECISION wkf
      DIMENSION wkf(*)
c-----------------------------------------------------------------------
      CALL makDivId(Nio,thisId(1:nId),'@')
      CALL mkTableTag(Nio,'w80',
     &                'WIENER-KOLMOGOROV FILTER, '//Chdr(1:Nhdr))
      CALL mkCaption(Nio,Chdr(1:Nhdr))
      DO i=1,nwkf,12
       i2=i+11
       if (i2.gt.nwkf) THEN
        CALL writTag(Nio,'<tr>')
        DO j=i,nwkf
         WRITE(Nio,6999)wkf(j)
        END DO
        DO j=nwkf+1,i2
         CALL mkTableCell(Nio,'@','&nbsp;')
        END DO
        CALL writTag(Nio,'</tr>')
       ELSE
        write (Nio,7000) (wkf(j), j = i,i2)
       END IF
      END DO
c-----------------------------------------------------------------------
      CALL writTag(Nio,'</table></div>')
      CALL mkPOneLine(Nio,'@','&nbsp;')
c-----------------------------------------------------------------------
 6999 FORMAT('<td class="nowrap">',F7.4,'</td>')
 7000 FORMAT('<tr>',12('<td class="nowrap">',F7.4,'</td>'),'</tr>')
c-----------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE writtd(Oline,Flhdnl,Lblnk,Lend)
      IMPLICIT NONE
c     -----------------------------------------------------------------
      INCLUDE 'units.cmn'
c     -----------------------------------------------------------------
      INTEGER Flhdnl
      CHARACTER Oline*(*)
      LOGICAL Lblnk,Lend
c     -----------------------------------------------------------------
      IF(Lblnk)THEN
        IF(Flhdnl.eq.Mt1.or.Flhdnl.eq.Mt2)THEN
         WRITE(Flhdnl,1010)'<td>'
        ELSE
         WRITE(Flhdnl,1010)' '
        END IF
      END IF
      WRITE(Flhdnl,1010)Oline
      IF(Flhdnl.eq.Mt1.or.Flhdnl.eq.Mt2)THEN
       IF(Lend)WRITE(Flhdnl,1010)'</td>'
      END IF
c     -----------------------------------------------------------------
 1010 FORMAT('  ',a)
c     -----------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE writAbb(Flhdnl,thisAbb,thisText)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INTEGER Flhdnl
      CHARACTER thisAbb*(*),thisText*(*)
c     -----------------------------------------------------------------
      WRITE(Flhdnl,1010)thisAbb,thisText
c-----------------------------------------------------------------------
 1010 FORMAT('<abbr title="',a,'">',a,'</abbr>')
c-----------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE mkAicRow(Fh,thisHdr,thisText,thisTd,thisTdAbb)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INTEGER Fh,nb
      CHARACTER*(*) thisHdr,thisText,thisTd,thisTdAbb
c-----------------------------------------------------------------------
      INTEGER nblank
      EXTERNAL nblank
c-----------------------------------------------------------------------
      CALL writTag(Fh,'<tr>')
      CALL mkHeaderCellScope(Fh,0,0,'row','@',thisHdr)
      IF(thisTd.eq.'@') THEN
       CALL mkTableCell(Fh,'center',thisText)
      ELSE
       nb=nblank(thisText)
       CALL writTagClass(Fh,'td','center')
       WRITE(Fh,*)thisText
       CALL writAbb(Fh,thisTdAbb,thisTd)
       IF(thisText(nb:nb).eq.'(')THEN
        WRITE(Fh,*)')</td>'
       ELSE
        CALL writTag(Fh,'</td>')
       END IF
      END IF
      CALL writTag(Fh,'</tr>')
c-----------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
C       subroutine mkTableCellAbb(Fh,thisClass,thisText,thisTextAbb)
C       IMPLICIT NONE
C c-----------------------------------------------------------------------
C c     Generate cell tag for a given class with given text 
C c-----------------------------------------------------------------------
C       INTEGER Fh
C       CHARACTER thisClass*(*),thisText*(*)
C c-----------------------------------------------------------------------
C       IF(thisClass(1:1).eq.'@')THEN
C        CALL writTag(Fh,'<td>')
C       ELSE
C        CALL writTagClass(Fh,'td',thisClass)
C       ENDIF
C       CALL writAbb(Fh,thisTextAbb,thisText)
C       CALL writTag(Fh,'</td>')
C c-----------------------------------------------------------------------
C       RETURN
C       END
c-----------------------------------------------------------------------
      SUBROUTINE mkAicRowReal(Fh,thisAIC,thisAICAbb,thisText,
     &                        thisReg,thisRegAbb,thisReal)
      IMPLICIT NONE
c-----------------------------------------------------------------------
      INTEGER Fh
      CHARACTER*(*) thisAIC,thisAICAbb,thisText,thisReg,thisRegAbb
      DOUBLE PRECISION thisReal
c-----------------------------------------------------------------------
      CALL writTag(Fh,'<tr><th scope="row">')
      CALL writAbb(Fh,thisAICAbb,thisAIC)
      IF(thisRegAbb.eq.'@')THEN
       WRITE(Fh,*)' ',thisText,' ',thisReg
      ELSE
       WRITE(Fh,*)' ',thisText
       CALL writAbb(Fh,thisRegAbb,thisReg)
      END IF
      WRITE(Fh,*)')</th>'
      WRITE(Fh,1010)thisReal
      CALL writTag(Fh,'</tr>')
c-----------------------------------------------------------------------
 1010 FORMAT('<td>',f15.4,'</td>')
c-----------------------------------------------------------------------
      RETURN
      END
c-----------------------------------------------------------------------
