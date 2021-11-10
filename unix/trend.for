      Program TREND
 
************************************************************************
*                                                                      *
*                       Tabular                                        *
*                       Representation of                              *
*                       Evaluated                                      *
*                       Nuclear                                        *
*                       Data                                           *
*                                                                      *
************************************************************************
 
************************************************************************
*  Generates ENSDF data tables report. Allows the user to view and     *
*     control the output file on the screen.                           *
************************************************************************
 
************************************************************************
*                                                                      *
*  Programmer:          Robert R. Kinsey                               *
*  Versions 1 to 2 by:  Bruce J. Barton                                *
*                                                                      *
*  Please direct any questions or comments to:                         *
*  Installation:        National Nuclear Data Center                   *
*                       Brookhaven National Laboratory                 *
*                       Bldg. 197D                                     *
*                       Upton, New York  11973                         *
*                       (516) 344-2902                                 *
*                                                                      *
*  Revision history:                                                   *
*                                                                      *
*   1(00) 17-Nov-83. First release (Fortran-77 standard).              *
*   1(01)  1-Aug-84. Close unit=30, with status='delete'.              *
*   1(02)  1-Aug-84. Some computers blank out text skipped over by T   *
*                    format. Fix things so that all tabs are forward.  *
*   1(03)  2-Aug-84. Clean up main program read/write statements.      *
*   2(04)  2-Aug-84. New style of footnote comments ($ delimits SYM    *
*                    from text).                                       *
*   2(05)  7-Nov-84. Fix 1(1): Move Close (unit=30... to main program. *
*   3(06) 23-Apr-85. Make program VAX compatible.                      *
*   4(07) 19-Jun-85. Major overhaul.                                   *
*   4(10) 25-Jun-85. Check for and delete any existing scratch files   *
*                    before first call to READDS.                      *
*   5(01)  1-May-87. Modified to output to terminals and disk files.   *
*   5(02)  7-Jul-87. Footnote symbol corrected ($>&).                  *
*   5(03)  6-Nov-87. VAX MDC READONLY added to OPEN  for input file.   *
*   5(04) 15-Jul-88. Minor format change for online use etc            *
*   6.00   4-Oct-89. Publication Normalization card and continuation   *
*                    cards for DSID processing added to the program.   *
*   6.01   8-Nov-89. Minor bug in subroutine RPT having to be with     *
*                    presentation change of XREF's.                    *
*   6.02   5-MAR-90. Test of lincnt before TBLHDR called to determine  *
*                    if the page needs to be advanced first.           *
*   6.03   7-MAR-90. PNPRO routine in standard Fortran 77.             *
*   6.04   9-APR-90. All variables declared and all machine independent*
*                    code in standard Fortran 77.                      *
*   6.05  18-APR-90. Corrected code for two instances where transfers  *
*                    into IF/ELSE/ENDIF blocks where made.             *
*   6.06  26-APR-90. Removed DEC code and added AST (Atari 1040ST) code.
*                    File handling of temporary files changed          *
*                    extensively to allow operation within the         *
*                    limitations of personnel computers. This version  *
*                    will compile and load with Microsoft Fortran V4.0 *
*                    on an IBM/PC but will not run properly (hangs).   *
*                    Run date stamping added for IBM/PC(Microsoft      *
*                    Fortran) and Atari 1040ST(Prospero Fortran).      *
*   6.07  19-JUN-90. Minor corrections to improve consistency between  *
*                    various computers in spite of real number roundoff*
*                    problems. IBM/PC Microsoft Fortran V5.0 now       *
*                    can be used to create a correct executable.       *
*   6.08  11-Sep-90. XREF field expanded from 40 characters to 70.     *
*   6.1    9-Jul-91. XREF field display modified to keep a datasets    *
*                    X flag in a fixed column. Uncertain placement     *
*                    footnote flag is now output in the radiation      *
*                    energy field. Column footnote for normalization   *
*                    is now output only if there is a normalization    *
*                    card in the dataset.                              *
*   6.11   3-Sep-91. Incorporate Pub comment and Update comment cards  *
*   6.12   9-Oct-92. Protected code against blank CN comments and blank*
*                    PN cards. PC version bombs if 0 is used in        *
*                    specifying the ending of a string and Lenstr      *
*                    returns 0 for a blank string.                     *
*   6.13  13-Apr-93. Delinted using FLINT 2.83                         *
*                    Removed extraneous <LF> on prompt for IPC         *
*                    Accounted for BACKSPACE at EOF in MS FORTRAN 5.0  *
*                    Worked around sporadic MS FORTRAN problem with    *
*                      BACKSPACEing after EOF found.                   *
*                    (TWB)                                             *
*   6.13a 19-May-93. Corrected initialization problem of flgstr in
*                    subroutine RPT (TWB)
*   6.13b 06-Aug-93. Corrected minor problem noted by Alpha FORTRAN
*                    compiler (RRK)
*   6.14  29-Nov-93. Footnotes used on multiple column headings were
*                    suppressed if the first column it was used on did
*                    not exist. A problem only for N card generated
*                    footnotes of EC Decay datasets. (RRK)
*   6.20  22-Oct-96. Illegal column footnote when given as the final   *
*                    table comment caused the program to bomb for gamma*
*                    tables with intensity flags given.                *
*   7.00  31-Mar-99. Uncertainties truncated in halflife field         *
*   8.00  17-Jun-99. Y2K and removes prints codes from output.         *
*   8.01  22-Jun-99. Patches applied (TWB):                            *
*                    1. All "t" comments were being omitted from output*
*                    2. Not properly taking into account wrapping on   *
*                      "+" continuations for table comments            *
*                    3. Outputting inappropriate blank lines for table *
*                      comments                                        *
*                    4. Minor cleanup in RemovePrints                  *
*   8.02  05-Jul-99. Patches applied (TWB):                            *
*                    1. Attempt to distinguish between pre- and        *
*                      post-Y2K                                        *
*                    2. Output Q comments with symbol and without      *
*                      symbol in the same form                         *
*   8.03  13-Jul-99  3. Corrected logic error when "$" in col. 10 of an*
*                      N record                                        *
*   8.05  16-Jul-99  4. Went into an infinite loop if PN record was    *
*                      completely blank                                *
*   8.10  15-Oct-99  Corrected display problem on PC. DVF will blank a *
*                    line of text if 80 characters are written. We must*
*                    stop at 79. Added command-line operation to DVF   *
*                    version.                                          *
*   8.15  11-Feb-00  Normalizations with no uncertainty were only      *
*                    output to three places past the decimal point.    *
*   8.20  12-Apr-00  Error in comment tables corrected. (It caused     *
*                    to be skipped.) Normalization comments are now    *
*                    as intended. (Mistype)                            *
*   8.3    7-Feb-01  Added Unix coding changes.                        *
*                                                                      *
************************************************************************
 
*  Parameters.
 
C     Files used by sort routine.
      Integer           filbas,      filelo,      filehi
      Parameter        (filbas = 30, filelo = 31, filehi = 36)
 
      Integer           cardl, carda, cardd, cardb, carde, cardg
      Parameter        (cardl = 1, carda = 2, cardd = 3)
      Parameter        (cardb = 4, carde = 5, cardg = 6)
 
*  Common blocks.
 
      Integer           filcnt(filelo:filehi)
      Common   /filcnt/ filcnt
      Integer           nclmn
      Common   /column/ nclmn
      Integer           lincnt,linpag
      Common   /lincnt/ lincnt,linpag
 
*  Local variables.
 
      Integer           I,ICARD,isw
      Logical           eof
      Character*60      innam, outnam
      Integer           icrd
      Integer           iunit
      Integer           flinpg
      Character*1       char
      Logical           outfil
      Logical           newfil

C+++MDC+++
C...VAX
C/      Integer           ISX,ISC,ISL
C...VAX, DVF
C/      Character*11      pdate
C/      Integer*2         ILENT
C/      Character*80      filnam
C/      Character*10      col
C...UNX
      Integer iymd(3)
C---MDC---
      Integer*2         iy,im,id
      Character*3       mon(12)
      Character*15      vers,verdat
      Data mon/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep',
     +   'Oct','Nov','Dec'/
      Data vers/'Version 8.3 '/
      Data verdat/'7-Feb-2001'/
 
************************************************************************
 
*  Begin main program
 
      nclmn=0
      linpag=0
      isw=0
!     Open input and output files.
      innam = ' '
      outnam = ' '
 
C+++MDC+++
C...VAX
C/      Call lib$get_foreign(filnam,,ilent)
C/      If(ilent.lt.1..or.ilent.gt.len(filnam)) then
C/         isw=0
C/      Else
C/         isw = index (filnam(1:ilent), ' ')
C/      Endif
C/      If (isw .le. 1)  goto 6
C/
C/!     Noninteractive input for VAX
C/      innam = filnam(1:isw-1)
C/      isx=index(filnam(isw+1:ilent),' ')+isw
C/      If(isx.le.isw) isx=ilent+1
C/      outnam = filnam(isw+1:isx-1)
C/      If(outnam.eq.'TT:'.or.outnam.eq.'tt:') then
C/         outnam='TT.DAT'
C/      End if
C/      newfil=.true.
C/      If(isx.gt.ilent) then
C/         nclmn=80
C/         linpag=23
C/      Else
C/         isc=index(filnam(isx+1:ilent),' ')+isx
C/         If(isc.le.isx) isc=ilent+1
C/         col=filnam(isx+1:isc-1)
C/         Read(unit=col,fmt='(BN,I)') nclmn
C/         If(nclmn.ne.80.and.nclmn.ne.132) nclmn=80
C/         If(isc.gt.ilent) then
C/            linpag=23
C/         Else
C/            isl=index(filnam(isc+1:ilent),' ')+isc
C/            If(isl.le.isc) isl=ilent+1
C/            col=filnam(isc+1:isl-1)
C/            Read(unit=col,fmt='(BN,I)') linpag
C/         End If
C/      End If
C/      Open  (unit=20, file=innam, status='OLD',READONLY)
C/      Go to 7
C...DVF
C/      Call Getarg(1,filnam,ilent)
C/      If(ilent.lt.1)  goto 6
C/!     Noninteractive input for VAX
C/      innam = filnam
C/      newfil=.true.
C/	Call Getarg(2,filnam,ilent)
C/	If(ilent.lt.1) then
C/	   outnam='tt.dat'
C/	   nclmn=80
C/	   linpag=23
C/         Open  (unit=20, file=innam, status='OLD',READONLY)
C/	   Go to 7
C/	Else
C/	   outnam = filnam
C/         If(outnam.eq.'TT:'.or.outnam.eq.'tt:') then
C/            outnam='TT.DAT'
C/         End if
C/	Endif
C/	Call Getarg(3,col,ilent)
C/      If(ilent.lt.1) then
C/         nclmn=80
C/         linpag=23
C/      Else
C/         Read(unit=col,fmt='(BN,I)') nclmn
C/         If(nclmn.ne.80.and.nclmn.ne.132) nclmn=80
C/	   Call Getarg(4,col,ilent)
C/         If(ilent.lt.1) then
C/            linpag=23
C/         Else
C/            Read(unit=col,fmt='(BN,I)') linpag
C/         End If
C/      End If
C/      Open  (unit=20, file=innam, status='OLD',READONLY)
C/      Go to 7
C...VAX, DVF, ANS, UNX
*     Interactive dialog for VAX and Digital Visual Fortran
    6 Continue
      Write (unit=6,fmt=900) vers,verdat
  900 FORMAT(/21X,'Execution of the Tabular Representation',/
     *       21x,'   of Evaluated Nuclear Data program'
     */34X,A/32X,'  of ',A)
C...VAX, DVF
C/      CALL DATE_AND_TIME(PDATE)
C/      Read(pdate,'(I4,I2,I2)') iy,im,id
C...UNX
      CALL IDATE(IYMD)
      IY=IYMD(3)
      IM=IYMD(2)
      ID=IYMD(1)
C...ANS
C/      IY=1900
C/      IM=1
C/      ID=1
C...VAX, DVF, ANS, UNX
      Write (unit=6,
     *fmt='(32X,''run on '',I2,''-'',A3,''-'',I4/)') id,mon(im),iy
      Write (unit=6, fmt=1) 'Input data set file name: '
      Read  (unit=5, fmt=5) innam
      If(innam.eq.'  ') then
C        May just want to view a previously generated file.
         Write(unit=6, fmt=1) 'Give report file name: '
         newfil = .FALSE.
         Read  (unit=5, fmt=5) outnam
         IF(OUTNAM.EQ.' ') STOP
         Go to 801
      Endif
C...VAX, DVF
C/      Open  (unit=20, file=innam, status='OLD',READONLY)
C...ANS, UNX
      Open  (unit=20, file=innam, status='OLD')
C...VAX, DVF, ANS, UNX
      Write (unit=6, fmt=1) 'Output report file name:  '
      newfil = .TRUE.
      Read  (unit=5, fmt=5) outnam
      If(outnam.eq.'TT:'.or.outnam.eq.'tt:'.or.
     +   outnam.eq.'TTY:'.or.outnam.eq.'tty:') then
         outnam='TT.DAT'
      Else
         INQUIRE(FILE=outnam, EXIST=outfil)
         IF (outfil) THEN
*           -- If the output file exists, check to see if the user
*           -- wants to view that file.
  4         WRITE(UNIT=6, FMT=907)
            READ(UNIT=5,FMT='(A)') CHAR
            IF (CHAR .NE. 'Y' .AND. CHAR .NE. 'y' .AND. CHAR .NE. 'N'
     +     .AND. CHAR .NE. 'n') GOTO 4
            IF (CHAR .EQ. 'Y' .OR. CHAR .EQ. 'y') THEN
*              -- Go to print routine
               newfil = .FALSE.
            END IF
         END IF
      End if
    1 Format(1X,A)
    5 Format(A)
  907 FORMAT(' Do you want to view the output file that already',
     +' exists (Y or N)? ')
 
* Ask whether to format for 80 or 132 column display if undefined
 
  801 Continue
      Write (unit=6, fmt=1)
     +'Format output for 80 or 132 column display (80 or 132)?'
      Read (unit=5,fmt='(I3)') nclmn
      If (nclmn.eq.80) then
         flinpg=60
      Else If (nclmn.eq.132) then
         flinpg=60
      Else
         Go to 801
      Endif
      Write (unit=6, fmt=
     +'('' Format output for how many lines per page (CR='',I2,'')?'')')
     + flinpg
      Read (unit=5,fmt='(I3)') linpag
      If (linpag.le.0) linpag=flinpg
C...VAX, DVF
C/    7 Continue
C/      Open  (unit=21, file=outnam, status='UNKNOWN',
C/     *   carriagecontrol='LIST')
C...ANS, UNX
      Open  (unit=21, file=outnam, status='UNKNOWN')
C---MDC---
      If(.not.newfil) Go to 35
 
* Begin processing the input data set file.
 
      Write (unit=6, fmt=1) 'Processing...'
 
*  Read the first data set, if there.
*  This will create temporary files from which the report will be made.
*  Open temp files.
 
      Do 9 i = filelo,filehi+4
*        -- Temporary files.
         Open (unit=i,status='SCRATCH')
    9 Continue
      Call readds(eof)
*  Produce report for each card type which is present.
*  Always report for L card.
*  Then read next data set until there are no more.
   10 If (.not. eof) then
*        -- Always process L card file.
         Call rpt(cardl)
*        -- If data cards exist for any other card type...
         Do 20 icard = carda, cardg
            icrd=icard
*           -- Check and process each card type.
            iunit = filbas + icrd
            If (filcnt(iunit) .gt. 0) Call rpt(icrd)
   20    Continue
*        -- Read next data set.
         Call readds(eof)
         Goto 10
      End if
 
*  Close input and output files.
      Close (unit=20, status='KEEP')
      If(lincnt+1.le.linpag-1.and.linpag.lt.99) then
         Do 30 i=lincnt+1,linpag-1
            Write(unit=21,fmt='(1X)')
   30    Continue
      Endif
      Write(UNIT=21,FMT='(''[END OF TREND LISTING] '',A,1X,A)')
     2  vers,verdat
      Close (unit=21, status='KEEP')
      Write(unit=6,fmt=1) 'Processing Complete.'
      If(isw.gt.1) Stop
   35 Call prnout(newfil,outnam)
      Stop
      End
* end of main program
 
 
      Subroutine prnout (newfil,outnam)
***********************************************************************
* Creates the screen report. Allows the user to control the output.   *
***********************************************************************
 
*  Subroutine arguments
 
      Logical           newfil
      Character*(*)     outnam
 
*  Common blocks
 
      Integer           nclmn
      Common   /column/ nclmn
      Integer           lincnt,linpag
      Common   /lincnt/ lincnt,linpag
 
      INTEGER           COUNT
      COMMON            COUNT
 
      LOGICAL           QUIT
      COMMON            QUIT
 
*  Functions
      Integer LENSTR
      External LENSTR
 
      Integer INDEX
*  Local variables
 
      CHARACTER*140     LINE
      INTEGER           indxx,LN,I
      CHARACTER*1       INCHAR
      Logical           eoffnd
      Save
 
*  Set appropriate screen size and format statements
 
C+++MDC+++
C...ANS
   3  FORMAT(' ***[ EOF encountered   1=PREVIOUS SCREEN   2=TOP OF'
     +,' FILE   RETURN=DONE ]*** ')
C...VAX, DVF, UNX
C/   3  FORMAT(' ***[ EOF encountered   1=PREVIOUS SCREEN   2=TOP OF'
C/     +,' FILE   RETURN=DONE ]*** ',$)
C---MDC---
 
   4  FORMAT(A)
   5  FORMAT(' ',A)
 
***********************************************************************
 
      IF (.NOT. NEWFIL) GOTO 50
      IF(OUTNAM.EQ.'TT.DAT') GO TO 50
C     Assume that if it is a new file, not directed to TT:, and
C     formatted for more than a screen display then we don't want to
C     view it.
      IF(linpag.GT.24) Stop
*        -- begin printing output file on screen
  50  OPEN(UNIT=21, STATUS='OLD', FILE=outnam)
  51  COUNT=0
*              -- clear the screen
C     ANSI TERMINAL (VT100 EQUIVALENT)
C+++MDC+++
C...ANS
C/      WRITE(UNIT=6,FMT=*) ' '//CHAR(27)//'[2J'//CHAR(27)//'[0;0H'
C...VAX, DVF, UNX
      WRITE(UNIT=6,FMT='(A,$)')'+'//CHAR(27)//'[2J'//CHAR(27)//'[0;0H'
C---MDC---
 
      eoffnd=.FALSE.
      quit=.false.
      READ(UNIT=21, FMT='(A)', END=90) LINE
*     -- read first card before entering loop
  60  Continue
      ln=lenstr(line)
      If(ln.le.0) then
         ln=1
         line=' '
      End If
      WRITE(UNIT=6,FMT=5) LINE(1:ln)
      CALL LNCTRL(linpag)
      IF (.NOT. QUIT) THEN
         READ(UNIT=21, FMT='(A)', END=65) LINE
         If(INDEX(line,'[END OF TREND LISTING]') .GT. 0)Goto 70
         GOTO 60
65       eoffnd=.TRUE.
   70    Continue
         IF(COUNT.GE.LINPAG) GO TO 72
         DO 71 I=COUNT+1,LINPAG
   71    WRITE(UNIT=6,FMT='(1X)')
   72    WRITE(UNIT=6,FMT=3)
         READ 4, INCHAR
         IF (INCHAR .EQ. '1') THEN
            If(eoffnd)Then
               eoffnd=.FALSE.
            Else
               Backspace(UNIT=21)
            Endif
            DO 80 indxx = 1, linpag+COUNT
               BACKSPACE(UNIT=21)
  80        CONTINUE
            GOTO 51
         ELSE IF(INCHAR.EQ.'2') THEN
            If(eoffnd)eoffnd=.FALSE.
            REWIND 21
            GOTO 51
         END IF
      END IF
  90  CLOSE(UNIT=21)
      END
 
 
      Subroutine LNCTRL (linpag)
***********************************************************************
*  Maintains a count of lines written to screen and handles screen    *
*  control commands.                                                  *
***********************************************************************
 
*  Subroutine arguments
 
      INTEGER              linpag
 
*  Common blocks
 
      INTEGER              COUNT
      COMMON               COUNT
 
      LOGICAL              QUIT
      COMMON               QUIT
*
* Local Variables
 
      CHARACTER*1          CH
      INTEGER              LNBACK,INDX
 
* Format statements
 
C+++MDC+++
C...ANS
C/ 993  FORMAT(' ***[ RETURN=NEXT SCREEN   1=PREVIOUS SCREEN   2=TOP OF'
C/     +,' FILE   3=QUIT ]*** ')
C...VAX, DVF, UNX
 993  FORMAT(' ***[ RETURN=NEXT SCREEN   1=PREVIOUS SCREEN   2=TOP OF'
     +,' FILE   3=QUIT ]*** ',$)
C---MDC---
 
 994  FORMAT(A1)
 995  FORMAT(' INPUT ERROR, TRY AGAIN ')
 
**********************************************************************
 
      COUNT = COUNT + 1
      IF (COUNT .GE. linpag) THEN
         WRITE(UNIT=6,FMT=993)
         COUNT = 0
 100     READ 994, CH
*        -- clear screen
C+++MDC+++
C...ANS
C/      WRITE(UNIT=6,FMT=*) ' '//CHAR(27)//'[2J'//CHAR(27)//'[0;0H'
C...VAX, DVF, UNX
      WRITE(UNIT=6,FMT='(A,$)')'+'//CHAR(27)//'[2J'//CHAR(27)//'[0;0H'
C---MDC---
         IF (CH .EQ. '2') THEN
            REWIND(UNIT=21)
         ELSEIF (CH .EQ. '1') THEN
            LNBACK = 2 * linpag
            DO 110 INDX = 1, LNBACK
               BACKSPACE(UNIT=21)
 110        CONTINUE
         ELSEIF (CH .EQ. '3') THEN
            QUIT = .TRUE.
         ELSEIF (CH .NE. ' ') THEN
                WRITE(UNIT=6,FMT=995)
                GOTO 100
         END IF
      END IF
      RETURN
      END
 
 
      Subroutine READDS (eof)
 
************************************************************************
*  Read the next data set.  If none exist, set eof true, else false.   *
*                                                                      *
*  Break the data set into pieces:                                     *
*                                                                      *
*     DSID card is saved in common.                                    *
*                                                                      *
*     L cards into unit 31. Save level energy for other card types.    *
*        <type><energy><seq><level energy><text>                       *
*        < I4 >< F9.3 >< I5><     A13    >< A80>                       *
*           1             Q card and its comments (L file only).       *
*           2             General comments for all record types.       *
*           3             General comments for this record type.       *
*           4             P card comments.                             *
*          10             N card comments, no field name (rad. files). *
*          11             N card comments, BR field (rad. files).      *
*          12             N card comments, NR, NT field (G file only). *
*          13             N card comments, NB field (B, E files only). *
*          14             N card comments, NP field (D file only).     *
*        1000+ichar(flag) Cross reference cards. (L file only).        *
*        2000             data cards (incl: comments and contin's).    *
*        3000+fn code     Footnotes.                                   *
*                                                                      *
*     A cards into unit 32. Same format as unit 31.                    *
*     D cards into unit 33. Same format as unit 31.                    *
*     B cards into unit 34. Same format as unit 31.                    *
*     E cards into unit 35. Same format as unit 31.                    *
*     G cards into unit 36. Same format as unit 31.                    *
*                                                                      *
*     N card is evaluated and footnotes generated and stored in proper *
*        files.                                                        *
*                                                                      *
*     P card is used to compute b-, b+, and ec energies if not given.  *
*                                                                      *
*     For unplaced data cards, ELEV will be blank.                     *
*     The first unplaced data card will cause a footnote (# 41) to be  *
*        generated and stored in the appropriate file.                 *
*                                                                      *
*  Primary data cards are stored with the proper sequence number.      *
*  If a FLAG field is found on a continuation card a special card is   *
*     written with the FLAG field only and given a sequence number of  *
*     pseq + 1 (i.e., it immediately follows the primary card).        *
*  The sequence number is offset by 1 for primary data cards to leave  *
*     room for the FLAG card.                                          *
*  All other comments and continuations therefore follow the FLAG card *
*     (if present).                                                    *
*  To check the existance of a FLAG card check if the next card has    *
*     sequence = pseq + 1 (=> FLAG) or sequence > pseq + 1 (=> no FLAG)*
*  If a continuation card contains more than the FLAG field, the FLAG  *
*     field is removed and the card is written as a normal continuation*
*     card.                                                            *
************************************************************************
 
*  Procedure arguments.
 
      Logical           eof
 
*  Function references.
 
      Integer           break,INDEXF
      Integer           krdtyp
      Integer           lenstr
      Integer           span
      Real              valstr
      Logical           comqpl
      External          break,INDEXF,krdtyp,lenstr,span,valstr,comqpl
 
 
*  Parameters.
 
      Integer           cardl, carda, cardd, cardb, carde, cardg
      Parameter        (cardl =  1, carda =  2, cardd =  3)
      Parameter        (cardb =  4, carde =  5, cardg =  6)
      Integer           cardi, cardn, cardp, cardq, cardx, cardr
      Parameter        (cardi = 10, cardn = 11, cardp = 12)
      Parameter        (cardq = 13, cardx = 14, cardr = 15)
      Integer           filbas,      filelo,      filehi
      Parameter        (filbas = 30, filelo = 31, filehi = 36)
      Integer           nfld, ncrd
      Parameter        (nfld = 8, ncrd = 6)
 
*  Common blocks.
 
      Logical           adopt
*                          True if data set is adopted.
*                          Used to not sort adopted gammas.
      Integer           gamord
*                          Gamord flag from PN card.
      Integer           col1, col2
*                          Ending column numbers for Eg and E(level).
*                          Used to reorder Eg and E(level) columns.
      Common   /adgam/  adopt, gamord, col1, col2
 
      Integer           filcnt(filelo:filehi)
      Common   /filcnt/ filcnt
 
      Character*11      fldnam(nfld, ncrd)
      Common   /fldnam/ fldnam
 
      Integer           fldwid(nfld, ncrd, 3)
      Common   /fldwid/ fldwid
 
      Character*80      idcard,idcont(20)
      Common   /idcard/ idcard,idcont
      Integer  ncont
      Common   /idcont/ ncont
 
      Logical           fldxst(nfld, ncrd)
      Common   /fldxst/ fldxst
 
      Integer           nxcrd
      Character*1       xflg(50)
      Common/  flgord  /nxcrd, xflg
 
*  Local variables.
 
      Integer           IBGN, IEND, NCODE, IFIELD, IFILE
      Character*80      card
      Real              e, de
*                          Energy field value from card.
      Real              el, del
*                          Energy field value from last L card.
      Real              ep, dep
*                          Energy field value from P card.
      Character*13      elev
*                          Text from energy field from last L card.
      Character*80      fcard
*                          Temporary card for holding flag data.
      Integer           flen(nfld-1)
*                          Lengths of output fields from first card.
      Character*70      flgstr
*                          Flags from flag card.
      Logical           header
*                          True if no L, A, D, B, E, or G card seen.
      Integer           i, j
      Integer           icrd
*                          Card type code.
      Integer           ifld
*                          Field type code.
      Integer           pseq
*                          Sequence number of current primary data card.
      Real              qp, dqp
*                          Value of QP field from P card.
      Integer           seq
*                          Sequence number of card within data set.
      Integer           nseq
*                          Sequence number of ncard.
      Character*12      temp
*                          Temp area to format E and DE for B, E cards.
      Integer           type
*                          Record type in temp files (see above).
      Character*80      unpl
*                          Card image for unplaced footnote.
      Logical           quespl(2:6)
*                          Logical flag to indicate whether uncertain
*                             placement has been output or not.
 
*  Format statements.
 
    1 Format(I4, F9.3, I5, A13, A80)
 
************************************************************************
 
 
*  Initialize those variables that need it.
 
      Do 10 i = filelo, filehi
         filcnt(i) = 0
         Rewind i
   10 Continue
      eof = .false.
      header = .true.
      Do 11 i=2,6
         quespl(i)=.false.
   11 Continue
      seq = 0
      nseq=0
      el = 0.0
      elev = ' '
*     -- Reset unplaced footnote card.  unpl(1:5) = ' ' => not yet.
      unpl = ' '
      unpl(20:80) = 'Not placed in level scheme'
*     -- Initialize the field tables.
      Call inifld
*     -- Initialize the footnote tables.
      Call footnt
*     Initialize the X card common
      nxcrd=0
      Do 12 i=1,50
         xflg(i)=' '
12    Continue
 
 
*  Read cards and write each to its proper place(s).
 
   20 Read (unit=20, fmt='(A80)', end=99) card
   21 seq = seq + 1
      If (card .eq. ' ') then
         If (seq .eq. 1) then
*           -- Ignore leading blank records.
            seq = 0
            Goto 20
         Else
*           -- End of data set, go to do sorting.
            Goto 30
         End if
      End if
      icrd = krdtyp(card)
      If (icrd .eq. 0) then
*        -- Invalid card type, ignore it.
      Else if (card(7:7) .eq. 'D'.or.card(7:7) .eq. 'd') then
*        -- Documentation comment, ignore it.
      Else if (icrd .eq. cardi) then
*        -- DSID card.
         idcard = card
         ncont=0
         adopt = .false.
         gamord=3
         If (card(10:14) .eq. 'ADOPT') adopt = .true.
         Write (unit=6, fmt='(1X,A)') card
*        -- Let user know our progress.
      Else if (abs(icrd) .eq. cardq) then
*        -- Q card or Q comments.
         Write (unit=filbas+cardl, fmt=1) 1, 0.0, seq, ' ', card
      Else if (icrd .eq. -cardi) then
         If (card(7:9).eq.' ') then
*        -- DSID continuation
            ncont=ncont+1
            idcont(ncont) = card
         Else
*        -- General comment or reference card, write to all files.
            Do 22 i = filelo, filehi
               Write (unit=i, fmt=1) 2, 0.0, seq, ' ', card
   22       Continue
         End if
      Else if (icrd .eq. cardp) then
*        -- P card (save qp and ep plus uncerts).
         Call cnvs2u(card(10:19), card(20:21), ep, dep)
         Call cnvs2u(card(65:74), card(75:76), qp, dqp)
      Else if (icrd .eq. -cardp) then
*        -- P card comments.
         Do 23 i = filelo, filehi
            Write (unit=i, fmt=1) 4, 0.0, seq, ' ', card
   23    Continue
      Else if (icrd .eq. cardn) then
         Call ncard(seq, card)
         nseq=seq
         seq=seq+1
      Else if (icrd .eq. -cardn) then
*        -- N card comments.
*        -- N card (special processing for footnotes). Also added PN
*           card processing as of 9/8/89.
         If (card(6:8).eq.' PN') then
            gamord=Ichar(card(78:78))-Ichar('0')
            If(card(10:) .EQ. ' ')Then
               gamord=Ichar('3')-Ichar('0')
               GoTo 20
            EndIf
            Call PNPRO(20,0,card)
            GO TO 21
         EndIf
        If (card(6:6) .eq. ' ' ) then
         i = index(card, '$') - 1
         If (i .lt. 0) then
            If (card(10:19) .eq. ' ' .or. card(19:19) .ne. ' ' ) then
*              -- Comment for whole card.
               i = 0
            Else
*              -- Old comment card format.
               i = 19
            End if
         End if
*        -- Check for each field name and put comments in proper files.
         If (i .eq. 0) then
*           -- Comment for whole card.
            Do 24 i = carda, cardg
               Write (unit=filbas+i, fmt=1) 10, 0.0, seq, ' ', card
   24       Continue
            ibgn=carda
            iend=cardg
            ncode=10
         Else if (index(card(10:i), 'BR') .gt. 0) then
            Do 124 i = carda, cardg
               Write (unit=filbas+i, fmt=1) 11, 0.0, seq, ' ', card
  124       Continue
            ibgn=carda
            iend=cardg
            ncode=11
         Else if (index(card(10:i), 'NR') .gt. 0) then
            Write (unit=filbas+cardg, fmt=1) 12, 0.0, seq, ' ', card
            ibgn=cardg
            iend=cardg
            ncode=12
         Else if (index(card(10:i), 'NT') .gt. 0) then
            Write (unit=filbas+cardg, fmt=1) 12, 0.0, seq, ' ', card
            ibgn=cardg
            iend=cardg
            ncode=12
         Else if (index(card(10:i), 'NB') .gt. 0) then
            Write (unit=filbas+cardb, fmt=1) 13, 0.0, seq, ' ', card
            Write (unit=filbas+carde, fmt=1) 13, 0.0, seq, ' ', card
            ibgn=cardb
            iend=carde
            ncode=13
         Else if (index(card(10:i), 'NP') .gt. 0) then
            Write (unit=filbas+cardd, fmt=1) 14, 0.0, seq, ' ', card
            ibgn=cardd
            iend=cardd
            ncode=14
         End if
        Else
*        -- Continuation of preceding N card comment
         Do 241 i=ibgn,iend
            Write (unit=filbas+i, fmt=1) ncode, 0.0, seq, ' ', card
  241    Continue
        End if
      Else if (icrd .eq. cardx) then
*        -- X card (use char code + 1000 for type).
         i = ichar(card(9:9))
         Write (unit=filbas+cardl, fmt=1) 1000+i, 0.0, seq, ' ', card
         nxcrd=nxcrd+1
         If(nxcrd.le.50) then
            xflg(nxcrd)=card(9:9)
         Else
            Write(*,*) 'Too many X cards: ',card
            nxcrd=50
         Endif
      Else
*        -- L, A, D, B, E, or G card (or comment or continuation).
         If (card(7:7) .ne. ' ') then
*           -- Must be some sort of comment.
            If (header) then
*              -- Comment for entire data set.
               If (card(6:6) .eq. ' ') then
*                 -- First comment card.
                  i = index(card, 'LABEL=')
                  If (i .gt. 0) then
*                    -- Process LABEL=.
*                    -- First find out the field being referenced.
                     If (card(10:10) .eq. 'L') then
                        ifld = 6
                     Else if (card(10:10) .eq. 'S') then
                        ifld = 7
                     Else
*                       -- Ignore this card (treat as normal comment).
                        Goto 25
                     End if
*                    -- Move the new text into the proper name field.
                     fldnam(ifld, cardl) = card(span(card,i+6,' '):80)
*                    -- Adjust field width if needed.
                     fldwid(ifld, cardl, 3)= max(fldwid(ifld, cardl, 3),
     +                  lenstr(fldnam(ifld, cardl)))
*                    -- No need for further processing of this card.
                     Goto 20
                  End if
   25             If (
     +            (index(card, '$') .eq. 0 .and.
     +               (card(10:19).eq.' '.or.card(19:19).ne.' '))
     +            .or. index(card,'$') .eq. 10
     1            .OR. CARD(7:7).EQ.'P') then
*                    -- Comment for the whole table.
                     type = 3
                     e = 0.0
*                    -- Increase file count if we have record comments.
*                    -- Note that icrd will be negative here so - => +.
                     filcnt(filbas-icrd) = filcnt(filbas-icrd) + 1
                     Write (unit=filbas-icrd,fmt=1) type,e,seq,elev,card
                  Else
*                    -- Footnote.
                     type = 3000
                     Call fnset(0, -icrd, card, seq)
                  End if
               Else
*                 -- Continuation comment card.
                  If (type .eq. 3000) then
*                    -- Footnote.
                     Call fnset(0, -icrd, card, seq)
                  Else
                     Write (unit=filbas-icrd,fmt=1) type,e,seq,elev,card
                  End if
               End if
            Else
*              -- Comment for preceding L, A, D, B, E, or G card.
               type = 2000
               Call setfld(card, nfld, -icrd)
               Write (unit=filbas-icrd, fmt=1) type, e, seq, elev, card
            End if
         Else
*           -- First or second data card.
            If (icrd .gt. 0) then
*              -- First (primary) data card.
               If (header) then
                  type = 2000
                  header = .false.
                  If (icrd .ne. cardl) then
*                    -- First unplaced data card, write footnote.
                     unpl(1:5) = card(1:5)
                     Write (unit=filbas+icrd, fmt=1)
     +                  3041, 0.0, seq, ' ', unpl
                  End if
               End if
*              -- First check for special processing of E field for
*                 various card types.
               If (icrd .eq. cardl) then
*                 -- L card special processing.
                  e = 0.0
*                 -- We don't really want to sort this table except by
*                    type and seq.
                  Call cnvs2u(card(10:19), card(20:21), el, del)
*                 -- Save el, del for computing energies on B, E cards.
*                 -- On L card a '?' in column 80 should show as a ? on
*                    E field and the LEV field as per M. Bhat 11/8/89
*                    and formats manual.
                  elev=card(10:19)
                  If(card(80:80).eq.'?') card(10:19)=
     *               elev(1:Lenstr(elev))//'?'
                  Call pakxdx(card(10:19), card(20:21), elev)
*                 -- Save elev to output for subsequent data cards.
               Else if ((adopt.or.gamord.ge.6).and.icrd.eq.cardg) then
*                 -- Like level table, we don't sort adopted gammas or
*                    photon branching from each level.
                  e = 0.0
               Else if (icrd .eq. cardb .or. icrd .eq. carde) then
*                 -- B and E card special processing.
                  If (card(10:19) .eq. ' ') then
*                    -- Energy not given, compute it.
                     e = qp + ep - el
C                    Round e up in the eigth significant figure just to
C                    improve consistency between various computers.
                     e=e*1.0000001
                     de = sqrt(dqp*dqp + dep*dep + del*del)
                     If(e.gt.0) then
                        Call numout(e, de, temp)
                        card(10:21) = '(' // temp(1:lenstr(temp)) // ')'
                     Else
                        card(10:21) = '(?-('//elev(1:lenstr(elev))//'))'
                     End if
                  Else
*                    -- Energy given, use it.
                     e = valstr(card(10:19))
                  End if
               Else
*                 -- Energy given, use it.
                  e = valstr(card(10:19))
               End if
*              -- Check to see if we need to output footnote for
*                 uncertain placement of radiations.
               If (icrd.gt.cardl.and.card(80:80).eq.'?') then
                  If (.not.quespl(icrd)) quespl(icrd)=Comqpl(icrd,'?')
               End if
*              -- Increase file count since we have data cards.
               filcnt(filbas+icrd) = filcnt(filbas+icrd) + 1
*              -- Set/adjust field tables for each field.
               Do 26 ifield = 1, nfld-1
                  ifld=ifield
                  If (ifld .eq. 2 .and. icrd .ne. cardl) then
*                    -- For non-L cards, use elev for field 2.
                     Call setfld(elev, 2, icrd)
                  Else
*                    -- Normal processing.
                     Call setfld(card, ifld, icrd)
*                    -- Save field length which may be augmented by
*                       footnotes from FLAG card.
                     Call fldlen(flen(ifld))
                  End if
   26          Continue
*              -- Write this card to proper file.
               Write (unit=filbas+icrd, fmt=1) type, e, seq, elev, card
*              -- Save sequence number of primary data card.
               pseq = seq
*              -- Increase seq to leave room for optional FLAG card and/
*                 or XREF.
               seq = seq + 2
            Else
*              -- Continuation data card.
*              -- Delete trailing $ if there.
               i = lenstr(card)
               If (card(i:i) .eq. '$') card(i:i) = ' '
*              -- Check for FLAG=, special processing.
               i = index(card, 'FLAG')
               If (i .gt. 0) then
*                 -- Process FLAG field.
                  fcard(1:9) = card(1:9)
*                 -- Look for $ after FLAG field.
                  j = break(card, i+4, '$')
                  If (j .eq. 81) then
*                    -- No trailing $, last or only field on card.
*                    -- Copy to fcard and delete from card.
                     fcard(10:80) = card(i:80)
                     Call delstr(card, i, 81-i)
                  Else
*                    -- First or middle field.
*                    -- Copy to fcard and delete from card.
                     fcard(10:80) = card(i:j-1)
*                    -- Want to delete leading blanks from next field.
                     j = span(card, j+1, ' ')
                     Call delstr(card, i, j-i)
                  End if
*                 -- Write the FLAG card.
                  Write (unit=filbas-icrd,fmt=1)type,e,pseq+1,elev,fcard
*                 -- It may be necessary to augment the fldwid table as
*                    a result of the FLAG card. Check it out.
*                 -- Isolate flags.
                  i = index(fcard, '=')
                  If(lenstr(fcard).ge.i+1) then
                     flgstr = fcard(span(fcard, i+1, ' '):lenstr(fcard))
                  End if
                  i = lenstr(flgstr)
*                 -- Check fields to see if they need augmentation.
                  Do 27 ifield = 1, nfld-1
                     ifld=ifield
*                    -- Not necessary for elev field.
                     If (ifld .eq. 2) Goto 27
                     Call fnlen(j, ifld, -icrd, flgstr(1:i))
                     If (j .gt. 0) then
                        j = j + flen(ifld)
                        fldwid(ifld,-icrd,3)=max(j,fldwid(ifld,-icrd,3))
                     End if
   27             Continue
               End if
*              XREF only exists following L cards in Adopted Data Sets
               If (-icrd.eq.cardl) then
*                 -- Check for XREF=, special processing.
                  i = index(card, 'XREF')
                  If (i .gt. 0) then
                     fldxst(4,-icrd) = .true.
*                    -- Process XREF field.
                     fcard(1:9) = card(1:9)
*                    -- Look for $ after XREF field.
                     j = break(card, i+4, '$')
                     If (j .eq. 81) then
*                       -- No trailing $, last or only field on card.
*                       -- Copy to fcard and delete from card.
                        fcard(10:80) = card(i:80)
                        Call delstr(card, i, 81-i)
                     Else
*                       -- First or middle field.
*                       -- Copy to fcard and delete from card.
                        fcard(10:80) = card(i:j-1)
*                       -- Want to delete leading blanks from next field
                        j = span(card, j+1, ' ')
                        Call delstr(card, i, j-i)
                     End if
                     i = index(fcard, '=')
                     If(lenstr(fcard).ge.i+1) then
                     flgstr = fcard(span(fcard, i+1, ' '):lenstr(fcard))
                     End if
                     fldwid(4,-icrd,3) = max(fldwid(4,-icrd,3),nxcrd)
*                    -- Write the XREF card.
                     Write (unit=filbas-icrd,fmt=1)type,e,pseq+2,elev,
     *                  fcard
                  End if
               End if
 
*              -- If card not blank, write it.
               If (card(10:80) .ne. ' ') then
                  Write (unit=filbas-icrd, fmt=1) type,e,seq,elev,card
                  Call setfld(card, nfld, -icrd)
               End if
            End if
         End if
      End if
*     -- Go get next card.
      Goto 20
 
*  Sort the temp files.
 
   30 Continue
C     Write normalization footnotes to the appropriate files before
C        sorting.
      If(nseq.gt.0) Call NRMOUT(nseq,card)
      Do 35 ifile = filelo, filehi
         i=ifile
         Write(unit=i,fmt='(''9999'')')
         If (filcnt(i) .gt. 0) Call fsort(i)
   35 Continue
      Return
*  If no more data sets, set eof true and delete all temp files.
 
   99 eof= .true.
      Do 999 i = filelo, filehi+4
         Close (unit=i, status='DELETE')
  999 Continue
      Return
      End
 
      Block data FLDATA
 
************************************************************************
*  The field tables contains information about the fields used in      *
*     various places.                                                  *
*  Fields are referenced by two or three indices:                      *
*     the first  is the field number (see chart),                      *
*     the second is the card type.                                     *
*     the third (if given) refers to a table type:                     *
*        1. the data field on a data card.                             *
*        2. the uncertainty field on a data card.                      *
*        3. the report file.                                           *
*                                                                      *
*    field | 1     2       3       4       5       6       7       8   *
*  card    |                                                           *
*  --------+-----------------------------------------------------------*
*    L     | E             JPI     XREF    T       L       S       COMM*
*    A     | E     LEV     IA      HF                              COMM*
*    D     | E     LEV     IP              WIDTH   L       LEVTO   COMM*
*    B     | E     LEV     IB              LOGFT                   COMM*
*    E     | E     LEV     IB      IE      LOGFT           TI      COMM*
*    G     | E     LEV     RI      M       MR      CC      TI      COMM*
************************************************************************
 
*  Parameters.
 
      Integer           nfld, ncrd
      Parameter        (nfld = 8, ncrd = 6)
      Integer           cardl, carda, cardd, cardb, carde, cardg
      Parameter        (cardl =  1, carda =  2, cardd =  3)
      Parameter        (cardb =  4, carde =  5, cardg =  6)
 
*  Common blocks.
 
      Character*11      fldnam(nfld, ncrd)
      Common   /fldnam/ fldnam
 
      Integer           fldpos(nfld, ncrd, 3)
      Common   /fldpos/ fldpos
 
      Integer           fldwid(nfld, ncrd, 3)
      Common   /fldwid/ fldwid
 
*  Local variables.
 
      Integer           icrd
      Integer           ifld
 
*  Data initializations.
 
      Data (fldnam(ifld, cardl), ifld = 1, nfld) /
     L   'E(level)   ', '           ', 'Jpi        ', 'XREF       ',
     L   'T1/2       ', 'L          ', 'S          ', 'Comments   '/
      Data (fldnam(ifld, carda), ifld = 1, nfld) /
     A   'Ealpha     ', 'Level      ', 'Ialpha     ', 'HF         ',
     A   '           ', '           ', '           ', 'Comments   '/
      Data (fldnam(ifld, cardd), ifld = 1, nfld) /
     D   'Epart      ', 'Level to   ', 'Ipart      ', '           ',
     D   'Width      ', 'L          ', 'Level from ', 'Comments   '/
      Data (fldnam(ifld, cardb), ifld = 1, nfld) /
     B   'Eb-        ', 'Level      ', 'Ib-        ', '           ',
     B   'Log ft     ', '           ', '           ', 'Comments   '/
      Data (fldnam(ifld, carde), ifld = 1, nfld) /
     E   'Eec        ', 'Level      ', 'Ib+        ', 'Iec        ',
     E   'Log ft     ', '           ', 'Iec+b+     ', 'Comments   '/
      Data (fldnam(ifld, cardg), ifld = 1, nfld) /
     G   'Eg         ', 'Level      ', 'Ig         ', 'Mult.      ',
     G   'Delta      ', 'Alpha      ', 'Ig+ce      ', 'Comments   '/
 
      Data ((fldpos(ifld, icrd, 1), ifld = 1, nfld), icrd = 1, ncrd) /
     L        10,  0, 22,  0, 40, 56, 65,  0,
     A        10,  0, 22, 32,  0,  0,  0,  0,
     D        10,  0, 22,  0, 40, 56, 32,  0,
     B        10,  0, 22,  0, 42,  0,  0,  0,
     E        10,  0, 22, 32, 42,  0, 65,  0,
     G        10,  0, 22, 32, 42, 56, 65,  0/
      Data ((fldpos(ifld, icrd, 2), ifld = 1, nfld), icrd = 1, ncrd) /
     L        20,  0,  0,  0, 50,  0, 75,  0,
     A        20,  0, 30, 40,  0,  0,  0,  0,
     D        20,  0, 30,  0, 50,  0,  0,  0,
     B        20,  0, 30,  0, 50,  0,  0,  0,
     E        20,  0, 30, 40, 50,  0, 75,  0,
     G        20,  0, 30,  0, 50, 63, 75,  0/
 
      Data ((fldwid(ifld, icrd, 1), ifld = 1, nfld), icrd = 1, ncrd) /
     L        10,  0, 18,  4, 10,  9, 10,  0,
     A        10,  0,  8,  8,  0,  0,  0,  0,
     D        10,  0,  8,  0, 10,  9,  8,  0,
     B        10,  0,  8,  0,  8,  0,  0,  0,
     E        10,  0,  8,  8,  8,  0, 10,  0,
     G        10,  0,  8, 10,  8,  7, 10,  0/
      Data ((fldwid(ifld, icrd, 2), ifld = 1, nfld), icrd = 1, ncrd) /
     L         2,  0,  0,  0,  6,  0,  2,  0,
     A         2,  0,  2,  2,  0,  0,  0,  0,
     D         2,  0,  2,  0,  6,  0,  0,  0,
     B         2,  0,  2,  0,  6,  0,  0,  0,
     E         2,  0,  2,  2,  6,  0,  2,  0,
     G         2,  0,  2,  0,  6,  2,  2,  0/
 
      End
 
      Subroutine INIFLD
 
************************************************************************
*  The field tables contains information about the fields used in      *
*     various places.                                                  *
*  Fields are referenced by two or three indices:                      *
*     the first  is the field number (see chart),                      *
*     the second is the card type.                                     *
*     the third (if given) refers to a table type:                     *
*        1. the data field on a data card.                             *
*        2. the uncertainty field on a data card.                      *
*        3. the report file.                                           *
*                                                                      *
*    field | 1     2       3       4       5       6       7       8   *
*  card    |                                                           *
*  --------+-----------------------------------------------------------*
*    L     | E             JPI             T       L       S       COMM*
*    A     | E     LEV     IA      HF                              COMM*
*    D     | E     LEV     IP              WIDTH   L       LEVTO   COMM*
*    B     | E     LEV     IB              LOGFT                   COMM*
*    E     | E     LEV     IB      IE      LOGFT           TI      COMM*
*    G     | E     LEV     RI      M       MR      CC      TI      COMM*
************************************************************************
 
*  Function references.
 
      Integer           lenstr
 
*  Parameters.
 
      Integer           nfld, ncrd
      Parameter        (nfld = 8, ncrd = 6)
      Integer           cardl, carda, cardd, cardb, carde, cardg
      Parameter        (cardl =  1, carda =  2, cardd =  3)
      Parameter        (cardb =  4, carde =  5, cardg =  6)
 
*  Common blocks.
 
      Character*11      fldnam(nfld, ncrd)
      Common   /fldnam/ fldnam
 
      Integer           fldpos(nfld, ncrd, 3)
      Common   /fldpos/ fldpos
 
      Integer           fldwid(nfld, ncrd, 3)
      Common   /fldwid/ fldwid
 
      Logical           fldxst(nfld, ncrd)
      Common   /fldxst/ fldxst
 
*  Local variables.
 
      Integer           icrd
      Integer           ifld
 
************************************************************************
 
*  Reset field names for L card, L and S fields.
*  These may be changed by LABEL= on comment cards.
 
      fldnam(6, cardl) = 'L'
      fldnam(7, cardl) = 'S'
 
*  Initialize non-constant parts of tables.
 
      Do 20 icrd = cardl, cardg
         Do 10 ifld = 1, nfld
            fldpos(ifld, icrd, 3) = 0
            fldwid(ifld, icrd, 3) = lenstr(fldnam(ifld, icrd))
*           -- Min width = width(title).
            fldxst(ifld, icrd) = .false.
   10    Continue
   20 Continue
      End
 
      Subroutine SETFLD (card, ifld, icrd)
 
************************************************************************
*  Sets fldwid and fldxst as appropriate for this field.               *
************************************************************************
 
*  Subroutine arguments.
 
      Character*(*)     card
      Integer           flen
*                          For FLDLEN.
      Integer           ifld
      Integer           icrd
 
*  Function references.
 
      Integer           lenstr
      Integer           span
 
*  Parameters.
 
      Integer           nfld, ncrd
      Parameter        (nfld = 8, ncrd = 6)
      Integer           cardl, carda, cardd, cardb, carde, cardg
      Parameter        (cardl =  1, carda =  2, cardd =  3)
      Parameter        (cardb =  4, carde =  5, cardg =  6)
 
*  Common blocks.
 
      Integer           fldpos(nfld, ncrd, 3)
      Common   /fldpos/ fldpos
 
      Integer           fldwid(nfld, ncrd, 3)
      Common   /fldwid/ fldwid
 
      Logical           fldxst(nfld, ncrd)
      Common   /fldxst/ fldxst
 
*  Local variables.
 
      Character*25      field
      Integer           ipos
      Integer           iwid
      Integer           jwid
      Save              jwid
 
************************************************************************
      jwid=0
*  For comment field, just mark its existance.
 
      If (ifld .eq. 8) then
         fldxst(8, icrd) = .true.
         Return
 
*  For elev field, mark its existance and size even though no pos.
 
      Else If (ifld .eq. 2 .and. icrd .ne. cardl) then
         If (card .eq. ' ') Return
         fldxst(2, icrd) = .true.
         jwid = lenstr(card)
         fldwid(ifld, icrd, 3) = max(fldwid(ifld, icrd, 3), jwid)
         Return
 
*  For all others, pick up field from card.
 
      Else
         ipos = fldpos(ifld, icrd, 1)
         If (ipos .eq. 0) Return
         iwid = fldwid(ifld, icrd, 1)
         field = card(ipos:ipos-1+iwid)
 
*  Check if field exists (if not, just return).
 
         If (field .eq. ' ') Return
         fldxst(ifld, icrd) = .true.
 
*  Compute output width of this field.
 
         jwid = lenstr(field)
*        -- Start with length of the field.
         jwid = jwid - span(field, 1, ' ') + 1
*        -- Ignore leading spaces.
         iwid = 0
         If (card(77:77) .ne. ' ') then
*           -- Add in length of footnote reference string.
            Call fnlen(iwid, ifld, icrd, card(77:77))
            jwid = jwid + iwid
         End if
         If (ifld .eq. 1.and.icrd.gt.cardl) then
*           -- For energy field only on radiation cards...
            If (card(80:80) .eq. '?') then
*              -- If field already has a footnote subtract for []
*                 characters.
               If (iwid.gt.0) jwid = jwid - 2
*              -- Add in length of footnote reference string generated
*                 by uncertain placement.
               Call fnlen(iwid, ifld, icrd, card(80:80))
               jwid = jwid + iwid
            End if
         End if
         If (ifld .eq. 1) then
*           -- For energy field only...
            If (card(80:80) .eq. 'S') then
*              -- Add space for "()" around E DE.
               If (field(1:1) .ne. '(') then
*                 -- But only if not already there.
                  jwid = jwid + 2
               End if
            End if
         Else if (ifld .eq. 5
     +   .and. (icrd .eq. 4 .or. icrd .eq. 5)) then
*           -- Include UN field in brackets [] if not blank.
            If (card(78:79) .ne. ' ') then
               jwid = jwid + 4
            End if
         End if
         ipos = fldpos(ifld, icrd, 2)
*        -- Now look at uncertainty.
         If (ipos .gt. 0) then
*           -- There may be an uncertainty here.
            iwid = fldwid(ifld, icrd, 2)
            field = card(ipos:ipos-1+iwid)
            If (field .ne. ' ') then
*              -- There is an uncertainty here.
               jwid = jwid + lenstr(field) + 1
*              -- Add 1 for blank between field and uncert.
               jwid = jwid - span(field, 1, ' ') + 1
*              -- Ignore leading spaces.
            End if
         End if
 
*  Set new max width.
 
         fldwid(ifld, icrd, 3) = max(fldwid(ifld, icrd, 3), jwid)
      End if
      Return
 
************************************************************************
 
      Entry FLDLEN (flen)
 
*  Return length (width) of last field set.
 
      flen = jwid
      Return
      End
 
      Integer function FLDID (field)
 
************************************************************************
*  Returns field designator number given field name string.            *
************************************************************************
 
*  Subroutine arguments.
 
      Character*(*)     field
 
*  Parameters.
 
      Integer           nfld
      Parameter        (nfld = 19)
 
*  Local variables.
 
      Character*5       fldnam(nfld)
      Integer           fldnum(nfld)
      Integer           i
 
*  Data initializations.
 
      Data fldnam /
     +    'E    ', 'BAND ',
     +    'J    ', 'RI   ', 'IB   ', 'IA   ', 'IP   ',
     +    'M    ', 'IE   ', 'HF   ',
     +    'T    ', 'MR   ', 'LOGFT',
     +    'L    ', 'CC   ',
     +    'S    ', 'TI   ', 'ED   ',
     +    'TITLE'/
      Data fldnum /2*1, 5*3, 3*4, 3*5, 2*6, 3*7, 8/
 
************************************************************************
 
      Do 10 i = 1, nfld
         If (field .eq. fldnam(i)) then
            fldid = fldnum(i)
            Return
         End if
   10 Continue
      fldid = 0
      Return
      End
 
      Logical Function COMQPL(icard,note)
C
C     Handles footnoting for questionable placed radiations
C
*  Subroutine arguments.
 
      Integer icard
      Character*1 note
 
*  Local variables.
 
      Character cmxqpl(2:6)*80
      Data cmxqpl/
     5'      CA E( )      Existence of this branch is questionable',
     6'      CD E( )      Placement of transition in the decay scheme is
     * uncertain.',
     4'      CB E( )      Existence of this branch is questionable.',
     3'      CE E( )      Existence of this branch is questionable.',
     2'      CG E( )      Placement of transition in the level scheme is
     * uncertain.'/
      cmxqpl(icard)(12:12)=note
      Call fnset(0,icard,cmxqpl(icard),1)
      comqpl=.true.
      Return
      End
 
 
      Subroutine FOOTNT ()
 
************************************************************************
*  Footnote processing procedures.  Kept in one place to hide details. *
*     FOOTNT - Initialization.                                         *
*     FNSET  - Store this footnote.                                    *
*     FNREF  - Form footnote reference string.                         *
*     FNLEN  - Returns length of footnote reference string or 0.       *
*     FNXST  - Returns true if column for this footnote exists.        *
*                                                                      *
*  Note: Field 8 is used for TITLE footnote, not comment footnotes.    *
************************************************************************
 
*  Subroutine arguments.
 
      Integer           ifld
      Integer           icrd
      Character*80      card
c      Integer           seq
      Integer           seqin
      Character*(*)     refstr
      Character*(*)     flgstr
      Integer           width
      Logical           xst
 
*  Function references.
 
      Integer           break
      Integer           fldid
      Integer           indexf
      Integer           lenstr
 
*  Parameters.
 
      Integer           filbas
      Parameter        (filbas = 30)
      Integer           maxcod
      Parameter        (maxcod = 40)
      Integer           nfld, ncrd
      Parameter        (nfld = 8, ncrd = 6)
      Integer           nflg, nref
      Parameter        (nflg = 10, nref = 35)
 
*  Common blocks.
 
      Logical           fldxst(nfld, ncrd)
      Common   /fldxst/ fldxst
 
*  Local variables.
 
      Character*(maxcod)code
      Save              code
*                          Report reference code characters.
      Character*70      flag
*                          Work space for sym(flag).
      Character*(nflg)  fnflg(nref, nfld, ncrd)
      Save              fnflg
*                          Flags for this footnote.
      Integer           fnnot(nref, nfld, ncrd)
      Save              fnnot
*                          Reference code numbers.
      Logical           gen
*                          Controls generation of footnote ref string.
      Logical           nompf
*                          Controls generation of multiply placed gamma
*                             footnote
      Logical           nodiv
*                          Controls generation of mult gamma, divided
*                             intensity footnote.
      Logical           nound
*                          Controls generation of mult gamma, undivided
*                             intensity footnote.
      Integer           i, j, k, l
*                          Temporary indices or counters.
      Integer           jfld, jcrd
*                          Local ifld and icrd if one must be used.
      Integer           nent(nfld, ncrd)
      Save              nent
*                          Current number of entries for this field.
      Integer           nfn(ncrd)
      Save              nfn
*                          Current number of footnotes for this card.
      Integer           seq
 
*  Data initializations.
 
      Data code /'*#@%&abcdefghijklmnopqrstuvwyz0123456789'/
*  Save everything for now
      Save
*  Format statements.
 
    1 Format('3', I3.3, '    0.000', I5, 13X, A)
    2 Format('   3', '    0.000', I5, 13X, A)
 
************************************************************************
 
      Do 15 jcrd = 1, ncrd
         Do 10 jfld = 1, nfld
            nent(jfld, jcrd) = 0
   10    Continue
         nfn(jcrd) = 0
   15 Continue
      nompf=.true.
      nodiv=.true.
      nound=.true.
      Return
 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 
      Entry FNSET (ifld, icrd, card, seqin)
 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*  Store this footnote in the footnote tables.                        *
*                                                                     *
*  In particular:                                                     *
*     the sequential footnote number.                                 *
*     the reference flags (if any).                                   *
*                                                                     *
*  If ifld = 0 then scan card to determine the field(s).              *
*                                                                     *
*  If seq = 0 then ignore text and add new entry with previous        *
*     footnote number and new card/field (flag = ' ').                *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*'860207' added code to output columns with no data if there is a
*   footnote in it.
*'870316' modified '860207' so that if must be a flagged footnote.
 
*  If seq = 0 same note, but new card/field.
 
      seq=seqin
      If (seq .eq. 0) then
         nent(ifld, icrd) = nent(ifld, icrd) + 1
         If(index(card,'(').gt.10.and.index(card,'(').lt.index(card,'$')
     +      .or.(index(card,'$').eq.0.and.index(card,'(').gt.10.and.
     +      index(card,'(').lt.17)) fldxst(ifld, icrd) = .true.
         If (nent(ifld, icrd) .gt. nref) then
C           Too many footnotes, write footnote a general comment.
            nent(ifld,icrd)=nref
            nfn(icrd)=-nfn(icrd)
            Write (unit=filbas+icrd, fmt=2) seq, card
            Return
         End if
         fnnot(nent(ifld, icrd), ifld, icrd) = nfn(icrd)
         fnflg(nent(ifld, icrd), ifld, icrd) = ' '
         Return
      End if
 
*  Continuation cards are sent out as is.
 
      If (card(6:6) .ne. ' ') then
*        -- Continued footnote.
         If(nfn(icrd).gt.0) Write (unit=filbas+icrd, fmt=1) nfn(icrd),
     +   seq, card
         If(nfn(icrd).lt.0) Write (unit=filbas+icrd, fmt=2) seq, card
         Return
      End if
 
*  Primary footnote card.
*  Check to see if there is room for it.
*  The sign of nfn has been used to signal if the continuation cards
*    should be output as footnotes or general comments.
 
      If(nfn(icrd).ge.0) nfn(icrd) = nfn(icrd)+ 1
      If(nfn(icrd).lt.0) nfn(icrd)=iabs(nfn(icrd))
      If (nfn(icrd) .gt. maxcod) then
         nfn(icrd)=-nfn(icrd)
         Write (unit=filbas+icrd, fmt=2) seq, card
         Return
      End if
 
*  If ifld given, then store footnote as given.
*  Note: No flags specified, enter blank flag string.
 
      If (ifld .gt. 0) then
         nent(ifld, icrd) = nent(ifld, icrd) + 1
         If(index(card,'(').gt.10.and.index(card,'(').lt.index(card,'$')
     +      .or.(index(card,'$').eq.0.and.index(card,'(').gt.10.and.
     +      index(card,'(').lt.17)) fldxst(ifld, icrd) = .true.
         If (nent(ifld, icrd) .gt. nref) then
            nent(ifld,icrd)=nref
            nfn(icrd)=-nfn(icrd)
            Write (unit=filbas+icrd, fmt=2) seq, card
            Return
         End if
         fnnot(nent(ifld, icrd), ifld, icrd) = nfn(icrd)
         fnflg(nent(ifld, icrd), ifld, icrd) = ' '
         Write (unit=filbas+icrd, fmt=1) nfn(icrd), seq, card
         Return
      End if
 
*  Else ifld not given, must decode field names [and flags].
*  Isolate sym(flag) information.
 
      i = indexf(card, 10, '$')
      If (i .eq. 0) i = 20
      flag = card(10:i-1)
      l = lenstr(flag)
 
*  Look for imbedded ',' within '(', ')' and delete them.
 
      j = 0
   50 i = indexf(flag(1:l), j+1, '(')
      If (i .eq. 0) Goto 110
      j = indexf(flag(1:l), i+1, ')')
      If (j .eq. 0) j = l
   60 k = indexf(flag(1:j), i+1, ',')
      If (k .gt. 0) then
         Call delstr(flag(1:l), k, 1)
         l = l - 1
         j = j - 1
         Goto 60
      End if
      If (j .lt. l) Goto 50
 
*  Get field information.
 
*     -- Find next ',' or end of field.
  110 k = index(flag(1:l), ',') - 1
      If (k .lt. 0) k = l
*     -- Find '(', if there.
      i = index(flag(1:k), '(') - 1
      If (i .lt. 0) i = k
*     -- Find field id number.
      jfld = fldid(flag(1:i))
      If (jfld .eq. 0) then
         Write (6, *) 'Invalid field name: ', flag(1:i)
         Write (6, *) card
         If(flag(1:1).eq.'D') Go to 120
         nfn(icrd)=-nfn(icrd)
         Write (unit=filbas+icrd, fmt=2) seq, card
         Return
      End if
*     -- Increase entity count.
      nent(jfld, icrd) = nent(jfld, icrd) + 1
         If(index(card,'(').gt.10.and.index(card,'(').lt.index(card,'$')
     +      .or.(index(card,'$').eq.0.and.index(card,'(').gt.10.and.
     +      index(card,'(').lt.17)) fldxst(jfld, icrd) = .true.
         If (nent(jfld, icrd) .gt. nref) then
            nent(jfld,icrd)=nref
            nfn(icrd)=-nfn(icrd)
            Write (unit=filbas+icrd, fmt=2) seq, card
            Return
         End if
*     -- Store reference code.
      fnnot(nent(jfld, icrd), jfld, icrd) = nfn(icrd)
      If (i .eq. k) then
*        -- No flags.
         fnflg(nent(jfld, icrd), jfld, icrd) = ' '
      Else
*        -- Flags.
         i = i + 2
         j = indexf(flag(1:k), i, ')') - 1
         If (j .lt. i) then
            Write (6, *) 'Invalid flags:'
            Write (6, *) card
            nfn(icrd)=-nfn(icrd)
         Write (unit=filbas+icrd, fmt=2) seq, card
            Return
         End if
         fnflg(nent(jfld, icrd), jfld, icrd) = flag(i:j)
      End if
  120 Call delstr(flag, 1, k+1)
      If (flag .ne. ' ') Goto 110
 
*  Finally, write the footnote text to the proper file.
 
      Write (unit=filbas+icrd, fmt=1) nfn(icrd), seq, card
      Return
 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 
      Entry FNREF (refstr, ifld, icrd, flgstr)
 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*  Generate the footnote reference string.                            *
*                                                                     *
*  For each entry for the specified field and card:                   *
*     If flgstr is blank then accept only blank entries.              *
*     If flgstr not blank then accept the entry if any character in   *
*        flgstr matches a character in fnflg(i, ifld, icrd).          *
*                                                                     *
*  If an entry is accepted add the code for the associated fnnot      *
*     value.                                                          *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 
      gen = .true.
*     -- Generate reference string.
      refstr = '['
      Goto 200
 
      Entry FNLEN (width, ifld, icrd, flgstr)
 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*  Compute length of footnote reference string without generating it. *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 
      gen = .false.
*     -- Do not generate reference string, count only.
 
  200 j = 1
*  The sign of nfn may have been used to signal if the continuation
*    cards should be output as footnotes or general comments.
      If(nfn(icrd).lt.0) nfn(icrd)=iabs(nfn(icrd))-1
 
*     -- New code to take care of outputting footnotes for multiply
*        placed gamma
      If (icrd .eq. 6) then
*        -- Okay, we have a G card, do we need to output a footnote?
         If (nompf.and.(index(flgstr,'*').gt.0)) then
            nfn(icrd) = nfn(icrd) + 1
            If (nfn(icrd) .gt. maxcod) then
               Write (unit=filbas+icrd, fmt=2) seq,'         '//
     +'No available code for multiplet footnote'
               Return
            End if
            nent(ifld, icrd) = nent(ifld, icrd) + 1
            If (nent(ifld, icrd) .gt. nref) then
               nent(ifld,icrd)=nref
               Write (unit=filbas+icrd, fmt=2) seq,'         '//
     +'No available room for multiplet footnote'
               Return
            End if
            fnnot(nent(ifld, icrd), ifld, icrd) = nfn(icrd)
            fnflg(nent(ifld, icrd), ifld, icrd) = '*'
            Write (unit=filbas+icrd, fmt=1) nfn(icrd), 1,'         '
     *       //'$Multiply placed.'
            nompf=.false.
         Else if (nodiv.and.(index(flgstr,'@').gt.0)) then
            nfn(icrd) = nfn(icrd) + 1
            If (nfn(icrd) .gt. maxcod) Then
               Write (unit=filbas+icrd, fmt=2) seq,'         '//
     +'No available code for multiplet footnote'
               Return
            End if
            nent(ifld, icrd) = nent(ifld, icrd) + 1
            If (nent(ifld, icrd) .gt. nref) then
               nent(ifld,icrd)=nref
               Write (unit=filbas+icrd, fmt=2) seq,'         '//
     +'No available room for multiplet footnote'
               Return
            End if
            fnnot(nent(ifld, icrd), ifld, icrd) = nfn(icrd)
            fnflg(nent(ifld, icrd), ifld, icrd) = '@'
            Write (unit=filbas+icrd, fmt=1) nfn(icrd), 1,'         '
     *       //'$Multiply placed; intensity suitably divided.'
            If (fldxst(3,icrd)) then
               nent(3, icrd) = nent(3, icrd) + 1
               If (nent(3, icrd) .gt. nref) then
                  nent(3,icrd)=nref
                  Write (unit=filbas+icrd, fmt=2) seq,'         '//
     +'No available room for multiplet footnote'
                  Return
               End if
               fnnot(nent(3, icrd), 3, icrd) = nfn(icrd)
               fnflg(nent(3, icrd), 3, icrd) = '@'
            Else if (fldxst(7,icrd)) then
               nent(7, icrd) = nent(7, icrd) + 1
               If (nent(7, icrd) .gt. nref) then
                  nent(7,icrd)=nref
               Write (unit=filbas+icrd, fmt=2) seq,'         '//
     +'No available room for multiplet footnote'
                  Return
               End if
               fnnot(nent(7, icrd), 7, icrd) = nfn(icrd)
               fnflg(nent(7, icrd), 7, icrd) = '@'
            End if
            nodiv=.false.
         Else if (nound.and.(index(flgstr,'&').gt.0)) then
            nfn(icrd) = nfn(icrd) + 1
            If (nfn(icrd) .gt. maxcod) Then
               Write (unit=filbas+icrd, fmt=2) seq,'         '//
     +'No available code for multiplet footnote'
               Return
            End if
            nent(ifld, icrd) = nent(ifld, icrd) + 1
            If (nent(ifld, icrd) .gt. nref) then
               nent(ifld,icrd)=nref
               Write (unit=filbas+icrd, fmt=2) seq,'         '//
     +'No available room for multiplet footnote'
               Return
            End if
            fnnot(nent(ifld, icrd), ifld, icrd) = nfn(icrd)
            fnflg(nent(ifld, icrd), ifld, icrd) = '&'
            Write (unit=filbas+icrd, fmt=1) nfn(icrd), 1,'         '
     *       //'$Multiply placed; undivided intensity given.'
            If (fldxst(3,icrd)) then
               nent(3, icrd) = nent(3, icrd) + 1
               If (nent(3, icrd) .gt. nref) then
                  nent(3,icrd)=nref
                  Write (unit=filbas+icrd, fmt=2) seq,'         '//
     +'No available room for multiplet footnote'
                  Return
               End if
               fnnot(nent(3, icrd), 3, icrd) = nfn(icrd)
               fnflg(nent(3, icrd), 3, icrd) = '&'
            Else if (fldxst(7,icrd)) then
               nent(7, icrd) = nent(7, icrd) + 1
               If (nent(7, icrd) .gt. nref) then
                  nent(7,icrd)=nref
               Write (unit=filbas+icrd, fmt=2) seq,'         '//
     +'No available room for multiplet footnote'
                  Return
               End if
               fnnot(nent(7, icrd), 7, icrd) = nfn(icrd)
               fnflg(nent(7, icrd), 7, icrd) = '&'
            End if
            nound=.false.
         End if
      End if
*     -- Current length of refstr.
      Do 210 i = 1, nent(ifld, icrd)
         flag = fnflg(i, ifld, icrd)
         If (flgstr .eq. ' ') then
            If (flag .eq. ' ') then
               j = j + 1
               If (gen) then
                  k = fnnot(i, ifld, icrd)
                  refstr(j:j) = code(k:k)
               End if
            End if
         Else
            If (break(flag, 1, flgstr) .le. lenstr(flag)) then
               j = j + 1
               If (gen) then
                  k = fnnot(i, ifld, icrd)
                  refstr(j:j) = code(k:k)
               End if
            End if
         End if
  210 Continue
      j = j + 1
      If (gen) then
         If (j .eq. 2) then
            refstr = ' '
         Else
            refstr(j:j) = ']'
         End if
      Else
         If (j .eq. 2) then
            width = 0
         Else
            width = j
         End if
      End if
      Return
 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 
      Entry FNXST (xst, seqin, icrd)
 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*  Determine if field associated with this footnote exists.           *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 
      seq=seqin
      xst = .false.
      jcrd = icrd
      Do 320 jfld = 1, nfld
         If(nent(jfld,jcrd).ge.1) then
         Do 310 k = 1, nent(jfld, jcrd)
            If (fnnot(k, jfld, jcrd) .eq. seq) then
               If (jfld .eq. nfld) then
                  xst = .true.
               Else
                  xst = fldxst(jfld, jcrd)
               End if
C              In the case of multiple columns using the same footnote,
C                 nonexistence of the first one encountered is not
C                 sufficient to suppress output of the footnote.
               If(xst) Return
            End if
  310    Continue
         Endif
  320 Continue
      Return
      End
 
      Subroutine NCARD (seq, card)
 
************************************************************************
*  Based on data found on N card, set footnotes.                       *
************************************************************************
 
*  Subroutine arguments.
 
      Integer           seq
      Character*80      card
 
*  Parameters.
 
      Integer           cardl, carda, cardd, cardb, carde, cardg
      Parameter        (cardl =  1, carda =  2, cardd =  3)
      Parameter        (cardb =  4, carde =  5, cardg =  6)
 
*  Common blocks
 
      LOGICAL NONRBR,NONTBR,NONBBR,NONP,ISCONT
      COMMON/LOGPN/NONRBR,NONTBR,NONBBR,NONP,ISCONT
C     Save the strings
      CHARACTER*2 DNRBRS,DNTBRS,DNPS
      CHARACTER*6 DNBBRS
      CHARACTER*7 NPS
      CHARACTER*8 NTBRS,NBBRS
      CHARACTER*10 NRBRS
      COMMON/PNSTR/NRBRS,DNRBRS,NTBRS,DNTBRS,NBBRS,DNBBRS,NPS,DNPS
 
*  Functions
      Integer           LENSTR
      External          LENSTR
 
*  Local variables.
 
      Real              br, dbr
      Real              nb, dnb
      Real              np, dnp
      Real              nr, dnr
      Real              nt, dnt
C     Save the normalization strings
      Character*10      brstr,nbstr,npstr,nrstr,ntstr
      Character*(*)     text
      Parameter        (text =
     +   'For absolute intensity per 100 decays, multiply by ')
      Logical           yesnb, yesnp, yesnr, yesnt, yesbr
      Save 
************************************************************************
 
*  We will generate some new card images, therefore, decrement seq.
 
      seq = seq - 1
 
*  Branching ratio, will be applied to other normalizations.
 
      yesbr = .false.
      br = 1.0
      dbr = 0.0
      If (card(32:39) .ne. ' ') then
         yesbr = .true.
         Call cnvs2u(card(32:39), card(40:41), br, dbr)
         brstr=card(32:39)
      End if
 
*  G:RI normalization factor.
 
      yesnr = .false.
      nonrbr = .true.
      nr = 0.0
      If (card(10:19) .ne. ' ') then
         yesnr = .true.
         Call cnvs2u(card(10:19), card(20:21), nr, dnr)
         nrstr=card(10:19)
         Call umult(nr, dnr, nr, dnr, br, dbr)
      End if
 
*  G:TI normalization factor.
 
      yesnt = .false.
      nontbr = .true.
      If (card(22:29) .ne. ' ') then
         yesnt = .true.
         Call cnvs2u(card(22:29), card(30:31), nt, dnt)
         ntstr=card(22:29)
         Call umult(nt, dnt, nt, dnt, br, dbr)
      End if
 
*  E:IB, E:IE, E:TI, B:IB normalization factor.
 
      yesnb = .false.
      nonbbr = .true.
      If (card(42:49) .ne. ' ') then
         yesnb = .true.
         Call cnvs2u(card(42:49), card(50:55), nb, dnb)
         nbstr=card(42:49)
         Call umult(nb, dnb, nb, dnb, br, dbr)
      Else if (yesbr) then
         yesnb = .true.
         nb = br
         dnb = dbr
      End if
 
*  P:IP normalization factor.
 
      yesnp = .false.
      nonp = .true.
      If (card(56:62) .ne. ' ') then
         yesnp = .true.
         Call cnvs2u(card(56:62), card(63:64), np, dnp)
         npstr=card(56:62)
C * Particle normalization is np not np*br
C *         Call umult(np, dnp, np, dnp, br, dbr)
      End if
      Return
 
      Entry NRMOUT(seq,card)
*     -- G:RI.
      If (yesnr) then
         card( 6: 8) = ' CG'
         card(10:12) = 'RI$'
         card(13:63) = text
         If (nonrbr) then
          If(dnr.gt.0.) then
            Call numout(nr, dnr, card(64:80))
          Else
            card(64:80)=nrstr
            Call formnum(nrstr,card(64:80))
          Endif
         Else
            card(64:80)=nrbrs(1:lenstr(nrbrs))//' '//dnrbrs
         End if
         seq = seq + 1
         Call fnset(3, cardg, card, seq)
      End if
 
*     -- G:TI.
      If (yesnt) then
         card( 6: 8) = ' CG'
         card(10:12) = 'TI$'
         card(13:63) = text
         If (nontbr) then
          If(dnt.gt.0.0) then
            Call numout(nt, dnt, card(64:80))
          Else
            card(64:80)=ntstr
          Endif
         Else
            card(64:80)=ntbrs(1:lenstr(ntbrs))//' '//dntbrs
         End if
         seq = seq + 1
         If (nr .eq. nt) then
            Call fnset(7, cardg, card,   0)
         Else
            Call fnset(7, cardg, card, seq)
         End if
      End if
 
      If (yesnb) then
         card( 6: 8) = ' CE'
         card(10:12) = 'IB$'
         card(13:63) = text
         If (nonbbr) then
          If(dnb.gt.0.0) then
            Call numout(nb, dnb, card(64:80))
          Else
            If (nb.eq.1.0) then
               card(64:80)='1.0'
            Else
               card(64:80)=nbstr
            Endif
          Endif
         Else
            card(64:80)=nbbrs(1:lenstr(nbbrs))//' '//dnbbrs
         End if
         seq = seq + 1
*     -- E:IB.
         Call fnset(3, carde, card, seq)
 
*     -- E:IE.
         card(10:12) = 'IE$'
         Call fnset(4, carde, card, 0)
 
*     -- E:TI.
         card(10:12) = 'TI$'
         Call fnset(7, carde, card, 0)
 
*     -- B:IB.
         card( 6: 8) = ' CB'
         card(10:12) = 'IB$'
         Call fnset(3, cardb, card, seq)
 
*     -- A:IA.
         card( 6: 8) = ' CA'
         card(10:12) = 'IA$'
         Call fnset(3, carda, card, seq)
      End if
 
*     -- P:IP.
      If (yesnp) then
         card( 6: 8) = ' CP'
         card(10:12) = 'IP$'
         card(13:63) = text
         If (nonp) then
            Call numout(np, dnp, card(64:80))
         Else
            card(64:80)=nps(1:lenstr(nps))//' '//dnps
         End if
         seq = seq + 1
         Call fnset(3, cardd, card, seq)
      End if
      Return
      End
      SUBROUTINE PNPRO(INPUT,DBMS,CARD)
C
C     Translates the new production normalization record.
C        Designed for use by all ENSDF codes
C   7-Mar-1990  For Trend where DBMS=0, DBMS read coding was eliminated
C
C     Input unit for ENSDF ASCII sequential file
      INTEGER INPUT
C     Signifies sequential or data base
      INTEGER DBMS
C     The image of CARD will be destroyed and
C        image of the first non-"nPN" record returned
C+++MDC+++
C...VAX, DVF
C/      CHARACTER*(*) CARD
C...ANS, UNX
      CHARACTER*80 CARD
C---MDC---
C+++MDC+++
C...PLT...
C/C     The following values (from N Record) may be overwritten
C/      REAL NRBRIN,DNRBRI,NTBRIN,DNTBRI,NBBRIN,DNBBRI,NPIN,DNPIN
C---MDC---
C
C     If OPTIN is greater than zero, it was set by OPTION command
C       NDSPLT command file.  If less than zero, derived by program.
C       If equal to zero don't check.  The image of OPTIN will
C       will be destroyed
      INTEGER OPTIN,OPTION
      COMMON/PNOPTI/OPTIN,OPTION
      CHARACTER*256 OPTSTR
      COMMON/PNOPTC/OPTSTR
C     Carry additional information along in COMMON's for non-general
C       use
C     Are there numeric values?
      LOGICAL NONRBR,NONTBR,NONBBR,NONP,ISCONT
      COMMON/LOGPN/NONRBR,NONTBR,NONBBR,NONP,ISCONT
C     Save the strings
      CHARACTER*2 DNRBRS,DNTBRS,DNPS
      CHARACTER*6 DNBBRS
      CHARACTER*7 NPS
      CHARACTER*8 NTBRS,NBBRS
      CHARACTER*10 NRBRS
      COMMON/PNSTR/NRBRS,DNRBRS,NTBRS,DNTBRS,NBBRS,DNBBRS,NPS,DNPS
C+++MDC+++
C...PLT...
C/C     Values for applied applications when limits are given
C/C        All values on PN Record must lie bewteen 0 and 1; therefore,
C/C        estimates may be made on uncertainties
C/      REAL NRBRA,DNRBRA,NTBRA,DNTBRA,NBBRA,DNBBRA,NPA,DNPA
C/      COMMON/PNAPL/NRBRA,DNRBRA,NTBRA,DNTBRA,NBBRA,DNBBRA,NPA,DNPA
C---MDC---
C     Messages
      INTEGER MAXMES
      PARAMETER (MAXMES=20)
      INTEGER NMES
      CHARACTER*132 MESSAG(MAXMES)
      COMMON/PNMESI/NMES
      COMMON/PNMESC/MESSAG
C
      INTEGER I,J
      INTEGER COUNT
C+++MDC+++
C...PLT...
C/      LOGICAL NUNCNR,NUNCNT,NUNCNB,NUNCNP
C/      LOGICAL DOIT
C/      LOGICAL PNDIF
C/      REAL NRBR,NTBR,NBBR,NP
C/      REAL DNRBR,DNTBR,DNBBR,DNP
C---MDC---
C
      INTEGER MAXOPT
      PARAMETER (MAXOPT=7)
      INTEGER MAXOPS
      PARAMETER (MAXOPS=48)
      CHARACTER*(MAXOPS) OPTS(MAXOPT)
      INTEGER MAXPLT
      PARAMETER (MAXPLT=5)
      INTEGER COMOPT(MAXOPT,MAXPLT)
C+++MDC+++
C...PLT...
C/      INTEGER NOPT(MAXOPT)
C/      Real    avenum
C---MDC---
C
      INTEGER IABS,IVLSTR,LENSTR
      REAL ABS,AMAX1
      EXTERNAL IVLSTR,LENSTR
      INTRINSIC ABS,AMAX1,IABS
      INTEGER LEN
      INTRINSIC LEN
      REAL X,Y
C
      DATA OPTS/'RELATIVE TI',
     2          'TI PER 100 DECAYS BY THIS BRANCH',
     3          'TI PER 100 PARENT DECAYS',
     4          'RI PER 100 PARENT DECAYS',
     5          'RELATIVE IG',
     6          'RELATIVE PHOTON BRANCHING RATIO FROM EACH LEVEL',
     7          '% PHOTON BRANCHING FROM EACH LEVEL'/
      DATA ((COMOPT(I,J),J=1,MAXPLT),I=1,MAXOPT)/ 1, 2, 3, 0, 0,
     2                                            5, 7, 8, 0, 0,
     3                                            6, 9, 4, 0, 0,
     4                                           10, 0, 0, 0, 0,
     5                                           11, 0, 0, 0, 0,
     6                                           11, 0, 0, 0, 0,
     7                                           11, 0, 0, 0, 0/
C+++MDC+++
C...PLT...
C/      DATA NOPT/1,3,5,1,1,1,1/
C/
C/      AVENUM(X,Y)=(X+Y)/2.
C---MDC---
C
50    CONTINUE
      NONRBR=.TRUE.
      NONTBR=.TRUE.
      NONBBR=.TRUE.
      NONP=.TRUE.
C+++MDC+++
C...PLT...
C/      NUNCNR=.TRUE.
C/      NUNCNT=.TRUE.
C/      NUNCNB=.TRUE.
C/      NUNCNP=.TRUE.
C/      DOIT=.TRUE.
C---MDC---
      NMES=0
      OPTSTR=' '
      OPTION=0
C
      IF(CARD(10:64) .NE. ' ')THEN
         NRBRS=CARD(10:19)
         IF(NRBRS .NE. ' ')THEN
            NONRBR=.FALSE.
         ENDIF
         DNRBRS=CARD(20:21)
         IF(DNRBRS .NE. ' ')THEN
            CALL SQZSTR(DNRBRS,' ')
C+++MDC+++
C...PLT...
C/            NUNCNR=.FALSE.
C/         ELSE
C/            IF(.NOT.NONRBR)NUNCNR=.FALSE.
C---MDC---
         ENDIF
         NTBRS=CARD(22:29)
         IF(NTBRS .NE. ' ')THEN
            NONTBR=.FALSE.
         ENDIF
         DNTBRS=CARD(30:31)
         IF(DNTBRS .NE. ' ')THEN
            CALL SQZSTR(DNTBRS,' ')
C+++MDC+++
C...PLT...
C/            NUNCNT=.FALSE.
C/         ELSE
C/            IF(.NOT.NONTBR)NUNCNT=.FALSE.
C---MDC---
         ENDIF
         NBBRS=CARD(42:49)
         IF(NBBRS .NE. ' ')THEN
            NONBBR=.FALSE.
         ENDIF
         DNBBRS=CARD(50:55)
         IF(DNBBRS .NE. ' ')THEN
            CALL SQZSTR(DNBBRS,' ')
C+++MDC+++
C...PLT...
C/            NUNCNB=.FALSE.
C/         ELSE
C/            IF(.NOT.NONBBR)NUNCNB=.FALSE.
C---MDC---
         ENDIF
         NPS=CARD(56:62)
         IF(NPS .NE. ' ')THEN
            NONP=.FALSE.
         ENDIF
         IF(CARD(63:64) .NE. ' ')THEN
            DNPS=CARD(63:64)
            CALL SQZSTR(DNPS,' ')
C+++MDC+++
C...PLT...
C/            NUNCNP=.FALSE.
C/         ELSE
C/            IF(.NOT.NONP)NUNCNP=.FALSE.
C---MDC---
         ENDIF
      ELSE
         IF(CARD(77:78) .EQ. ' ')THEN
C+++MDC+++
C...PLT...
C/            DOIT=.FALSE.
C---MDC---
            NMES=NMES+1
            IF(NMES .LE. MAXMES)THEN
               WRITE(MESSAG(NMES),1)' ***** Blank '//
     2           CARD(1:80)
1              FORMAT(A)
            ELSE
               NMES=MAXMES
               WRITE(MESSAG(NMES),3)' ***** More than ',MAXMES,
     2           ' from PNPRO'
3              FORMAT(A,I3,A)
            ENDIF
            RETURN
         ENDIF
      ENDIF
C
      OPTION=IVLSTR(CARD(78:78))
      IF(OPTION .GE. 1 .AND. OPTION .LE. MAXOPT)THEN
         OPTSTR=OPTS(OPTION)
      ELSE
         NMES=NMES+1
         IF(CARD(78:78) .NE. ' ')THEN
            IF(NMES .LE. MAXMES)THEN
               WRITE(MESSAG(NMES),1)
     2           ' ***** Undefined character in col. 78 of PN card - "'
     3           //CARD(78:78)//'"'
            ELSE
               NMES=MAXMES
               WRITE(MESSAG(NMES),3)' ***** More than ',MAXMES,
     2           ' from PNPRO'
            ENDIF
         ELSE
            OPTION=3
            OPTSTR=OPTS(OPTION)
            IF(NMES .LE. MAXMES)THEN
               WRITE(MESSAG(NMES),5)
     2           ' ***** Default=',OPTION,'(OPTION=',COMOPT(OPTION,1),
     3           ') assumed for PN card'
5              FORMAT(A,I2,A,I2,A)
            ELSE
               NMES=MAXMES
               WRITE(MESSAG(NMES),3)' ***** More than ',MAXMES,
     2           ' from PNPRO'
            ENDIF
         ENDIF
      ENDIF
      IF(OPTION .GE. 0 .AND. (OPTION .LE. 2 .OR. OPTION .GE. 5))THEN
         IF(.NOT.NONRBR)THEN
            IF(NMES .LE. MAXMES)THEN
               WRITE(MESSAG(NMES),3)
     2           ' ***** NRBR='//NRBRS//' not consistent with option=',
     2           OPTION,' from PN card.  Ignoring NRBR'
            ELSE
               NMES=MAXMES
               WRITE(MESSAG(NMES),3)' ***** More than ',MAXMES,
     2           ' from PNPRO'
            ENDIF
            NONRBR=.TRUE.
         ENDIF
         IF(.NOT.NONTBR)THEN
            IF(NMES .LE. MAXMES)THEN
               WRITE(MESSAG(NMES),3)
     2           ' ***** NTBR='//NTBRS//' not consistent with option=',
     2           OPTION,' from PN card.  Ignoring NTBR'
            ELSE
               NMES=MAXMES
               WRITE(MESSAG(NMES),3)' ***** More than ',MAXMES,
     2           ' from PNPRO'
            ENDIF
            NONTBR=.TRUE.
         ENDIF
      ENDIF
      IF(OPTION .EQ. 4 .AND. .NOT.NONTBR)THEN
         IF(NMES .LE. MAXMES)THEN
            WRITE(MESSAG(NMES),3)
     2        ' ***** NTBR='//NTBRS//' not consistent with option=',
     2        OPTION,' from PN card.  Ignoring NTBR'
         ELSE
            NMES=MAXMES
            WRITE(MESSAG(NMES),3)' ***** More than ',MAXMES,
     2        ' from PNPRO'
         ENDIF
         NONTBR=.TRUE.
      ENDIF
C
      IF(CARD(77:77) .EQ. 'C')THEN
         ISCONT=.TRUE.
      ELSE
         ISCONT=.FALSE.
         IF(CARD(77:77) .NE. ' ')THEN
            NMES=NMES+1
            IF(NMES .LE. MAXMES)THEN
               WRITE(MESSAG(NMES),1)
     2           ' ***** Undefined character in col. 77 of PN card - '
     3           //CARD(77:77)
            ELSE
               NMES=MAXMES
               WRITE(MESSAG(NMES),3)' ***** More than ',MAXMES,
     2           ' from PNPRO'
            ENDIF
         ENDIF
      ENDIF
      COUNT=0
1000  CONTINUE
C
C   For TREND, assume always DBMS=0.
C
      READ(UNIT=INPUT,FMT='(A)',END=1100)CARD
      IF(.NOT.ISCONT .AND. CARD(7:8) .NE. 'PN')THEN
          RETURN
      ELSE
         IF(.NOT.ISCONT)THEN
            NMES=NMES+1
            IF(NMES .LE. MAXMES)THEN
               WRITE(MESSAG(NMES),1)' ***** PN or nPN found. '//
     2           CARD(1:80)
            ELSE
               NMES=MAXMES
               WRITE(MESSAG(NMES),3)' ***** More than ',MAXMES,
     2           ' from PNPRO'
            ENDIF
            GOTO 1000
         ENDIF
      ENDIF
      IF(CARD(7:8) .EQ. 'PN' .AND. CARD(6:6) .EQ. ' ')THEN
         NMES=NMES+1
         IF(NMES .LE. MAXMES)THEN
            WRITE(MESSAG(NMES),1)' ***** Another  PN. '//
     2        CARD(1:80)
         ELSE
            NMES=MAXMES
            WRITE(MESSAG(NMES),3)' ***** More than ',MAXMES,
     2        ' from PNPRO'
         ENDIF
         GOTO 50
      ENDIF
      IF(CARD(7:8) .EQ. 'PN' .AND. CARD(6:6) .NE. ' ')THEN
         COUNT=COUNT+1
         CALL LBSUP(CARD(10:80))
         IF(COUNT .EQ. 1)THEN
            OPTSTR=CARD(10:80)
         ELSE
            IF(LENSTR(OPTSTR)+1 .GT. LEN(OPTSTR))THEN
               NMES=NMES+1
               IF(NMES .LE. MAXMES)THEN
                  WRITE(MESSAG(NMES),1)' ***** Too many nPN. '//
     2              CARD(1:80)
               ELSE
                  NMES=MAXMES
                  WRITE(MESSAG(NMES),3)' ***** More than ',MAXMES,
     2              ' from PNPRO'
               ENDIF
            ELSE
               IF(LENSTR(OPTSTR)+1+LENSTR(CARD(10:80))
     2           .GT. LEN(OPTSTR))THEN
                  NMES=NMES+1
                  IF(NMES .LE. MAXMES)THEN
                     WRITE(MESSAG(NMES),1)' ***** nPN truncated. '//
     2                  CARD(LEN(OPTSTR)-LENSTR(OPTSTR)-1:LENSTR(CARD))
                  ELSE
                     NMES=MAXMES
                     WRITE(MESSAG(NMES),3)' ***** More than ',MAXMES,
     2                 ' from PNPRO'
                  ENDIF
               ENDIF
             CALL ADDSTR(OPTSTR,LENSTR(OPTSTR)+1,CARD(10:lenstr(card)))
            ENDIF
         ENDIF
         GOTO 1000
      ENDIF
1100  CONTINUE
      RETURN
C+++MDC+++
C...PLT...
C/C
C/      ENTRY PNNOR(NRBRIN,DNRBRI,NTBRIN,DNTBRI,NBBRIN,DNBBRI,
C/     2  NPIN,DNPIN,PNDIF)
C/C     Entry point for calculating and comparing normalizations
C/      IF(.NOT.DOIT)RETURN
C/      PNDIF=.NOT.(NONRBR .AND. NONTBR .AND. NONBBR .AND. NONP)
C/      IF(.NOT.PNDIF)RETURN
C/      NMES=0
C/      IF(.NOT.NONRBR)THEN
C/         CALL CNVS2U(NRBRS,DNRBRS,NRBR,DNRBR)
C/         IF(ABS(NRBR-NRBRIN) .GT. AMAX1(DNRBR,DNRBRI))THEN
C/            NMES=NMES+1
C/            IF(NMES .LE. MAXMES)THEN
C/               WRITE(MESSAG(NMES),2)' ***** NRBR discrepant  ',
C/     2           NRBRIN,DNRBRI,NRBR,DNRBR
C/2                FORMAT(A,4(2X,E11.4))
C/            ELSE
C/               NMES=MAXMES
C/               WRITE(MESSAG(NMES),3)' ***** More than ',MAXMES,
C/     2           ' from PNNOR'
C/            ENDIF
C/         ENDIF
C/         NRBRIN=NRBR
C/         DNRBRI=DNRBR
C/         IF(DNRBRS(1:1) .GE. '1' .AND. DNRBRS(1:1) .LE. '9')THEN
C/            NRBRA=NRBR
C/            DNRBRA=DNRBR
C/         ELSE
C/            IF(DNRBRS(1:1) .EQ. 'G' .OR. DNRBRS(1:1) .EQ. 'L')THEN
C/               IF(DNRBRS(1:1) .EQ. 'G')NRBRA=AVENUM(1.,NRBR)
C/               IF(DNRBRS(1:1) .EQ. 'L')NRBRA=AVENUM(0.,NRBR)
C/               DNRBRA=ABS(NRBRA-NRBR)
C/            ELSE
C/               NRBRA=NRBR
C/               DNRBRA=0.
C/            ENDIF
C/         ENDIF
C/      ELSE
C/         IF(.NOT.NUNCNR)THEN
C/            NMES=NMES+1
C/            IF(NMES .LE. MAXMES)THEN
C/               WRITE(MESSAG(NMES),1)
C/     2           ' ***** Non-blank UNC but blank NRBR.'//
C/     3           '  Ignoring'
C/            ELSE
C/               NMES=MAXMES
C/               WRITE(MESSAG(NMES),3)' ***** More than ',MAXMES,
C/     2           ' from PNNOR'
C/            ENDIF
C/         ENDIF
C/      ENDIF
C/      IF(.NOT.NONTBR)THEN
C/         CALL CNVS2U(NTBRS,DNTBRS,NTBR,DNTBR)
C/         IF(ABS(NTBR-NTBRIN) .GT. AMAX1(DNTBR,DNTBRI))THEN
C/            NMES=NMES+1
C/            IF(NMES .LE. MAXMES)THEN
C/               WRITE(MESSAG(NMES),2)' ***** NTBR discrepant',
C/     2           NTBRIN,DNTBRI,NTBR,DNTBR
C/            ELSE
C/               NMES=MAXMES
C/               WRITE(MESSAG(NMES),3)' ***** More than ',MAXMES,
C/     2           ' from PNNOR'
C/            ENDIF
C/         ENDIF
C/         NTBRIN=NTBR
C/         DNTBRI=DNTBR
C/         IF(DNTBRS(1:1) .GE. '1' .AND. DNTBRS(1:1) .LE. '9')THEN
C/            NTBRA=NTBR
C/            DNTBRA=DNTBR
C/         ELSE
C/            IF(DNTBRS(1:1) .EQ. 'G' .OR. DNTBRS(1:1) .EQ. 'L')THEN
C/               IF(DNTBRS(1:1) .EQ. 'G')NTBRA=AVENUM(1.,NTBR)
C/               IF(DNTBRS(1:1) .EQ. 'L')NTBRA=AVENUM(0.,NTBR)
C/               DNTBRA=ABS(NTBRA-NTBR)
C/            ELSE
C/               NTBRA=NTBR
C/               DNTBRA=0.
C/            ENDIF
C/         ENDIF
C/      ELSE
C/         IF(.NOT.NUNCNT)THEN
C/            NMES=NMES+1
C/            IF(NMES .LE. MAXMES)THEN
C/               WRITE(MESSAG(NMES),1)
C/     2           ' ***** Non-blank UNC but blank NTBR.'//
C/     3           '  Ignoring'
C/            ELSE
C/               NMES=MAXMES
C/               WRITE(MESSAG(NMES),3)' ***** More than ',MAXMES,
C/     2           ' from PNNOR'
C/            ENDIF
C/         ENDIF
C/      ENDIF
C/      IF(.NOT.NONBBR)THEN
C/         CALL CNVS2U(NBBRS,DNBBRS,NBBR,DNBBR)
C/         IF(ABS(NBBR-NBBRIN) .GT. AMAX1(DNBBR,DNBBRI))THEN
C/            NMES=NMES+1
C/            IF(NMES .LE. MAXMES)THEN
C/               WRITE(MESSAG(NMES),2)' ***** NBBR discrepant',
C/     2           NBBRIN,DNBBRI,NBBR,DNBBR
C/            ELSE
C/               NMES=MAXMES
C/               WRITE(MESSAG(NMES),3)' ***** More than ',MAXMES,
C/     2           ' from PNNOR'
C/            ENDIF
C/         ENDIF
C/         NBBRIN=NBBR
C/         DNBBRI=DNBBR
C/         IF(DNBBRS(1:1) .GE. '1' .AND. DNBBRS(1:1) .LE. '9')THEN
C/            NBBRA=NBBR
C/            DNBBRA=DNBBR
C/         ELSE
C/            IF(DNBBRS(1:1) .EQ. 'G' .OR. DNBBRS(1:1) .EQ. 'L')THEN
C/               IF(DNBBRS(1:1) .EQ. 'G')NBBRA=AVENUM(1.,NBBR)
C/               IF(DNBBRS(1:1) .EQ. 'L')NBBRA=AVENUM(0.,NBBR)
C/               DNBBRA=ABS(NBBRA-NBBR)
C/            ELSE
C/               NBBRA=NBBR
C/               DNBBRA=0.
C/            ENDIF
C/         ENDIF
C/      ELSE
C/         IF(.NOT.NUNCNB)THEN
C/            NMES=NMES+1
C/            IF(NMES .LE. MAXMES)THEN
C/               WRITE(MESSAG(NMES),1)
C/     2         ' ***** Non-blank UNC but blank NBBR.'//
C/     3         '  Ignoring'
C/            ELSE
C/               NMES=MAXMES
C/               WRITE(MESSAG(NMES),3)' ***** More than ',MAXMES,
C/     2           ' from PNNOR'
C/            ENDIF
C/         ENDIF
C/      ENDIF
C/      IF(.NOT.NONP)THEN
C/         CALL CNVS2U(NPS,DNPS,NP,DNP)
C/         IF(ABS(NP-NPIN) .GT. AMAX1(DNP,DNPIN))THEN
C/            NMES=NMES+1
C/            IF(NMES .LE. MAXMES)THEN
C/               WRITE(MESSAG(NMES),2)' ***** NP discrepant',
C/     2           NPIN,DNPIN,NP,DNP
C/            ELSE
C/               NMES=MAXMES
C/               WRITE(MESSAG(NMES),3)' ***** More than ',MAXMES,
C/     2           ' from PNNOR'
C/            ENDIF
C/         ENDIF
C/         NPIN=NP
C/         DNPIN=DNP
C/         IF(DNPS(1:1) .GE. '1' .AND. DNPS(1:1) .LE. '9')THEN
C/            NPA=NP
C/            DNPA=DNP
C/         ELSE
C/            IF(DNPS(1:1) .EQ. 'G' .OR. DNPS(1:1) .EQ. 'L')THEN
C/               IF(DNPS(1:1) .EQ. 'G')NPA=AVENUM(1.,NP)
C/               IF(DNPS(1:1) .EQ. 'L')NPA=AVENUM(0.,NP)
C/               DNPA=ABS(NPA-NP)
C/            ELSE
C/               NPA=NP
C/               DNPA=0.
C/            ENDIF
C/         ENDIF
C/      ELSE
C/         IF(.NOT.NUNCNP)THEN
C/            NMES=NMES+1
C/            IF(NMES .LE. MAXMES)THEN
C/               WRITE(MESSAG(NMES),1)
C/     2           ' ***** Non-blank UNC but blank NP.'//
C/     3           '  Ignoring'
C/            ELSE
C/               NMES=MAXMES
C/               WRITE(MESSAG(NMES),3)' ***** More than ',MAXMES,
C/     2           ' from PNNOR'
C/            ENDIF
C/         ENDIF
C/      ENDIF
C/      RETURN
C/C
C/      ENTRY PNOPT
C/C     Entry point for getting and comparing options
C/      IF(.NOT.DOIT)RETURN
C/      NMES=0
C/      IF(OPTIN .NE. 0)THEN
C/         DO 2000 I=1,NOPT(OPTION)
C/            IF(COMOPT(OPTION,I) .EQ. IABS(OPTIN))THEN
C/               OPTIN=IABS(OPTIN)
C/               RETURN
C/            ENDIF
C/2000     CONTINUE
C/         NMES=NMES+1
C/         IF(NMES .LE. MAXMES)THEN
C/            IF(OPTIN .GT. 0)THEN
C/               WRITE(MESSAG(NMES),4)
C/     2           ' ***** Option on PN differs from OPTION',
C/     3           OPTIN, ' :', (COMOPT(OPTION,I),I=1,NOPT(OPTION))
C/4              FORMAT(A,I3,A,3(I3))
C/            ELSE
C/               WRITE(MESSAG(NMES),4)
C/     2           ' ***** Option on PN differs from derived',
C/     3           IABS(OPTIN), ' :', (COMOPT(OPTION,I),I=1,NOPT(OPTION)
C/            ENDIF
C/         ELSE
C/            NMES=MAXMES
C/            WRITE(MESSAG(NMES),3)' ***** More than ',MAXMES,
C/     2        ' from PNOPT'
C/         ENDIF
C/         OPTIN=COMOPT(OPTION,1)
C/      ENDIF
C/      RETURN
C---MDC---
      END
       Subroutine FSORT (iunit)
 
************************************************************************
*                                                                      *
*  Sort the file specified by iunit.                                   *
*                                                                      *
*  This is an external sort. The input file must be closed before      *
*     calling FSORT. The sorted data will be returned in the same file *
*     as the input, but in sorted order. The file will be closed on    *
*     exit.                                                            *
*                                                                      *
*  The alogorithm used is Multiway Merging and Replacement Selection   *
*     (see The Art of Computer Programming - Volume 3 / Sorting and    *
*     Searching by Donald E. Knuth, Addison-Wesley Publishing Co.,     *
*     1973).                                                           *
*                                                                      *
*  This implementation uses a tree of losers to organize the data in a *
*     buffer array to minimize the time it takes to find the least     *
*     element of the buffer to send out to the temporary file.         *
*                                                                      *
*  The merge phase uses a repeated two into two method to merge the    *
*     runs down to two runs which are finally merged back into the     *
*     user's file.                                                     *
*                                                                      *
*  There are various parameters which may be varied at compile time to *
*     either adjust the performance (i.e., mbuf, the number of records *
*     stored in main memory at any time (the number of leaves in the   *
*     sort tree)) or tailor the routine for other applications.        *
*                                                                      *
*  To simplify the implementation, it is required that the sort key be *
*     the first n characters of the record (n as appropriate for the   *
*     application) and that this key will be sorted in the inherent    *
*     character set of the host machine as a simple string of n char-  *
*     acters.                                                          *
*                                                                      *
************************************************************************
 
************************************************************************
*                                                                      *
*  Author:              Bruce J. Barton                                *
*                                                                      *
*  Please direct any questions or comments to:                         *
*  Installation:        National Nuclear Data Center                   *
*                       Brookhaven National Laboratory                 *
*                       Bldg. 197D                                     *
*                       Upton, New York  11973                         *
*                       (631) 344-2901                                 *
*                                                                      *
*  Revision history:                                                   *
*                                                                      *
*   1(00) 17-NOV-83. First release (Fortran-77 standard).              *
*                                                                      *
************************************************************************
 
*  Subroutine arguments.
 
      Integer           iunit
*                          Fortran logical unit number of file to be
*                          sorted.
 
*  Function references.
 
      Logical           odd
*                          Statement function, .true. if arg is odd.
 
*  Parameters.
 
      Integer           filelo, filehi
      Parameter        (filelo = 31, filehi = 36)
*                          Fortran logical unit numbers for files to
*                          be sorted.
      Integer           filbas
      Parameter        (filbas = filehi + 1)
*                          Fortran logical unit number of first temp
*                          file. There will be 4 such files.
      Integer           mbuf
      Parameter        (mbuf = 96)
*                          Number of leaves in sort tree.
*                          Performance consideration.
      Integer           mkey
      Parameter        (mkey = 18)
*                          Maximum size of key field.
*                          Application dependent.
      Integer           mrec
      Parameter        (mrec = 111)
*                          Maximum size of data record.
*                          Application dependent.
 
*  Local variables.
 
      Character*(mrec)  buffer(mbuf)
*                          Internal buffer of sort records.
*                          These are the leaves of the sort tree.
      Integer           cop, copret
*                          Label of copy internal subroutine,
*                          and its return label.
      Integer           ieof
*                          Eof flag for input file.
      Logical           eof
*                          Logical eof flag for input file.
      Integer           filin
*                          For merge,  input file unit number.
      Integer           filout
*                          For merge, output file unit number.
      Integer           filsw
*                          For merge, file switch (0 or 2).
*                          Controls which temp files are input and which
*                          are output.
      Character*(mkey)  flag
*                          Used to indicate no more data in sort tree.
*                          Also used to indicate end of run in temp
*                          files.
      Integer           i
*                          Miscellaneous counter uses.
      Character*(mrec)  inprec
*                          Buffer for input records.
      Equivalence      (inprec, merge(0))
*                          Will share space with merge records.
      Integer           jrun
*                          Do loop counter for merge runs.
      Integer           keysiz
*                          Size of sort key, based on iunit.
      Character*(mrec)  merge(0:1)
*                          Merge data buffers.
      Integer           mrg, mrgret
*                          Label of merge internal subroutine,
*                          and its return label.
      Integer           nbuf
*                          Number of sort records currently in buffer.
*                          Also do loop counter and buffer index.
      Integer           nrun
*                          Number of merge runs.
      Logical           nxtrun(mbuf)
*                          Next run indicator.
      Integer           recsiz
*                          Size of sort record, based on iunit.
      Integer           t
*                          Temporary for swapping winner/loser in sort.
      Integer           tree(0:mbuf-1)
*                          Tree of losers for sort phase.
*                          Pointer table.
      Integer           win
      Equivalence      (win, tree(0))
*                          Pointer to winner.
*                          Alias for top of tree.
      Character*(mkey)  winner
*                          Winning key value.
 
*  Data initializations.
 
      Data              flag /'999999999999999'/
*                          For numeric keys use 9's.
*                          For alpha keys use Z's.
*                          Application dependent.
 
*  Format statements.
 
    1 Format(A)
 
*  Statement functions.
 
      odd(i) = mod(i, 2) .eq. 1
*     -- .true. if i is odd, .false. if even.
 
************************************************************************
 
*  Determine various sort parameters.
*  Application dependent.
 
      recsiz = mrec
      keysiz = mkey
 
*  Rewind input file, rewind scratch files, and set file parameters.
*     -- Input/output file.
      Rewind (unit=iunit)
      Do 10 filout = filbas, filbas+3
*        -- Temporary merging files. Opened in the main program.
         Rewind (unit=filout)
         Rewind (unit=filout)
   10 Continue
      filout = filbas
*     -- First output file is filbas.
      nrun = 1
 
*  Read in start of data to fill buffer.
 
      Read (unit=iunit, fmt=1, iostat=ieof, end=11) inprec
   11   continue
        eof=.false.
        If (ieof.lt.0) eof=.true.
        If(inprec(1:4).eq.'9999') eof=.true.
      Do 20 nbuf = 1, mbuf
         If (eof) then
*           -- Since eof, flag infinite value (also next run status).
            buffer(nbuf) = flag(1:keysiz)
            nxtrun(nbuf) = .true.
         Else
            buffer(nbuf) = inprec
            nxtrun(nbuf) = .false.
            Read (unit=iunit, fmt=1, iostat=ieof, end=21) inprec
   21       Continue
                eof=.false.
                If (ieof.lt.0) eof=.true.
                If (inprec(1:4).eq.'9999') eof=.true.
         End if
   20 Continue
 
*  Set up loser tree initially.
 
   30 Do 33 i = 0, mbuf-1
*        -- Clear the pointers out.
         tree(i) = 0
   33 Continue
      Do 39 nbuf = 1, mbuf, 2
*        -- For each buffer pair ...
         i = mbuf / 2 + nbuf / 2
*        -- i is index into tree of father.
         If (buffer(nbuf)(1:keysiz) .le. buffer(nbuf+1)(1:keysiz)) then
*           -- Winner is nbuf, store winner and loser pointers.
            win = nbuf
            tree(i) = nbuf + 1
         Else
*           -- Winner is nbuf+1, store winner and loser pointers.
            win = nbuf + 1
            tree(i) = nbuf
         End if
*        -- Now, percolate the winner up the tree.
         winner(1:keysiz) = buffer(win)(1:keysiz)
   35    i = i / 2
         If (i .gt. 0) then
            If (tree(i) .eq. 0) then
               tree(i) = win
            Else
*              -- We have to keep percolating.
               If (winner(1:keysiz) .gt. buffer(tree(i))(1:keysiz)) then
*                 -- We lose comparison, swap winner and loser.
                  t = win
                  win = tree(i)
                  tree(i) = t
                  winner(1:keysiz) = buffer(win)(1:keysiz)
               End if
               Goto 35
            End if
         End if
   39 Continue
 
*  Write out the winner and replace its buffer.
*  Loop until no more winners.
 
   40 If (.not. nxtrun(win)) then
         Write (unit=filout, fmt=1) buffer(win)(1:recsiz)
         If (eof) then
*           -- Flag infinity and set next run indicator.
            buffer(win) = flag(1:keysiz)
            nxtrun(win) = .true.
         Else
*           -- Copy next buffer, determine next run status.
            buffer(win) = inprec
            Read (unit=iunit, fmt=1, iostat=ieof, end=41) inprec
   41           continue
                eof=.false.
                If (ieof.lt.0) eof=.true.
                If(inprec(1:4).eq.'9999') eof = .true.
            If (buffer(win)(1:keysiz) .lt. winner(1:keysiz))
     +         nxtrun(win) = .true.
         End if
*        -- Percolate the winner to the top.
         winner(1:keysiz) = buffer(win)(1:keysiz)
         i = mbuf / 2 + (win - 1) / 2
*        -- i is index into tree of father.
   46    If (i .gt. 0) then
            If (nxtrun(tree(i))) then
*              -- Always win against next run.
*              -- Since we're already winner, no need to swap.
            Else If (nxtrun(win) .or.
     +         (winner(1:keysiz) .gt. buffer(tree(i))(1:keysiz))) then
*              -- Next run implies lose.
*              -- So does loss of comparison.
*              -- Swap winner and loser.
               t = win
               win = tree(i)
               tree(i) = t
               winner(1:keysiz) = buffer(win)(1:keysiz)
            End if
            i = i / 2
*           -- Now check father.
            Goto 46
         End if
         Goto 40
      End if
*  End this run, check for next run data.
*  If there is some, start another run, else end sort phase.
 
      Write (unit=filout, fmt=1) flag(1:keysiz)
*     -- End of run sentinal.
      i = 0
*     -- i is used here to tell us if there are any more data records.
      Do 50 nbuf = 1, mbuf
         If (buffer(nbuf)(1:keysiz) .ne. flag(1:keysiz)) then
            nxtrun(nbuf) = .false.
*           -- Clear next run indicator for next pass.
            i = -1
*           -- Signal more data.
         End if
   50 Continue
      If (i .ne. 0) then
*        -- There is more data to process.
         nrun = nrun + 1
*        -- Set for next run.
         filout = 1 - (filout - filbas) + filbas
*        -- Change output file to alternate.
         Goto 30
      End if
 
*  End of sort phase, rewind iunit and sorted runs.
*     Input file will now be used for output. Mark its beginning.
      Rewind iunit
*     Sorted files will now be used for input.
      Rewind filbas
      Rewind filbas+1
 
*  Merge the runs down to 1 or 2.
 
      filin = filbas
      filsw = 2
*     -- filsw points to output side.
      filout = filbas + filsw
   55 If (nrun .gt. 2) then
*        -- If more than 2 runs, merge to temporary output.
         Do 62 jrun = 1, nrun / 2
*           -- For each merge pass there are nrun / 2 run pairs
*           -- to be merged.
         Call Mergefils(filin,filout,keysiz,recsiz,flag,merge)
   60       Write (unit=filout, fmt=1) flag(1:keysiz)
*           -- Write end of run sentinal.
            filout = 1 - (filout - (filbas + filsw)) + filbas + filsw
*           -- Set up for other output file.
   62    Continue
         If (odd(nrun)) then
*           -- If there are an odd number of runs,
*           -- copy final run to filout.
            Read (unit=filin, fmt=1,end=64) merge(0)
            If(merge(0)(1:keysiz).eq.flag(1:keysiz)) Go to 64
            Call Copy(filin,filout,keysiz,recsiz,flag,merge)
   64       Write (unit=filout, fmt=1) flag(1:keysiz)
*           -- Write end of run sentinal.
         End if
*        -- After this merge pass, mark their ends and rewind all temp
*           files.
         Do 66 filout = filbas, filbas+3
            Rewind filout
   66    Continue
*        -- Swap input / output pointers
*        -- (I.e., input now output and output now input.)
         filin = filbas + filsw
         filsw = 2 - filsw
         filout = filbas + filsw
         nrun = (nrun + 1) / 2
*        -- This pass has half as many runs as previous pass.
         Goto 55
      End if
 
*  We now have 1 or 2 runs left, copy (or merge) to iunit.
 
      filout = iunit
      If (nrun .eq. 1) then
*        -- Only 1 run, copy it.
         Read (unit=filin, fmt=1,end=70) merge(0)
         If(merge(0)(1:keysiz).eq.flag(1:keysiz)) Go to 70
         Call Copy(filin,filout,keysiz,recsiz,flag,merge)
      Else
*        -- Two runs, merge.
         Call Mergefils(filin,filout,keysiz,recsiz,flag,merge)
      End if
 
*  Rewind, close files, and return.
 
   70 Continue
*     Iunit will now be used for input. Mark its end and rewind.
      Write(unit=iunit,fmt='(''9999'')')
      Rewind (unit=iunit)
      Return
 
*     Error reading files in the merge section.
   88 Continue
      Write(unit=6,fmt=
     *'('' *** Unexpected end of file in FSORT merge/copy'',I4)') ios
      Stop
      End
 
      Subroutine Mergefils(filin,filout,keysiz,recsiz,flag,merge)
 
      Integer filin,filout,keysiz,recsiz
 
      Character*(*) flag,merge(0:1)
 
*  Format statements.
 
    1 Format(A)
 
*  Internal procedure MERGE
 
   80 Read (unit=filin,   fmt=1,end=88) merge(0)
      Read (unit=filin+1, fmt=1,end=88) merge(1)
   85 Continue
      If (merge(0)(1:keysiz) .eq. flag(1:keysiz)) then
*        -- End of run from input file 0.
*        -- Copy rest of run from file 1.
         filin = filin + 1
         merge(0) = merge(1)
         Call Copy(filin,filout,keysiz,recsiz,flag,merge)
   87    filin = filin - 1
         Return
      Else If (merge(1)(1:keysiz) .eq. flag(1:keysiz)) then
*        -- End of run from input file 1.
*        -- Copy rest of run from file 0.
         Call Copy(filin,filout,keysiz,recsiz,flag,merge)
         Return
      Else
*        -- Use least entry.
         If (merge(0)(1:keysiz) .le. merge(1)(1:keysiz)) then
            Write (unit=filout, fmt=1) merge(0)(1:recsiz)
            Read  (unit=filin,  fmt=1,end=88) merge(0)
            Goto 85
         Else
            Write (unit=filout, fmt=1) merge(1)(1:recsiz)
            Read (unit=filin+1, fmt=1,end=88) merge(1)
            Goto 85
         End if
      End if
*     Error reading files in the merge section.
   88 Continue
      Write(unit=6,fmt=
     *'('' *** Unexpected end of file in FSORT merge'')')
      Stop
      End
 
      Subroutine Copy(filin,filout,keysiz,recsiz,flag,merge)
 
      Integer filin,filout,keysiz,recsiz
 
      Character*(*) flag, merge(0:1)
 
*  Format statements.
 
    1 Format(A)
 
*  Internal procedure COPY.
   90 If (merge(0)(1:keysiz) .ne. flag(1:keysiz)) then
         Write (unit=filout, fmt=1) merge(0)(1:recsiz)
         Read  (unit=filin,  fmt=1,end=88) merge(0)
         Goto 90
      End if
      Return
*     Error reading files in the merge section.
   88 Continue
      Write(unit=6,fmt=
     *'('' *** Unexpected end of file in FSORT copy'')')
      Stop
 
      End
 
      Subroutine RPT (icrd)
 
************************************************************************
*  Produce a tabular report for this card type.                        *
************************************************************************
 
*  Subroutine arguments.
 
      Integer           icrd
 
      Integer           nclmn
      Common   /column/ nclmn
 
*  Function references.
 
      Integer           lenstr,INDEXF
      Integer           span
      Integer           typstr
      External          lenstr,INDEXF,span,typstr
 
 
*  Parameters.
 
      Integer           nfld, ncrd
      Parameter        (nfld = 8, ncrd = 6)
      Integer           maxcod
      Parameter        (maxcod = 40)
      Integer           filbas
      Parameter        (filbas = 30)
      Integer           cardl, carda, cardd, cardb, carde, cardg
      Parameter        (cardl =  1, carda =  2, cardd =  3)
      Parameter        (cardb =  4, carde =  5, cardg =  6)
 
*  Common blocks.
 
      Logical           adopt
*                          True if data set is adopted.
*                          Used to not sort adopted gammas.
      Integer           gamord
*                          Gamord flag from PN card.
      Integer           col1, col2
*                          Ending column numbers for Eg and E(level).
*                          Used to reorder Eg and E(level) columns.
      Common   /adgam/  adopt, gamord, col1, col2
 
      Integer           fldpos(nfld, ncrd, 3)
      Common   /fldpos/ fldpos
 
      Integer           fldwid(nfld, ncrd, 3)
      Common   /fldwid/ fldwid
 
      Character*80      idcard,idcont(20)
      Common   /idcard/ idcard,idcont
      Integer  ncont
      Common   /idcont/ ncont
 
      Integer           lincnt,linpag
      Common   /lincnt/ lincnt,linpag
 
      Integer           nxcrd
      Character*1       xflg(50)
      Common/  flgord  /nxcrd, xflg
 
*  Local variables.
 
      Character*50      adgtmp
      Character*80      card ,crdtmp
      Logical           contin
      Logical           contlv
      Logical           newpag
      Real              e
      Character*13      elev
      Character*80      fcard
      Character*40      flgstr
      Character*70      xrfstr, negstr
      Character*(maxcod)fncode
      Character*42      fnote
      Integer           LN,L,LL,IFIELD,LNG
      Integer           i, j, k, ij
      Integer           icode
      Integer           ifld
      Integer           iposx, iposd, iposr
      Integer           iunit
      Integer           jposx, jposd, jposr
      Integer           lrpt
      Character*396     rptlin,rptln2
      Integer           seq
      Integer           type
      Logical           xst
      Logical           ieof
      Logical           outtab
      integer           comspa
      integer           last, m, n
      INTEGER           COMLEN,LEN
      LOGICAL           COMNXT
      CHARACTER*80      CARD2
      CHARACTER*255     COMBUF,TMPBUF
      INTEGER           DIFER,FLDSIZ
      LOGICAL           COMFLD
      Save
 
*  Data initializations.
 
      Data fncode /'*#@%&abcdefghijklmnopqrstuvwyz0123456789'/
 
*  Format statements.
 
    1 Format(I4, F9.3, I5, A13, A80)
    2 Format(26X, A)
 
************************************************************************
 
*  Process report id.
      Call rptid(icrd)
*  Rewind input file.
      iunit = filbas + icrd
      Rewind (unit=iunit)
      contin = .false.
      ieof = .false.
      outtab=.FALSE.
      rptln2=' '
      adgtmp=' '

*  Process table comments.
   10 Read (unit=iunit, fmt=1, end=99) type, e, seq, elev, card
      If(Lenstr(rptlin).GT.0 .AND. card(6:7).NE.'+T'
     2  .AND. card(6:7).NE.'+t')Then
         outtab=.TRUE.
         GoTo 12
      EndIf
11    Continue
      If(type.eq.9999) Go to 99
      If (type .ge. 2000) Goto 20
C     Prepare rptlin for output.
      If (type .eq. 1) then
*        -- Q card and comments.
         If (card(7:7) .eq. ' ') then
*           -- Q card.
            rptlin = ' '
            lrpt = 0
*           -- Process each non-blank field.
            If (card(10:19) .ne. ' ') then
               rptlin(1:9) = 'Q(beta-)='
               Call pakxdx(card(10:19), card(20:21),
     *            rptlin(10:LEN(RPTLIN)))
               lrpt = lenstr(rptlin)
            End if
            If (card(22:29) .ne. ' ') then
               If (lrpt .ne. 0) then
                  rptlin(lrpt+1:lrpt+2) = '; '
                  lrpt = lrpt + 2
               End if
               rptlin(lrpt+1:lrpt+5) = 'S(n)='
               lrpt = lrpt + 5
               Call pakxdx(card(22:29), card(30:31),
     *            rptlin(lrpt+1:LEN(RPTLIN)))
               lrpt = lenstr(rptlin)
            End if
            If (card(32:39) .ne. ' ') then
               If (lrpt .ne. 0) then
                  rptlin(lrpt+1:lrpt+2) = '; '
                  lrpt = lrpt + 2
               End if
               rptlin(lrpt+1:lrpt+5) = 'S(p)='
               lrpt = lrpt + 5
               Call pakxdx(card(32:39), card(40:41),
     *            rptlin(lrpt+1:LEN(RPTLIN)))
               lrpt = lenstr(rptlin)
            End if
            If (card(42:49) .ne. ' ') then
               If (lrpt .ne. 0) then
                  rptlin(lrpt+1:lrpt+2) = '; '
                  lrpt = lrpt + 2
               End if
               rptlin(lrpt+1:lrpt+9) = 'Q(alpha)='
               lrpt = lrpt + 9
               Call pakxdx(card(42:49), card(50:55),
     *            rptlin(lrpt+1:LEN(RPTLIN)))
               lrpt = lenstr(rptlin)
            End if
            If (card(56:80) .ne. ' ') then
               rptlin(lrpt+1:LEN(RPTLIN)) = '  ' // card(56:80)
            End if
         Else
*           -- Q card comment.
            Call qpncom(rptlin, card)
         End if
C
      Else if (type .eq. 2 .or. type .eq. 3) then
*        -- General comments and references.
         If (card(8:8) .eq. 'R') then
*           -- Reference card.
            If (card(6:6) .eq. ' ') then
*              -- First reference card.
               If(card(12:12).ge.'A'.and.card(12:12).le.'Z') then
                  rptlin = card(10:15)
                  Call locase(rptlin(4:4))
                  rptlin(10:LEN(RPTLIN)) = card(span(card,16,' '):80)
               Else
                  rptlin = card(10:17)
                  Call locase(rptlin(6:6))
                  rptlin(10:LEN(RPTLIN)) = card(span(card,18,' '):80)
               Endif
            Else
*              -- Continuation reference card.
               rptlin = ' '
               rptlin(10:LEN(RPTLIN)) = card(span(card,10,' '):80)
            End if
         Else
*           -- General comment card.
            If (card(6:6) .eq. ' ') then
*              -- First comment card.
               If (card(7:7).eq.'T' .OR. card(7:7).EQ.'t') then
                  rptlin=card(10:80)
                  GoTo 10
               ELSE IF (CARD(7:7).EQ.'U') THEN
C                 ---Update comment
                  RPTLIN = 'Update C:'//CARD(10:80)
               ELSE IF (CARD(7:7).EQ.'P') THEN
C                 -- publication comment
                  CALL PUBCRD(CARD,CRDTMP)
                  RPTLIN='Pub C:'//CRDTMP(10:80)
               Else
                  If (card(10:80) .eq. ' ') then
                     rptlin = ' '
                  Else
C                    If $ then k=next character loc. If not, k=20
                     Call Brksym(card,j,k)
                     If(k.eq.20) k = span(card, 10, ' ')
                     rptlin = card(k:80)
                  End if
               End if
            Else
*              -- Continuation comment card.
               If (card(7:7).eq.'T' .OR. card(7:7).EQ.'t') then
                  If(card(6:6) .EQ. '+')Then
                     If(Lenstr(rptlin) .GT. 0)Then
                        rptlin=rptlin(1:Lenstr(rptlin))//card(10:80)
                     Else
                        rptlin=card(10:80)
                     EndIf
                     GoTo 10
                  Else
                     rptlin=card(10:80)
                     GoTo 10
                  EndIf
               Else If(card(10:80).ne.' ') then
                  rptlin = '  ' // card(span(card, 10, ' '):80)
               Else
                  rptlin = ' '
               End if
            End if
         End if
      Else if (type .lt. 1000) then
*        -- P and N card comments.
         Call qpncom(rptlin, card)
      Else
*        -- Cross references.
         If (.not. contin) then
*           -- Print heading if first cross reference.
C           Starts a new page if no room
            If (lincnt .ge. linpag-4.and.linpag.lt.99) Call rptidc
            Write (unit=21, fmt=*)
            Write (unit=21, fmt=2) 'Cross Reference XREF Flags'
            Write (unit=21, fmt=2) '----- --------- ---- -----'
            Write (unit=21, fmt=*)
            lincnt = lincnt + 4
            contin = .true.
         End if
         rptlin = card(9:9) // ' ' // card(10:39)
      End if
12    Continue
      Call RemovePrints(rptlin)
      lrpt = lenstr(rptlin)
*     -- If line is blank, print at least one blank.
      If (lincnt .ge. linpag.and.linpag.lt.99) Call rptidc
      ln=lrpt
      If(ln+4.ge.nclmn) then
         Do 395 l=nclmn-4,1,-1
         If(rptlin(l:l).eq.' ') then
            Write(unit=21, fmt=400) rptlin(1:l)
            lincnt = lincnt + 1
            ll=l
            Call delstr(rptlin,1,ll)
            If(.NOT.outtab)Then
               Call addstr(rptlin,1,'  ')
               ln=lenstr(rptlin)
               Go to 396
            Else
               ln=Lenstr(rptlin)
               If(ln .EQ. 0)GoTo 396
            EndIf
         End if
  395    Continue
  396 Continue
      End if
      If (lincnt .ge. linpag.and.linpag.lt.99) Call rptidc
      If (ln.eq.0 .AND. .NOT.(card(7:7).EQ.'T' .OR. card(7:7).EQ.'t'))
     2  Then
         ln = 1
         Write (unit=21, fmt=400) rptlin(1:ln)
 400     FORMAT(5X, A)
         lincnt = lincnt + 1
      ElseIf(ln .GT. 0)Then
         Write (unit=21, fmt=400) rptlin(1:ln)
         lincnt = lincnt + 1
      EndIf
      If(rptln2 .NE. ' ')Then
         rptlin=rptln2
         rptln2=' '
      Else
         rptlin=' '
      EndIf
      If(outtab)Then
         outtab=.FALSE.
         GoTo 11
      EndIf
      Goto 10
 
*  Process data table.
 
C     Starts a new page if no room
   20 If (lincnt .ge. linpag-3.and.linpag.lt.99) Call rptidc
C     Adds three lines to lincnt
      Call tblhdr(icrd)
      If (type .gt. 2000) Goto 30
      COMLEN = 0
      rptlin = ' '
   21 If (card(6:7) .eq. '  ') then
*        -- Get footnote references, if any.
         If (card(77:77) .eq. 'C') then
*           -- Get rid of non-flag characters.
            If (icrd .eq. carda) card(77:77) = ' '
            If (icrd .eq. cardb) card(77:77) = ' '
            If (icrd .eq. carde) card(77:77) = ' '
         End if
         flgstr = card(77:77)
*        7/9/91 -- Uncertain placement footnote of radiations is missing
*           this the place to add it? "?" added to energy of level elsew
         If(icrd.gt.cardl.and.card(80:80).eq.'?') then
            i=Lenstr(flgstr)
            If(i.gt.0) then
               flgstr(i+1:)='?'
            Else
               flgstr='?'
            Endif
         Endif
 
*        -- Look for FLAG card as next card.
         Read (unit=iunit, fmt='(31X,A80)', iostat=i, end=85) fcard
*        -- If end of file found, treat it as a non-flag data card.
   85    If (i .ne. 0) ieof = .true.
         If (fcard(10:13) .eq. 'FLAG') then
*           -- Found flag card, use the given string.
            i = index(fcard, '=')
            i = span(fcard, i+1, ' ')
            If(Lenstr(flgstr) .GT. 0)Then
               flgstr = flgstr(1:lenstr(flgstr))//fcard(i:lenstr(fcard))
            Else
               flgstr=fcard(i:lenstr(fcard))
            Endif
         Else
*           -- Not a FLAG card, backspace to allow reread.
            Backspace (unit=iunit)
         End if
 
*        XREF cards only exist in Adopted level tables.
         If (icrd.eq.cardl) then
*           -- Look for XREF card as next card.
            Read (unit=iunit, fmt='(31X,A80)', iostat=i, end=86) fcard
*           -- If end of file found, treat it as a non-flag data card.
   86       If (i .ne. 0) ieof = .true.
            If (fcard(10:13) .eq. 'XREF') then
*              -- Found flag card, use the given string.
               i = index(fcard, '=')
               i = span(fcard, i+1, ' ')
               xrfstr= fcard(i:lenstr(FCARD))
cxrf
*              Convert -() notation from exclusive to an inclusive
*                 character list
               If(Index(xrfstr,'-').gt.0) then
                  negstr=xrfstr
                  Call Sqzstr(negstr,'-')
                  Call Sqzstr(negstr,'(')
                  Call Sqzstr(negstr,')')
                  xrfstr=' '
                  Do 210 i=1,nxcrd
                     If(Index(negstr,xflg(i)).le.0)
     +                  xrfstr(i:i)=xflg(i)
210               Continue
                  Call Sqzstr(xrfstr,' ')
               Endif
*              -- Get rid of all quantities within parenthesis
  299          i = index(xrfstr,'(')
               If (i.gt.0) then
                  If(indexf(xrfstr,i+1,')').gt.i)
     *            Call delstr(xrfstr,i,indexf(xrfstr,i+1,')')-i+1)
                  Go to 299
               End if
               Backspace (unit=iunit)
            Else
*              -- Not a XREF card, backspace to allow reread.
               xrfstr=' '
               Backspace (unit=iunit)
            End if
         End if
 
         Do 25 ifield = 1, nfld-1
            ifld=ifield
*           -- If there are footnote references, include them.
            i = lenstr(flgstr)
            If (i .eq. 0) then
               fnote = ' '
            Else
               Call fnref(fnote, ifld, icrd, flgstr(1:i))
            End if
*           -- Some fields need special processing. Check these first.
            If (ifld .eq. 1) then
*              -- Special processing for E field.
               If (icrd .eq. cardb .or. icrd .eq. carde) then
                  If (card(10:10) .eq. '(') then
*                    -- If (energy) form.
                     jposr = fldwid(1, icrd, 3)
                     jposx = 9 + lenstr(card(10:21))
                     rptlin(1:jposr) = card(10:jposx) // fnote
                     Goto 25
                  End if
*                 -- If not (energy) use standard processing.
               End if
            Else if (ifld .eq. 2 .and. icrd .ne. cardl) then
*              -- Use elev for this field.
               iposr = fldpos(2, icrd, 3)
               If (iposr .gt. 0) then
                  jposr = iposr - 1 + fldwid(2, icrd, 3)
                  rptlin(iposr:jposr) = elev
                  Goto 25
               End if
            Else if (ifld .eq. 4 .and. icrd. eq. cardl) then
*              -- Output XREF field
cxrf
               iposr = fldpos(4, icrd, 3)
               If (iposr .gt. 0) then
                  jposr = iposr - 1 + fldwid(4, icrd, 3)
                  rptlin(iposr:jposr) = ' '
                  Do 220 n=1,nxcrd
                     If((Index(xrfstr,xflg(n)).gt.0.and.xflg(n).ne.' ')
     +                 .or.(xrfstr.eq.'+'))
     +                 then
                        If(n.le.fldwid(4,icrd,3)) then
                           ij=iposr-1+n
                           rptlin(ij:ij)=xflg(n)
                        Else
                           rptlin(jposr:jposr)='>'
                        Endif
                     Endif
220               Continue
                  Goto 25
               End if
            Else if (ifld .eq. 5
     +      .and. (icrd .eq. 4 .or. icrd .eq. 5)) then
*              -- Add UN field to LOGFT field if UN not blank.
               If (card(78:79) .ne. ' ') then
                  Call addstr(fnote, 1, '[' // card(78:79) // ']')
               End if
            End if
*           -- Standard processing.
*           -- Get start of output field.
            iposr = fldpos(ifld, icrd, 3)
            If (iposr .gt. 0) then
*              -- This column is present and will be processed.
*              -- Get end of output field.
               jposr = iposr - 1 + fldwid(ifld, icrd, 3)
*              -- Get start of input field.
               iposx = fldpos(ifld, icrd, 1)
*              -- Get end of input field.
               jposx = iposx - 1 + fldwid(ifld, icrd, 1)
*              -- Reduce input field to exclude trailing blanks.
               jposx = iposx - 1 + lenstr(card(iposx:jposx))
*              -- If there is no data for this column on this card,
*                 just write one blank.
               If (jposx .lt. iposx) jposx = iposx
*              -- Get start on input uncertainty field.
               iposd = fldpos(ifld, icrd, 2)
               If (iposd .gt. 0) then
*                 -- There exists an uncertainty.
*                 -- Get end of input uncertainty field.
                  jposd = iposd - 1 + fldwid(ifld, icrd, 2)
*                 -- Pack the uncertainty in with the field.
*                 -- Include footnote between them.
                  Call pakxdx(card(iposx:jposx) // fnote,
     +               card(iposd:jposd), rptlin(iposr:jposr))
               Else
*                 -- There is no uncertainty.
*                 -- Just add the footnote after the field.
                  Call pakxdx(card(iposx:jposx) // fnote,
     +               ' ', rptlin(iposr:jposr))
               End if
*              -- Special processing for unplaced data records.
               If (ifld .eq. 1 .and. elev .eq. ' ') then
*                 -- Add 'x' before energy for unplaced.
                  rptlin(1:1) = 'x'
               End if
*              -- Some more special processing for E field.
               If (ifld .eq. 1 .and. card(80:80) .eq. 'S') then
*                 -- If "S" set add "()" around E DE.
                  iposx = lenstr(rptlin(iposr:jposr)) + iposr
                  rptlin(iposx:iposx) = ')'
                  Call addstr(rptlin(iposr:jposr), 1, '(')
               End if
            End if
   25    Continue
*        -- Check if level same as previous level, set contlv.
         contlv = .false.
         If ((adopt.or.gamord.ge.6).and. icrd .eq. cardg) then
            col1=fldpos(2,cardg,3)
            col2=col1+fldwid(2,cardg,3)-1
            If(adgtmp(col1:col2).eq.rptlin(col1:col2)) contlv = .true.
            adgtmp=rptlin(1:col2)
         End if
         If (lincnt .ge. linpag.and.linpag.lt.99) then
C           Resets lincnt to 2 + No. of lines in title and inc page
            Call rptidc
C           Adds three lines to lincnt
            Call tblhdc
            newpag=.true.
         Else
            newpag=.false.
         End if
         If (ieof) Goto 90
 
      Else
*        -- Comment or continuation card.
   27    iposr = fldpos(8, icrd, 3)
         jposr = iposr - 1 + fldwid(8, icrd, 3)
         COMNXT = .FALSE.
         READ(UNIT=IUNIT,FMT=1,END=180) TYPE,E,SEQ,ELEV,CARD2
         If(type.eq.9999) Go to 180
*        -- Read ahead so next card can be checked
         GOTO 185
 180     BACKSPACE(UNIT=IUNIT)
         GOTO 186
 185     IF (CARD2(7:7) .NE. ' ') THEN
*          -- Check if next card is a continuation comment
           IF((CARD2(6:6) .NE. ' ').AND.(CARD2(6:6).NE.'1'))THEN
              COMNXT = .TRUE.
           END IF
         END IF
         BACKSPACE(UNIT=IUNIT)
 
*        -- Now process original card
 186     If (card(7:7) .ne. ' ') then
*           -- Comment.
            If (card(6:6) .eq. ' ') then
*              -- First comment card.
               IF(CARD(7:7).EQ.'U') THEN
C                 --Update comment
                  CRDTMP='Update C:'//CARD(10:)
                  CALL PAKCOM(CRDTMP, 1, COMBUF(COMLEN+1:LEN(COMBUF)),
     1               CONTIN)
               ELSE IF(CARD(7:7).EQ.'P') THEN
C                 --Publication comment
                  CALL PUBCRD(CARD,CRDTMP)
                  CRDTMP(1:9)='   Pub C:'
                  CALL PAKCOM(CRDTMP, 4, COMBUF(COMLEN+1:LEN(COMBUF)),
     1               CONTIN)
               ELSE
C                 --other comments
                  Call brksym(card, j, k)
                  If (j.lt.10.or.card(10:j) .eq. ' ') then
*                    -- No sym.
                     Call pakcom(card, k, COMBUF(COMLEN+1:LEN(COMBUF)),
     *                  contin)
                  Else
*                    -- Sym.
                     Call pakxdx(card(10:j), ' ', COMBUF(1:LEN(COMBUF)))
                     iposr = lenstr(COMBUF) + 1
                     COMBUF(iposr:iposr) = ':'
                     iposr = iposr + 2
                     Call pakcom(card, k, COMBUF(iposr:LEN(COMBUF)),
     *                  contin)
                     IPOSR = FLDPOS(8, ICRD, 3)
*                    -- set iposr to point to comment symbol
                  End if
               ENDIF
            Else
*              -- Continuation comment card.
               Call pakcom(card, 10, COMBUF(COMLEN+1:LEN(COMBUF)),
     *            contin)
            End if
         Else
*           -- Continuation card.
            If (index(card,'XREF').gt.0) then
               i = index(card,'=')
               card(i:i)=':'
               Call sqzstr(card(10:80),' ')
*              -- Remove flags not followed by ().
               i = index(card,':')+1
               xrfstr = card(i:lenstr(card))
               card(i:80)=' '
  184          i=index(xrfstr,'(')
               If (i.gt.0) then
                  card(lenstr(card)+1:80)=
     *               xrfstr(i-1:indexf(xrfstr,i+1,')'))
                  Call delstr(xrfstr,1,index(xrfstr,')'))
                  Go to 184
               End if
C              Go and read the next card if all the XREF information
C                 goes into the XREF column and no Comment line is
C                 required.
               If (card(index(card,':')+1:80).eq.' ') Go to 300
               Call pakcom(card, 10, COMBUF(COMLEN+1:LEN(COMBUF)),
     *            contin)
            Else
               Call pakcom(card, 10, COMBUF(COMLEN+1:LEN(COMBUF)),
     *            contin)
            End if
         End if
 
*        -- handles level data and comments or just comments.
         comspa = nclmn - 1  - iposr
  190    IF (COMSPA .GE. 25) THEN
*           -- Comment field exists
            FLDSIZ = COMSPA
            COMFLD = .TRUE.
         ELSE
            FLDSIZ = nclmn - 1
            COMFLD = .FALSE.
         END IF
 
         COMLEN = LENSTR(COMBUF)
         DIFER = FLDSIZ - COMLEN
*        -- Check if comment is larger than comment field
         IF ((COMLEN .GT. FLDSIZ) .OR. (DIFER .LT. 5)) THEN
            IF ((COMBUF(FLDSIZ:FLDSIZ).NE.' ') .AND.
     +        (COMBUF(FLDSIZ+1:FLDSIZ+1) .NE. ' ')) THEN
*              -- Word wrap.
               LAST = FLDSIZ
               DO 195 M = LAST, 10, -1
                  IF (COMBUF(M:M) .EQ. ' ') THEN
                     FLDSIZ = M
                     GOTO 200
                  END IF
 195           CONTINUE
C              NO BLANK FOUND TO BREAK ON. LOOK FOR A NONALPHANUMERIC
               DO 196 M=LAST,10,-1
                  IF(TYPSTR(COMBUF(M:M)).LE.0) THEN
                     FLDSIZ=M
                     GO TO 200
                  END IF
  196          CONTINUE
            END IF
 200        CONTINUE
            If (lincnt .ge. linpag.and.linpag.lt.99) then
C              Resets lincnt to 2+# of lines in title and inc page
               Call rptidc
C              Adds three lines to lincnt
               Call tblhdc
               newpag=.true.
            Else
               newpag=.false.
            End if
            IF (COMFLD) THEN
               RPTLIN(IPOSR:nclmn - 1 ) = COMBUF(1:FLDSIZ)
               If (contlv.and.(.not.newpag)) then
*                 -- If level same as previous, blank out field.
                  rptlin(col1:col2)=' '
               End if
               lng = LENSTR(RPTLIN(1:nclmn - 1 ))
               IF (lng .GT. 0) THEN
                  WRITE(UNIT=21,FMT='(A)') RPTLIN(1:lng)
                  LINCNT = LINCNT + 1
                  COMLEN = 0
                  rptlin = ' '
               END IF
            ELSE
*              -- Comment field does not exist, so,
*              -- print level dat if it exists
               IF (RPTLIN(1:IPOSR-1) .NE. ' ') THEN
                  If (lincnt .ge. linpag-1.and.linpag.lt.99) then
C              Resets lincnt to 2+#(lines in title) and increments page
C                 number.
                     Call rptidc
C                    Adds three lines to lincnt
                     Call tblhdc
                     newpag=.true.
                  Else
                     newpag=.false.
                  End if
                  If (contlv.and.(.not.newpag)) then
*                    -- If level same as previous, blank out field.
                     rptlin(col1:col2)=' '
                  End if
                  lng = LENSTR(RPTLIN(1:IPOSR-1))
                  WRITE(UNIT=21,FMT='(A)') RPTLIN(1:lng)
                  LINCNT = LINCNT + 1
                  RPTLIN = ' '
                  COMLEN=0
               END IF
*              -- Print comment on next line
               RPTLIN(1:nclmn - 1 ) = COMBUF(1:FLDSIZ)
               lng = LENSTR(RPTLIN(1:nclmn - 1 ))
               IF (lng .GT. 0 ) THEN
                  WRITE(UNIT=21,FMT='(A)') RPTLIN(1:lng)
                  LINCNT = LINCNT + 1
                  COMLEN = 0
                  rptlin = ' '
               END IF
            END IF
*           -- Shift contents of comment buffer foward
            RPTLIN(1:nclmn - 1 ) = ' '
            COMBUF(1:FLDSIZ) = ' '
            TMPBUF = COMBUF(FLDSIZ+1:LEN(COMBUF))
            COMBUF(2:LEN(COMBUF)) = TMPBUF(1:254)
            GOTO 190
*           -- Go back up and begin printing the rest of the buffer
 
         ELSE
*           -- Comment will fit into comment field
            IF (COMNXT) THEN
               COMLEN = COMLEN + 1
*           read next comment card and go back up to process it
               READ(UNIT=IUNIT,FMT=1,END=180)TYPE,E,SEQ,ELEV,CARD
               If(type.eq.9999) Go to 180
               GOTO 27
 
            ELSE
               If (lincnt .ge. linpag.and.linpag.lt.99) then
C                 Resets lincnt to 2+#(lines in title) and inc page
                  Call rptidc
C                 Adds three lines to lincnt
                  Call tblhdc
                  newpag=.true.
               Else
                  newpag=.false.
               End if
*              -- just print what is already in buffer
               IF (COMFLD) THEN
                  RPTLIN(IPOSR:nclmn - 1 ) = COMBUF(1:FLDSIZ)
                  If (contlv.and.(.not.newpag)) then
*                    -- If level same as previous, blank out field.
                     rptlin(col1:col2)=' '
                  End if
                  lng = LENSTR(RPTLIN(1:nclmn - 1 ))
                  IF (lng .NE. 0) THEN
                     WRITE(UNIT=21,FMT='(A)') RPTLIN(1:lng)
                     LINCNT = LINCNT + 1
                     COMLEN = 0
                     rptlin = ' '
                  END IF
 
               ELSE
*              -- Comment field does not exist, so,
*              -- print level data if it exists
                  IF (RPTLIN(1:IPOSR-1) .NE. ' ') THEN
                     If (lincnt .ge. linpag-1.and.linpag.lt.99) then
C              Resets lincnt to 2+#(lines in title) and increments page
C                 number.
                        Call rptidc
C                       Adds three lines to lincnt
                        Call tblhdc
                        newpag=.true.
                     Else
                        newpag=.false.
                     End if
                     If (contlv.and.(.not.newpag)) then
*                       -- If level same as previous, blank out field.
                        rptlin(col1:col2)=' '
                     End if
                     lng = LENSTR(RPTLIN(1:IPOSR-1))
                     WRITE(UNIT=21,FMT='(A)') RPTLIN(1:lng)
                     LINCNT = LINCNT + 1
                     RPTLIN(1:nclmn - 1 ) = ' '
                     COMLEN = 0
                     rptlin = ' '
                  END IF
*                 -- Print comment on next line
                  RPTLIN(1:nclmn - 1 ) = COMBUF(1:FLDSIZ)
                  lng = LENSTR(RPTLIN(1:nclmn - 1 ))
                  IF (lng .GT. 0) THEN
                     WRITE(UNIT=21,FMT='(A)') RPTLIN(1:lng)
                     LINCNT = LINCNT + 1
                     COMLEN = 0
                     rptlin = ' '
                  END IF
 
                  READ(UNIT=IUNIT,FMT=1,END=250)TYPE,E,SEQ,ELEV,CARD
                  If(type.eq.9999) Go to 250
                  IF (CARD(6:7) .EQ. ' '.AND.LINCNT.LT.linpag.and.
     +              linpag.lt.99) THEN
*                    -- Next card is not a contin. card, so,
*                    -- print as blank line to separate the groups
                     WRITE(UNIT=21,FMT=*)
                     LINCNT = LINCNT + 1
                  END IF
 250              BACKSPACE(UNIT=IUNIT)
               END IF
 
            END IF
            COMBUF = ' '
            RPTLIN = ' '
            COMLEN = 0
         END IF
         IF (CONTIN) GOTO 27
      End if
C     Must come back to here and get next card if XREF all goes to
C        column.
 300  READ(UNIT=IUNIT,FMT=1,END=99) TYPE, E, SEQ, ELEV, CARD
      If(type.eq.9999) Go to 90
 
      If((card(6:7).eq.'  '.and. type. le. 2000).or.type.gt.2000) then
C        The next card is a data card or a footnote card so write out
C           the current report line (rptlin).
*        --  handles level data only
         If (contlv.and.(.not.newpag)) then
*           -- If level same as previous, blank out field.
            rptlin(col1:col2)=' '
         End if
 
         lng = Lenstr(rptlin)
         If(lng.gt.0) then
            Write (unit=21, fmt='(A)') rptlin(1:lng)
            lincnt = lincnt + 1
            COMLEN = 0
            rptlin = ' '
         Endif
      Endif
      If (type .le. 2000) Goto 21
 
*  Process footnote table.
 
   30 If (lincnt .ge. linpag.and.linpag.lt.99) then
         Call rptidc
      Else
         Write (unit=21, fmt=*)
         lincnt = lincnt + 1
      End if
      icode = type - 3000
   35 Call fnxst(xst, icode, icrd)
      If (xst .or. icode .eq. 41) then
*        -- If column associated with footnote exists then
*           print this footnote, otherwise ignore it.
         If (card(6:6) .eq. ' ') then
*           -- First footnote card.
            If (icode .eq. 41) then
*              -- 41 is special 'unplaced' footnote.
               rptlin = '[x] ' // card(20:80)
            Else
               i = index(card, '$') + 1
               If (i .eq. 1) i = 20
               rptlin = '[' // fncode(icode:icode) // '] '
     +            // card(span(card, i, ' '):80)
            End if
         Else
*           -- Continuation footnote card.
            rptlin = '    ' // card(span(card, 10, ' '):80)
         End if
         If (lincnt .ge. linpag.and.linpag.lt.99) Call rptidc
         lng = LENSTR(RPTLIN(1:nclmn - 1 ))
         IF (lng .GT. 0) THEN
            Call RemovePrints(rptlin)
            lng=Lenstr(rptlin)
            If(lng .LE. 0)Then
               lng=1
               rptlin=' '
            EndIf
            Write (unit=21, fmt='(1X,A)') rptlin(1:lng)
            rptlin=' '
            lincnt = lincnt + 1
         END IF
      End if
      Read (unit=iunit, fmt=1, end=99) type, e, seq, elev, card
      If(type.eq.9999) Go to 99
      icode = type - 3000
      Goto 35
 
   90 If (contlv.and.(.not.newpag)) then
*        -- If level same as previous, blank out field.
         rptlin(col1:col2)=' '
      End if
      lng = LENSTR(RPTLIN(1:nclmn - 1 ))
      IF (lng .GT. 0) THEN
         Call RemovePrints(rptlin)
         lng=Lenstr(rptlin)
         If(lng .LE. 0)Then
            lng=1
            rptlin=' '
         EndIf
         WRITE(UNIT=21, FMT='(A)') RPTLIN(1:lng)
         rptlin=' '
         LINCNT = LINCNT + 1
      END IF
   99 Continue
      Return
      End
 
 
      Subroutine RPTID (icrd)
 
************************************************************************
*  Format report header line for the given card type.                  *
************************************************************************
 
      Integer           nclmn
      Common   /column/ nclmn
 
*  Subroutine arguments.
 
      Integer           icrd
 
*  Function references.
 
      Integer           lenstr
 
*  Common blocks.
 
      Character*80      idcard,idcont(20)
      Common   /idcard/ idcard,idcont
      Integer  ncont
      Common   /idcont/ ncont
 
      Integer           lincnt,linpag
      Common   /lincnt/ lincnt,linpag
 
*  Local variables.
      Character*66      blank
      Character*840     dsid
      Character*26      dsref
      Integer           i,N
      Integer           icolon
      Character*892     idline
      Character*108     outlin(15)
      Integer           lid
      Integer           nout,lout,offset,break
      Integer           pblank
      Character*5       nucid
      Integer           page
      Character*132     undsco
      Save
*  Data initializations.
 
      Data              blank /' '/
      Data              page /0/
 
*  Format statements.
 
    2 Format(' ', 2A/)
 
************************************************************************
 
*  Extract nucid, dsid, and dsref from idcard.
 
      nucid = idcard(1:5)
      dsid  = idcard(10:39)
*     add DSID continuation cards if required
      If (ncont.gt.0.and.ncont.le.20) then
         Do 90 i=1,ncont
            dsid(lenstr(dsid)+1:LEN(DSID))=idcont(i)(10:39)
   90    Continue
      End if
      icolon = index(dsid, ':')
C     Attempt to distinguish between pre-Y2K and post-Y2K when obtaining
C       the DSREF (TWB)
      If(idcard(63:63) .EQ. ' ')Then
         dsref = idcard(40:64)
      Else
         dsref = idcard(40:65)
      EndIf
      Call locase(nucid(5:5))
*     -- Make 2nd letter of Z lower case.
*  Select proper heading style based upon card type.
 
      Goto (100, 200, 300, 400, 500, 600) icrd
*            L    A    D    B    E    G
 
*  Comments, References, and L card.
 
  100 If (dsid(1:10) .eq. 'REFERENCES') then
         idline = 'References for A = ' // nucid
         If (icolon .gt. 0) idline(lenstr(idline)+2:LEN(IDLINE)) =
     *      dsid(icolon:LENstr(DSID))
      Else if (dsid(1:8) .eq. 'COMMENTS') then
         idline = 'Comments for A = ' // nucid
         If (icolon .gt. 0) idline(lenstr(idline)+2:LEN(IDLINE)) =
     *      dsid(icolon:LENstr(DSID))
         idline(lenstr(idline)+4:LEN(IDLINE)) = dsref
      Else if (dsid(1:5) .eq. 'ADOPT') then
         idline = nucid // '   Adopted levels'
         If (icolon .gt. 0) idline(lenstr(idline)+2:LEN(IDLINE)) =
     *      dsid(icolon:LENstr(DSID))
         idline(lenstr(idline)+4:LEN(IDLINE)) = dsref
      Else
         idline = nucid // ' levels from ' // dsid(1:lenstr(dsid))
     +      // '   ' // dsref
      Endif
      Goto 900
 
*  A card.
 
  200 idline='alpha-radiations from '//dsid(1:lenstr(dsid))//'   '//
     *   dsref
      Goto 900
 
*  D card.
 
  300 idline = 'particle-radiations from ' // dsid(1:lenstr(dsid)) //
     +   '   ' // dsref
      Goto 900
 
*  B card.
 
  400 idline ='beta-radiations from '//dsid(1:lenstr(dsid))//'   '//
     *   dsref
      Goto 900
 
*  E card.
 
  500 idline = 'beta+,ec data from '// dsid(1:lenstr(dsid))//'   '//
     *   dsref
      Goto 900
 
*  G card.
 
  600 If (index(dsid, 'ADOPT') .gt. 0) then
         idline = nucid // '   Adopted gamma-radiations'
         If (icolon .gt. 0) idline(lenstr(idline)+2:LEN(IDLINE)) =
     *      dsid(icolon:LENstr(DSID))
         idline(lenstr(idline)+4:LEN(IDLINE)) = dsref
      Else
         idline = 'gamma(' // nucid // ') from ' // dsid(1:lenstr(dsid))
     +      // '   ' // dsref
      Endif
      Goto 900
 
*  Center and underline.
 
  900 lid = lenstr(idline)
*     -- Add title footnotes, if any.
      Call fnref(idline(lid+2:LEN(IDLINE)), 8, icrd, ' ')
      lid = lenstr(idline)
      pblank = (nclmn - lid) /2
      If (pblank.ge.12) then
         nout=1
         outlin(1)=idline(1:lid)
      Else
*     -- ID Title must be given on more than one line.
         pblank=12
         nout=0
         offset=0
  880    Continue
         Do 890 i=offset+(nclmn-24),offset+2,-1
            If (idline(i:i).eq.' '.and.idline(i-1:i-1).ne.' ') then
                break=i-1
               outlin(nout+1)=idline(offset+1:break)
               nout=nout+1
               offset=i+1
               If (offset.gt.lid) Go to 895
               Go to 880
            Else if(idline(i:i+1).eq.'),') then
               break=i+1
               outlin(nout+1)=idline(offset+1:break)
               nout=nout+1
               offset=i+2
               If (offset.gt.lid) Go to 895
               Go to 880
            Endif
  890    Continue
         break=offset+(nclmn-24)
         outlin(nout+1)=idline(offset+1:break)
         nout=nout+1
         offset=break+1
         If (offset.gt.lid) Go to 895
         Go to 880
  895    Continue
      Endif
      page = page + 1
      If(page.gt.1.and.lincnt.lt.linpag.and.linpag.lt.99) then
         Do 901 i = lincnt+1, linpag
            WRITE(UNIT=21,FMT=*)
  901    Continue
      End if
      lout=lenstr(outlin(1))
      lout=((lout+1)/2)*2
      pblank=(nclmn-lout)/2
      If(nclmn.eq.80) then
         Write (unit=21,fmt=401) nucid,blank(1:pblank-6),
     *      outlin(1)(1:lout), page
 401     Format(' ', 3A, T71, 'Page', I4)
      Else
         Write (unit=21,fmt=402) nucid,blank(1:pblank-6),
     *      outlin(1)(1:lout), page
 402     Format(' ', 3A, T123, 'Page', I4)
      End if
      lincnt=1
      lid=lout
      If (nout.ge.2) then
         Do 905 n=2,nout
            lout=lenstr(outlin(n))
            lout=((lout+1)/2)*2
            pblank=(nclmn-lout)/2
            Write(unit=21,fmt='(1X,2A)') blank(1:pblank-1),
     *         outlin(n)(1:lout)
            lincnt=lincnt+1
            lid=max0(lid,lout)
  905    Continue
      End if
      pblank=(nclmn-lid)/2
      Do 910 i = 1, lid+6
         undsco(i:i) = '='
  910 Continue
      Write (unit=21, fmt=2) blank(1:pblank-4), undsco(1:lid+6)
      lincnt=lincnt+2
*     -- In case of continuation onto another page later, add (cont) and
*        ===.
*     -- Add it to the last line of the title.
      outlin(nout)(lenstr(outlin(nout))+2:len(outlin(nout)))='(cont)'
      lout=lenstr(outlin(nout))
      If (lout.gt.lid) then
         lid=lout
         Do 930 i = 1, lid+6
            undsco(i:i) = '='
  930    Continue
      End if
      Return
 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 
      Entry RPTIDC
 
*        Report id continued.
      page = page + 1
      If(lincnt.lt.linpag.and.linpag.lt.99) then
         Do 902 i = lincnt+1, linpag
            WRITE(UNIT=21,FMT=*)
  902    Continue
      End If
      lout=lenstr(outlin(1))
      lout=((lout+1)/2)*2
      pblank=(nclmn-lout)/2
      If(nclmn.eq.80) then
         Write (unit=21,fmt=401) nucid,blank(1:pblank-6),
     *      outlin(1)(1:lout), page
      Else
         Write (unit=21,fmt=402) nucid,blank(1:pblank-6),
     *      outlin(1)(1:lout), page
      End if
      lincnt=1
      If (nout.ge.2) then
         Do 903 n=2,nout
            lout=lenstr(outlin(n))
            lout=((lout+1)/2)*2
            pblank=(nclmn-lout)/2
            Write(unit=21,fmt='(1X,2A)') blank(1:pblank-1),
     *         outlin(n)(1:lout)
            lincnt=lincnt+1
  903    Continue
      End if
      pblank=(nclmn-lid)/2
      Write (unit=21, fmt=2) blank(1:pblank-4), undsco(1:lid+6)
      lincnt=lincnt+2
      Return
      End
 
      Subroutine TBLHDR (icrd)
 
************************************************************************
*  Set up the fldpos table for a report of type icrd.                  *
*                                                                      *
*  Based on the amount of space left on the line, set up fldpos and    *
*  fldwid for the line comments column in the table.                   *
*                                                                      *
*  Print the header for this table.                                    *
************************************************************************
 
      Integer           nclmn
      Common   /column/ nclmn
 
*  Subroutine arguments.
 
      Integer           icrd
 
*  Function references.
 
      Integer           lenstr
 
*  Parameters.
 
      Integer           nfld, ncrd
      Parameter        (nfld = 8, ncrd = 6)
      Integer           cardg
      Parameter        (cardg = 6)
 
*  Common blocks.
 
      Logical           adopt
*                          True if data set is adopted.
*                          Used to not sort adopted gammas.
      Integer           gamord
*                          Gamord flag from PN card.
      Integer           col1, col2
*                          Ending column numbers for Eg and E(level).
*                          Used to reorder Eg and E(level) columns.
      Common   /adgam/  adopt, gamord, col1, col2
 
      Character*11      fldnam(nfld, ncrd)
      Common   /fldnam/ fldnam
 
      Integer           fldpos(nfld, ncrd, 3)
      Common   /fldpos/ fldpos
 
      Integer           fldwid(nfld, ncrd, 3)
      Common   /fldwid/ fldwid
 
      Logical           fldxst(nfld, ncrd)
      Common   /fldxst/ fldxst
 
      Integer           lincnt,linpag
      Common   /lincnt/ lincnt,linpag
 
*  Local variables.
      Character*132     hdrlin
      Save              hdrlin
      Integer           i,IFIELD,LNG
      Integer           ipos
      Integer           ifld
      Integer           iwid
      Integer           jpos
      Integer           lfld
      Integer           ltxt
      Character*42      refstr
      Character*132     undsco
      Save              undsco
      integer           CWID
      integer           LINPOS
      Save              linpos
      Logical           maxxrf
      Integer           colspc
 
************************************************************************
 
*  Based upon fldxst, build fldpos and fldwid (for comments).
 
      colspc=3
      maxxrf=.true.
    5 Continue
      ipos = 2
*     -- Start at column 2; x, if needed, goes in column 1.
      Do 10 ifield = 1, nfld-1
         ifld=ifield
         If (fldxst(ifld, icrd)) then
            Call fnlen(iwid, ifld, icrd, ' ')
*           -- Find length of any footnote references.
            If (iwid .gt. 0) then
*              -- If there are references...
               iwid = iwid + lenstr(fldnam(ifld, icrd))
*              -- Find length of column title with ref list.
               fldwid(ifld, icrd, 3) = max(fldwid(ifld, icrd, 3), iwid)
*              -- Field width is max of data width and title width.
            End if
            fldpos(ifld, icrd, 3) = ipos
            ipos = ipos + fldwid(ifld, icrd, 3) + colspc
         Endif
   10 Continue
 
*     When there is an XREF column and the header will not fit in nclmn
*        then reduce the size of the XREF column
      IF (icrd.eq.1.and.fldxst(4,1).and.maxxrf) then
         maxxrf=.false.
         If(IPOS .GE. nclmn+7) THEN
*           There is not enough room for the data column headdings. Redu
*              column as much as possible or needed and redo. Comments w
*              to column 1.
            fldwid(4,1,3)=Max(4,nclmn+7-ipos+fldwid(4,1,3))
            colspc=1
            Go to 5
         Else If(ipos.ge.nclmn-26) then
*           There is not enough room for the comment field.
            iwid=nclmn-26-ipos+fldwid(4,1,3)
            If(iwid.ge.4) then
               colspc=2
               Go to 5
            Else If(iwid.ge.0) then
               colspc=1
               Go to 5
            Endif
         Endif
      END IF
 
*     -- Now see if header will fit in nclmn columns. If not,
*     -- squeeze it in by removing two spaces at a time.
      IF (IPOS .GE. nclmn - 1 ) THEN
         colspc=colspc-1
         If(colspc.gt.0) then
            Go to 5
         Else
            Write (*,*) 'There are not enough columns available to '//
     +         'display the data columns.'
         Endif
      END IF
      fldpos(8, icrd, 3) = ipos
      CWID = nclmn - 1  - IPOS
      iwid = 133 - ipos
      If (iwid .GT. 71) iwid = 71
      fldwid(8, icrd, 3) = iwid
 
*     Reverse positions of EG and Elevel fields for adopted gammas
 
      If ((adopt.or.gamord.ge.6).and. icrd .eq. cardg) then
         ipos=fldpos(1,cardg,3)
         fldpos(1,cardg,3)=fldpos(2,cardg,3)+fldwid(2,cardg,3)
     +      -fldwid(1,cardg,3)
         fldpos(2,cardg,3)=ipos
      End if
 
*  Build header line.
 
      hdrlin = ' '
      Do 35 ifield = 1, nfld
         ifld=ifield
*        -- Label the fields.
         If (fldxst(ifld, icrd)) then
            iwid = 0
            If (ifld .eq. nfld) then
               lfld = cwid
               ltxt = lenstr(fldnam(ifld,icrd))
            Else
               lfld = fldwid(ifld, icrd, 3)
               Call fnlen(iwid, ifld, icrd, ' ')
               ltxt = lenstr(fldnam(ifld, icrd)) + iwid
            End If
            jpos = fldpos(ifld, icrd, 3) + (lfld - ltxt + 1) / 2
            If((ifld .eq. nfld) .and. (cwid .lt. 25)) goto 35
*           -- If comment field too small, don't create comment header
            If (iwid .eq. 0) then
               hdrlin(jpos:jpos-1+ltxt) = fldnam(ifld, icrd)
            Else
               Call fnref(refstr(1:iwid), ifld, icrd, ' ')
               hdrlin(jpos:jpos-1+ltxt) =
     +            fldnam(ifld,icrd)(1:lenstr(fldnam(ifld,icrd))) //
     +            refstr(1:iwid)
            End if
         End if
   35 Continue
      LNG = LENSTR(HDRLIN(1:nclmn - 1 ))
      Write (unit=21, fmt='(/A)') hdrlin(1:LNG)
      lincnt=lincnt+2
 
*  Underscore titles.
 
      undsco = ' '
      Do 50 ifld = 1, nfld
         If (fldxst(ifld, icrd)) then
            ipos = fldpos(ifld, icrd, 3)
            iwid = fldwid(ifld, icrd, 3)
            If (ifld .eq. nfld) then
               jpos = ipos - 1 + cwid
            Else
               jpos = ipos - 1 + iwid
            End If
            Do 40 i = ipos, jpos
               undsco(i:i) = '-'
   40       Continue
         Endif
   50 Continue
      If (fldxst(8,icrd)) then
*        -- comment field exists
         If (cwid .lt. 25) then
            Write(unit=21, fmt='(A)') undsco(1:ipos-3)
            lincnt=lincnt+1
            LINPOS = IPOS - 3
         Else
            Write (unit=21, fmt='(A)') undsco(1:nclmn - 1 )
            lincnt=lincnt+1
            LINPOS = nclmn - 1
         End If
      Else
*        -- no comment follows on this line
         Write (unit=21, fmt='(A)') undsco(1:jpos)
         lincnt=lincnt+1
         LINPOS = JPOS
      End If
      Return
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 
      Entry TBLHDC
 
      Write (unit=21, fmt='(/A)') hdrlin(1:nclmn - 1 )
      Write (unit=21, fmt='(A)') undsco(1:LINPOS)
      lincnt = lincnt + 3
      Return
      End
      Integer function KRDTYP (card)
 
*  L, A, D, B, E, G, ID,  N,  P,  Q,  X, other.                        *
*  1, 2, 3, 4, 5, 6, 10, 11, 12, 13, 14,   0.                          *
*  Negative values for comments and continuations.                     *
*  General comments and references are type -10.                       *
************************************************************************
 
*  Function arguments.
 
      Character*80      card
 
*  Parameters.
 
      Integer           cardl, carda, cardd, cardb, carde, cardg
      Parameter        (cardl =  1, carda =  2, cardd =  3)
      Parameter        (cardb =  4, carde =  5, cardg =  6)
      Integer           cardi, cardn, cardp, cardq, cardx, cardr
      Parameter        (cardi = 10, cardn = 11, cardp = 12)
      Parameter        (cardq = 13, cardx = 14, cardr = 15)
 
*  Local variables.
 
      Character*(cardr) code
 
*  Data initializations.
 
      Data              code /'LADBEG... NPQXR'/
 
************************************************************************
 
      krdtyp = index(code, card(8:8))
      If (krdtyp .eq. 0) Return
      If (krdtyp .eq. cardr) krdtyp = -cardi
      If (index('CDTcdtPU', card(7:7)) .gt. 0) krdtyp = -abs(krdtyp)
      If (card(6:6) .eq. '1') card(6:6) = ' '
      If (card(6:6) .ne. ' ') krdtyp = -abs(krdtyp)
      End
 
      Subroutine LOCASE (string)
 
************************************************************************
*  For each character in string which is an upper case alphabetic,     *
*  convert it to the corresponding lower case alphabetic.              *
************************************************************************
 
*  Subroutine arguments.
 
      Character*(*)     string
 
*  Local variables.
 
      Integer           i, j
      Character*26      lower,  upper
 
*  Data initializations.
 
      Data              lower /'abcdefghijklmnopqrstuvwxyz'/
      Data              upper /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
 
************************************************************************
 
*  Procedure.
 
      Do 10 i = 1, len(string)
         j = index(upper, string(i:i))
         If (j .gt. 0) string(i:i) = lower(j:j)
   10 Continue
      End
 
      Subroutine NUMOUT (x, dx, str)
 
************************************************************************
*  Convert x and dx into string format, str.                           *
*                                                                      *
*  dx will be in the range [3, 25] and will be rounded from 0.1 up.    *
*                                                                      *
*  If dx = 10 or 20, x contains a fractional part, and the last digit  *
*     of x is 0, then make dx = 1 or 2 as appropriate and drop the last*
*     digit of x.                                                      *
*                                                                      *
*  If x has no fractional part and dx > 25, then multiply dx by        *
*     whatever needed to make legal output without using E format      *
*                                                                      *
*  If dx = 0; x only, to 3 decimal digits.                             *
*                                                                      *
*  Fields will be left justified with a single blank between x and dx. *
************************************************************************
 
*  Subroutine arguments.
 
      Real              x, dx
      Character*(*)     str
 
*  Functions
      Integer index
 
*  Local variables.
 
      Character*7       fmt
      Integer           ipwr, jpwr
      Integer           lstr,indnz
      Real              xx, dxx
 
************************************************************************
 
*  Fit dx into range.
 
      dxx = dx
      ipwr = 0
      If (dxx .eq. 0) then
*        -- Use 3 digits of significance.
         ipwr = -3
      Else
*        -- Compute ipwr.
  10     If (dxx .lt. 3.0) then
            ipwr = ipwr - 1
            dxx = dxx * 10.0
            Goto 10
         End if
   20    If (dxx .gt. 25.0) then
            ipwr = ipwr + 1
            dxx = dxx / 10.0
            Goto 20
         End if
      End if
*  Format x based on ipwr.
 
      If (ipwr .le. 0) then
*        -- -ipwr is number of digits in fractional part.
         jpwr = int(alog10(x)) + 1
         If (jpwr .lt. 1) jpwr = 1
*        -- jpwr is number of digits in integer part.
         lstr = jpwr - ipwr + 1
*        -- Actual length of numeric string.
         Write (unit=fmt, fmt='(2H(F,I2.2,1H.,I1,1H))') lstr, -ipwr
         Write (unit=str, fmt=fmt) x
C        Some fortrans do not require that at least 1 numeric digit prec
C        a decimal point when outputting in the F format (as VAX does).
         indnz=index(str,' .')
         If(indnz.gt.0) str(indnz:indnz)='0'
         If(abs(dxx-10.).lt.0.5.or.abs(dxx-20.).lt.0.5) then
*           -- We may be able to reduce uncert to 1 or 2.
            If (str(lstr:lstr) .eq. '0') then
               str(lstr:lstr) = ' '
               lstr = lstr - 1
               dxx = dxx / 10.0
            End if
         End if
      Else
*        -- ipwr is number of zeros that must be added to xx and dxx.
         jpwr = 10 ** ipwr
         xx = anint(x / jpwr) * jpwr
         dxx = dxx * jpwr
         lstr = int(alog10(xx*1.0000001)) + 2
*        -- Actual length of numeric string.
         Write (unit=fmt, fmt='(2H(F,I2.2,3H.0))') lstr
         Write (unit=str, fmt=fmt) xx
      End if
 
*  Add in dx part (if non-zero)
 
      If (dxx .ne. 0.0) then
C         jpwr = int(alog10(dxx*1.0000001)) + 1
C        The I field width must be based on the rounded dxx
         jpwr = int(alog10((dxx+0.55)*1.0000001)) + 1
         Write (unit=fmt, fmt='(2H(I,I1,1H))') jpwr
C        On output, round dxx up to next higher integer.
         Write (unit=str(lstr+2:LEN(STR)), fmt=fmt) int(dxx+0.55)
      End if
      End
 
      Subroutine PAKXDX (xstr, dxstr, pakstr)
 
************************************************************************
*  Pack xstr and dxstr into pakstr.                                    *
*                                                                      *
*  Remove leading and trailing blanks.                                 *
*                                                                      *
*  Allow a single blank between xstr and dxstr.                        *
************************************************************************
 
*  Subroutine arguments.
 
      Character*(*)     xstr
      Character*(*)     dxstr
      Character*(*)     pakstr
 
*  Function references.
 
      Integer           len,lenstr,span
      External          lenstr,span
 
*  Local variables.
 
      Integer           ifrom
      Integer           ilen
      integer           lnpaks
 
************************************************************************
 
      lnpaks=len(pakstr)
      If (xstr .ne. ' ') then
*        -- Move in xstr.
         ifrom = span(xstr, 1, ' ')
         pakstr = xstr(ifrom:lenstr(xstr))
         ilen = lenstr(xstr) - ifrom + 3
*        -- Move in dxstr, if not blank.
         If (dxstr .ne. ' ') then
            ifrom = span(dxstr, 1, ' ')
            pakstr(ilen:lnpaks) = dxstr(ifrom:lenstr(dxstr))
         End if
      Else
*        -- Return blank string.
      End if
      End
 
      Subroutine PAKCOM (card, start, dest, contin)
 
************************************************************************
*  Pack the comment field from the card, starting at start, into dest. *
*                                                                      *
*  Remove leading and trailing blanks.                                 *
*                                                                      *
*  If the source field is larger than the destination field then:      *
*  1. Set the continued flag to true.                                  *
*  2. Set the continuation field on the card to 'X'.                   *
*  3. Look backward for the first blank at which to break the text.    *
*  4. Copy the text to the left of the break to the destination field. *
*  5. Copy the text to the right of the break back to card(10:80).     *
************************************************************************
 
*  Subroutine arguments.
 
      Character*80      card
      Integer           start
      Character*(*)     dest
      Logical           contin
 
*  Function references.
 
      Integer           lenstr
      Integer           span
 
*  Local variables.
 
      Integer           slen, dlen
      Integer           k, l
      Character*80      src
 
************************************************************************
 
*  Isolate source field.
 
      src = card(start:80)
      l = span(src, 1, ' ')
      If (l .gt. 80) then
*     -- All blank field.
         contin = .false.
         Return
      End if
      If(l.gt.1) Call delstr(src, 1, l-1)
      Call RemovePrints(src)
      slen = lenstr(src)
 
*  Check if source field will fit into destination field.
 
      dlen = len(dest)
      If (slen .le. dlen) then
         contin = .false.
         dest = src(1:slen)
         Return
      End if
 
*  Source won't fit. Flag the condition.
 
      contin = .true.
      card(6:7) = 'XC'
 
*  Look for break at blank (if there is one).
 
      Do 10 k = dlen, 1, -1
         If (src(k:k) .eq. ' ') Goto 20
   10 Continue
 
*  None found, break at end of destination field.
 
      dest = src(1:dlen)
      card(10:80) = src(dlen+1:slen)
      Return
 
*  Break found, break there.
 
   20 dest = src(1:k)
      card(10:80) = src(k+1:slen)
      End
 
      Subroutine QPNCOM (rptlin, card)
 
************************************************************************
*  Process comment cards for Q, P, and N type data cards.              *
************************************************************************
 
**  Subroutine arguments.
 
      Character*132     rptlin
      Character*80      card
 
*  Function references.
 
      Integer           lenstr
      Integer           span
 
*  Local variables.
 
      Integer           j, k
      Integer           lrpt
 
************************************************************************
 
      If (card(6:6) .eq. ' ') then
*        -- First comment card.
         If (card(8:8) .eq. 'Q') then
            rptlin = 'Q-value'
         Else if (card(8:8) .eq. 'P') then
            rptlin = card(1:5)
         Else
            rptlin = ' '
         End if
         Call brksym(card, j, k)
         If(j.eq.9 .AND. card(8:8).NE.'Q') rptlin = ' '
         lrpt = lenstr(rptlin)
         If (j.ge.10.and.card(10:j) .ne. ' ') then
            rptlin(lrpt+2:LEN(RPTLIN)) = card(10:j)
            lrpt = lenstr(rptlin)
         End if
         If(lrpt.gt.0) then
            rptlin(lrpt+1:LEN(RPTLIN))=': '//card(k:len(card))
         Else
            rptlin=card(k:len(card))
         Endif
      Else
*        -- Continuation comment.
         rptlin = '  ' // card(span(card, 10, ' '):80)
      End if
      Return
      End
 
      Subroutine BRKSYM (card, j, k)
 
************************************************************************
*  Search card looking for the end of the sym(flag) field.             *
*  It will either be at the '$' or column 19.                          *
*                                                                      *
*  J will be set to the last character position of the sym(flag) field.*
*  K will be set to the first character of the following text field.   *
************************************************************************
 
*  Subroutine arguments.
 
      Character*80      card
      Integer           j, k
 
*  Local variables.
 
      Integer           i
 
************************************************************************
 
      i = index(card, '$')
      If (i .eq. 0) then
         j = 19
         k = 20
      Else
         j = i - 1
         k = i + 1
      End if
      Return
      End
 
      SUBROUTINE PUBCRD(CARD,CRDTMP)
C
C   get rid of PUB= and $ from CARD and copy to CRDTMP for RPT
 
      CHARACTER*(*) CARD,CRDTMP
 
C     Functions
 
      Integer        Lenstr
 
C     Local
 
      INTEGER       I
C
      CRDTMP=CARD
C
  100 I=INDEX(CRDTMP,'PUB=')
      IF(I.GT.0) THEN
         CALL DELSTR(CRDTMP,I,4)
         GO TO 100
      ENDIF
      CALL REPCHR(CRDTMP,'$',',')
      I=LENSTR(CRDTMP)
      IF(CRDTMP(I:I).EQ.',') CRDTMP(I:I)=' '
      RETURN
      END

      Subroutine RemovePrints(src)
 
      Character*(*) src
 
      Integer Index,Indexf,Lenstr,Ichar
      External Indexf,Lenstr
 
      Character*1 CHAR
      Intrinsic CHAR
 
      Integer ilcb, ibal, i, ibar, ilen, ich
      Character*1 next
      Character*10 subst(91)
      Data subst/'_','(C)','_','#','e','Sqrt','=','deg','<-','->','x',
     1'+-','1/2','-+','oc','/','(',')','[',']','<','>','SQRT','INTG',
     2'PROD','SUMOF','dag','dbldag','<=','<>','>=',' aprox ','inf',
     3'Alpha','Beta','Eta','Delta','Epsilon','Phi','Gamma','Chi','Iota',
     4'~','Kappa','Lambda','Mu','Nu','Omicron','Pi','Theta','Rho',
     5'Sigma','Tau','Upsilon','gradient','Omega','Xi','Psi','Zeta',
     6'{','|','}','(up)','(down)','`',
     7'alpha','beta','eta','delta','ec','phi','gamma','chi','iota','ec',
     8'kappa','lambda','mu','nu','omicron','pi','theta','rho','sigma',
     9'tau','upsilon','?','omega','xi','psi','zeta'/
 
      ibar=0
  100 Continue
      ilcb=Index(src,'{')
      ilen=Lenstr(src)
      If(ilcb.gt.0) then
         ibal=1
         i=ilcb+1
         Do While(ibal.ne.0.and.i.le.ilen)
            If(src(i:i).eq.'{') ibal=ibal+1
            If(src(i:i).eq.'}') ibal=ibal-1
            i=i+1
         Enddo
         If(i.gt.ilen+1) then
            Write(*,*) 'Curly brackets not balanced for ',src
            Go to 200
         Endif
         If(src(ilcb+1:ilcb+1).eq.'T') then
            Call Delstr(src,ilcb,i-ilcb)
         Else
            Call Delstr(src,i-1,1)
            Call Delstr(src,ilcb,2)
         Endif
         Go to 100
      Endif
  200 Continue
      ibar=Indexf(src,ibar+1,'|')
      If(ibar.le.0) Go to 300
      next=src(ibar+1:ibar+1)
C     Handle special cases
      If(next.eq.' ') then
         Call Delstr(src,ibar,1)
         Go to 200
      Else If(next.eq.'"') then
         Call Delstr(src,ibar,2)
         Go to 200
      Endif
      ich=Ichar(next)
      If(ich.ge.32.and.ich.le.122) then
C        If there is room fit translation into the blank space.
C          Should keep better alignment for table comments (TWB. 19990622)      
         If(ibar+Lenstr(subst(ich-31)) .LE. Lenstr(src))Then
            If(src(ibar:ibar+Lenstr(subst(ich-31))) .EQ. ' ')Then
               Call Delstr(src,ibar,1+Lenstr(subst(ich-31)))
               Call
     2           Addstr(src,ibar,subst(ich-31)(1:Lenstr(subst(ich-31))))
               GoTo 200
            EndIf
         EndIf
         Call Delstr(src,ibar,2)
         Call Addstr(src,ibar,subst(ich-31)(1:Lenstr(subst(ich-31))))
         Go to 200
      Endif
      Call Delstr(src,ibar,1)
      Go to 200
  300 Continue
C     Get rid of blackslashes since they represent a backspace
C       (TWB. 19990622)
C+++MDC
C...VAX, DVF, ANS
C/      Call Repstr(src,'\',CHAR(0))
C...UNX
      Call Repstr(src,'\\',CHAR(0))
C---MDC
C     Get rid of up arrows since they indicate no translation
C       (TWB. 19990622)
      Call Repstr(src,'^',CHAR(0))
C     Delete leading "$" (TWB. 19990622)
      If(src(1:1) .EQ. '$')src=src(2:Lenstr(src))
      Return
      End
 
 
      Subroutine Formnum(nstr,outstr)
 
      Character*(*) nstr,outstr
 
      Integer Lenstr,Index
 
      Integer locp,loce,lng,ipwr,iexp,i,j,k
 
      outstr=nstr
      loce=Index(nstr,'E')
      If(loce.eq.0) Return
      locp=Index(nstr,'.')
      lng=Lenstr(nstr)
      If(locp.le.0) locp=lng+1
C     This is an exponential number
      Read(nstr(loce+1:lng),'(I)') iexp
      If(iexp.gt.0) then
         outstr=' '
         Do i=1,locp-1
            outstr(i:i)=nstr(i:i)
         Enddo
         iend=locp-1
         Do i=1,iexp
            j=iend+i
            k=locp+i
            outstr(j:j)=nstr(k:k)
            If(k.ge.loce) outstr(j:j)='0'
         Enddo
         iend=j+1
         outstr(iend:iend)='.'
         If(k+1.le.loce-1) outstr(iend+1:)=nstr(k+1:loce-1)
         lng=Lenstr(outstr)
         Do While(outstr(lng:lng).eq.'0')
            outstr(lng:lng)=' '
            lng=lng-1
         Enddo
      Else If(iexp.lt.0) then
         outstr='0.'
         Do i=1,-1-iexp
            j=2+i
            outstr(j:j)='0'
         Enddo
         iend=2-iexp
         k=locp-1
         outstr(iend:)=nstr(k:k)
         If(locp+1.le.loce-1) outstr(iend+1:)=nstr(locp+1:loce-1)
         lng=Lenstr(outstr)
         Do While(outstr(lng:lng).eq.'0')
            outstr(lng:lng)=' '
            lng=lng-1
         Enddo
      Endif
      Return
      End
