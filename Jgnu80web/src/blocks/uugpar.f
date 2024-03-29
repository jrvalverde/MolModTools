#if(0)
  FTANGLE v1.61,
 created with UNIX on "Friday, September 25, 1998 at 8:02." 
  COMMAND LINE: "ftangle -ybs15000 uugpar"
  RUN TIME:     "Friday, June 5, 2009 at 15:05."
  WEB FILE:     "uugpar.web"
  CHANGE FILE:  (none) 
#endif

      blockdata uugpar
C for gparse
      implicit none
      integer Dummy , Gcl1 , Gcl2 , Gcl3 , Gcl4 , Gcl5 , Gcl6 , Gcl7 ,
     &        Gcl8 , Gcl9 , Gesop1 , Gesop2 , Lev , Opt , Opt2 ,
     &        Optcyc , Prc1 , Prc2 , Psd , Psd2
      integer Save1 , Save2 , Save3 , Scf , Stars , Stb , Stb2 , Sym ,
     &        Sym2 , Unit , Unit1
      dimension Gcl1(40)
      dimension Gcl2(58)
      dimension Gcl3(43)
      dimension Gcl4(41)
      dimension Gcl5(30)
      dimension Gcl6(55)
      dimension Gcl7(91)
      dimension Gcl8(108)
      dimension Gcl9(29)
      dimension Unit(43) , Unit1(32)
      dimension Scf(13) , Gesop1(46) , Gesop2(39)
      dimension Lev(13)
      dimension Opt(61) , Opt2(50)
      dimension Optcyc(11) , Prc1(68)
      dimension Psd(33) , Prc2(57)
      dimension Psd2(22) , Stars(16)
      dimension Save1(39) , Save2(28) , Save3(18)
      dimension Stb(90) , Stb2(79) , Sym(41) , Sym2(35)
      common /bd0001/ Dummy
      common /prstlb/ Gcl1 , Gcl2 , Gcl3 , Gcl4 , Gcl5 , Gcl6 , Gcl7 ,
     &                Gcl8 , Gcl9 , Stars , Optcyc , Scf , Lev , Unit ,
     &                Unit1 , Gesop1 , Gesop2 , Opt , Opt2 , Prc1 ,
     &                Prc2 , Psd , Psd2 , Save1 , Save2 , Save3 , Stb ,
     &                Stb2 , Sym , Sym2
C ----------------------------------------------------------------------
C      dimension gcl1(40)
      data Gcl1/'GCL' , -1 , '-' , 'GCL' , 0 , 0 , -1 , '/' , 'GCL' ,
     &     0 , 0 , -1 , ',' , 'GCL' , 0 , 0 , -1 , '#' , 'GCL' , 0 , 0 ,
     &     'EOL' , 'EXI' , 0 , 0 , 1 , 'N' , 'GCL' , 0 , 0 , 1 , 'P' ,
     &     'GCL' , 0 , 0 , 1 , 'F' , 'GCL' , 0 , 0/
C      dimension gcl2(58)
      data Gcl2/2 , 'HF' , 'GCL' , 6 , 00030000 , 3 , 'RHF' , 'GCL' ,
     &     6 , 00010000 , 3 , 'UHF' , 'GCL' , 6 , 00020000 , 4 ,
     &     'ROHF' , 'GCL' , 6 , 00040000 , 7 , 'COMP' , 'LEX' , 'GCL' ,
     &     33 , 1 , 3 , 'MP2' , 'PRC' , 6 , 02030000 , 4 , 'RMP2' ,
     &     'PRC' , 6 , 02010000 , 4 , 'UMP2' , 'PRC' , 6 , 02020000 ,
     &     6 , 'NONS' , 'TD' , 'EXI' , 25 , 1 , 7 , 'REST' , 'ART' ,
     &     'EXI' , 25 , 1 , 4 , 'CNOE' , 'EXI' , 46 , 1/
C      dimension gcl3(43)
      data Gcl3/3 , 'MP3' , 'PRC' , 6 , 03030000 , 4 , 'RMP3' , 'PRC' ,
     &     6 , 03010000 , 4 , 'UMP3' , 'PRC' , 6 , 03020000 , 4 ,
     &     'RMP4' , 'PRC' , 6 , 04010000 , 4 , 'UMP4' , 'PRC' , 6 ,
     &     04020000 , 6 , 'MP4S' , 'DQ' , 'PRC' , 6 , 04030400 , 7 ,
     &     'RMP4' , 'SDQ' , 'PRC' , 6 , 04010400 , 7 , 'UMP4' , 'SDQ' ,
     &     'PRC' , 6 , 04020400/
C      dimension gcl4(41)
      data Gcl4/5 , 'MP4D' , 'Q' , 'PRC' , 6 , 04030500 , 3 , 'MP4' ,
     &     'PRC' , 6 , 04030000 , 6 , 'RMP4' , 'DQ' , 'PRC' , 6 ,
     &     04010500 , 6 , 'UMP4' , 'DQ' , 'PRC' , 6 , 04020500 , 7 ,
     &     'MP4S' , 'DTQ' , 'PRC' , 6 , 04030600 , 8 , 'RMP4' , 'SDTQ' ,
     &     'PRC' , 6 , 04010600 , 8 , 'UMP4' , 'SDTQ' , 'PRC' , 6 ,
     &     04020600/
C      dimension gcl5(30)
      data Gcl5/2 , 'CI' , 'PRC' , 6 , 05000000 , 3 , 'CID' , 'PRC' ,
     &     6 , 05000200 , 4 , 'CISD' , 'PRC' , 6 , 05000300 , 4 ,
     &     'CIDS' , 'PRC' , 6 , 05000300 , 2 , 'CC' , 'PRC' , 6 ,
     &     06000000 , 3 , 'CCD' , 'PRC' , 6 , 06000200/
C      dimension gcl6(55)
      data Gcl6/3 , 'GEN' , 'GCL' , 2 , 07000000 , 5 , 'GUES' , 'S' ,
     &     'GES' , 0 , 0 , 6 , 'SCFC' , 'YC' , 'SCF' , 0 , 0 , 6 ,
     &     'VSHI' , 'FT' , 'LEV' , 0 , 0 , 6 , 'NOSY' , 'MM' , 'GCL' ,
     &     23 , 1 , 4 , 'SYMM' , 'SYM' , 0 , 0 , 3 , 'OPT' , 'OPT' , 1 ,
     &     2 , 2 , 'SP' , 'GCL' , 1 , 1 , 4 , 'FREQ' , 'GCL' , 1 , 6 ,
     &     6 , 'STAB' , 'LE' , 'STB' , 1 , 7/
C      dimension gcl7(91)
      data Gcl7/8 , 'NOEX' , 'TRAP' , 'GCL' , 18 , 1 , 4 , 'RAFF' ,
     &     'GCL' , 19 , 1 , 6 , 'NORA' , 'FF' , 'GCL' , 19 , 2 , 6 ,
     &     'DIRE' , 'CT' , 'GCL' , 49 , 1 , 6 , 'PSEU' , 'DO' , 'PSD' ,
     &     0 , 0 , 5 , 'NOPO' , 'P' , 'GCL' , 20 , 1 , 6 , 'MINP' ,
     &     'OP' , 'GCL' , 20 , 2 , 5 , 'SCFD' , 'M' , 'GCL' , 15 , 1 ,
     &     5 , 'ALTE' , 'R' , 'GCL' , 14 , 1 , 4 , 'SAVE' , 'SAV' , 0 ,
     &     0 , 4 , 'GEOM' , 'GEO' , 0 , 0 , 4 , 'TEST' , 'GCL' , 21 ,
     &     1 , 6 , 'NOND' , 'EF' , 'GCL' , 44 , 1 , 7 , 'CHAR' , 'GES' ,
     &     'GCL' , 45 , 1 , 6 , 'FORC' , 'ES' , 'GCL' , 1 , 5 , 4 ,
     &     'RPAC' , 'GCL' , 47 , 1/
C      dimension gcl8(108)
      data Gcl8/ - 6 , 'STO-' , '2G' , 'STRS' , 2 , 00020000 , -6 ,
     &     'STO-' , '3G' , 'STRS' , 2 , 00030000 , -6 , 'STO-' , '4G' ,
     &     'STRS' , 2 , 00040000 , -6 , 'STO-' , '5G' , 'STRS' , 2 ,
     &     00050000 , -6 , 'STO-' , '6G' , 'STRS' , 2 , 00060000 , -5 ,
     &     '3-21' , 'G' , 'STRS' , 2 , 05000000 , -5 , '4-21' , 'G' ,
     &     'STRS' , 2 , 05040000 , -5 , '6-21' , 'G' , 'STRS' , 2 ,
     &     05060000 , -5 , '4-31' , 'G' , 'STRS' , 2 , 01000000 , -5 ,
     &     '5-31' , 'G' , 'STRS' , 2 , 01050000 , -5 , '6-31' , 'G' ,
     &     'STRS' , 2 , 01060000 , -6 , '6-31' , '1G' , 'STRS' , 2 ,
     &     04060000 , -6 , 'LP-3' , '1G' , 'STRS' , 2 , 03000000 , -7 ,
     &     'LANL' , '1MB' , 'STRS' , 2 , 06000000 , -7 , 'LANL' ,
     &     '1DZ' , 'STRS' , 2 , 06010000 , -7 , 'LAL1' , 'STV' ,
     &     'STRS' , 2 , 06020000 , -7 , 'LAL1' , 'LP3' , 'STRS' , 2 ,
     &     06030000 , -6 , 'LP-4' , '1G' , 'STRS' , 2 , 03040000/
C      dimension gcl9(29)
      data Gcl9/5 , 'COOR' , 'D' , 'GCL' , 42 , 1 , 6 , 'OPTC' , 'YC' ,
     &     'OPCY' , 0 , 0 , 2 , '5D' , 'GCL' , 2 , 1 , 2 , '6D' ,
     &     'GCL' , 2 , 2 , 5 , 'UNIT' , 'S' , 'UNT' , 0 , 0 , 'EOS'/
C
C                              state for getting input units.
C      dimension unit(43)
C
      data Unit/'UNT' , -1 , '=' , 0 , 0 , 0 , -1 , '(' , 'UNT2' , 0 ,
     &     0 , 'NUL' , 'GCL' , 0 , 0 , 'EOS' , 'UNT1' , -1 , '(' ,
     &     'UNT2' , 0 , 0 , 2 , 'AU' , 'GCL' , 38 , 1 , 3 , 'ANG' ,
     &     'GCL' , 38 , 2 , 3 , 'RAD' , 'GCL' , 39 , 1 , 3 , 'DEG' ,
     &     'GCL' , 39 , 2 , 'EOS'/
C
C      dimension unit1(32)
      data Unit1/'UNT2' , -1 , ',' , 'UNT2' , 0 , 0 , -1 , ')' , 'GCL' ,
     &     0 , 0 , 2 , 'AU' , 'UNT2' , 38 , 1 , 3 , 'ANG' , 'UNT2' ,
     &     38 , 2 , 3 , 'RAD' , 'UNT2' , 39 , 1 , 3 , 'DEG' , 'UNT2' ,
     &     39 , 2 , 'EOS'/
C
C-rpac statement below no longer used for rpac
C      dimension nmrop(12)
C
C                              state for getting number of scf cycles.
C      dimension scf(13)
      data Scf/'SCF' , -1 , '=' , 0 , 0 , 0 , 'EOS' , 'SCF1' , 'I10' ,
     &     'GCL' , 24 , 0 , 'EOS'/
C                            state for getting level shifter
C      dimension lev(13)
      data Lev/'LEV' , -1 , '=' , 0 , 0 , 0 , 'EOS' , 'LEV1' , 'I10' ,
     &     'GCL' , 48 , 0 , 'EOS'/
C
C
C                              state for finding 'guess' options.
C      dimension gesop1(46)
C
      data Gesop1/'GES' , -1 , '=' , 0 , 0 , 0 , -1 , '(' , 'GES2' , 0 ,
     &     0 , 'EOS' , 'GES1' , -1 , '(' , 0 , 0 , 0 , 4 , 'READ' ,
     &     'GCL' , 10 , 1 , 4 , 'CORE' , 'GCL' , 10 , 2 , 5 , 'ALTE' ,
     &     'R' , 'GCL' , 14 , 1 , 4 , 'ONLY' , 'GCL' , 1 , 3 , 5 ,
     &     'PRIN' , 'T' , 'GCL' , 32 , 1 , 'EOS'/
C
C      dimension gesop2(39)
      data Gesop2/'GES2' , -1 , ',' , 'GES2' , 0 , 0 , -1 , ')' ,
     &     'GCL' , 0 , 0 , 4 , 'READ' , 'GES2' , 10 , 1 , 4 , 'CORE' ,
     &     'GES2' , 10 , 2 , 5 , 'ALTE' , 'R' , 'GES2' , 14 , 1 , 4 ,
     &     'ONLY' , 'GES2' , 1 , 3 , 5 , 'PRIN' , 'T' , 'GES2' , 32 ,
     &     1 , 'EOS'/
C
C
C                              state for finding 'opt' options.
C      dimension opt(61)
C
      data Opt/'OPT' , -1 , '=' , 0 , 0 , 0 , -1 , '(' , 'OPT2' , 0 ,
     &     0 , 'NUL' , 'GCL' , 0 , 0 , 'EOS' , 'OPT1' , -1 , '(' ,
     &     'OPT2' , 0 , 0 , 4 , 'GRAD' , 'GCL' , 34 , 1 , 2 , 'FP' ,
     &     'GCL' , 34 , 2 , 2 , 'MS' , 'GCL' , 34 , 4 , 6 , 'CALC' ,
     &     'FC' , 'GCL' , 35 , 1 , 6 , 'READ' , 'FC' , 'GCL' , 35 , 2 ,
     &     8 , 'STAR' , 'ONLY' , 'GCL' , 43 , 1 , 2 , 'TS' , 'GCL' ,
     &     16 , 1 , 'EOS'/
C
C      dimension opt2(50)
      data Opt2/'OPT2' , -1 , ',' , 'OPT2' , 0 , 0 , -1 , ')' , 'GCL' ,
     &     0 , 0 , 4 , 'GRAD' , 'OPT2' , 34 , 1 , 2 , 'FP' , 'OPT2' ,
     &     34 , 2 , 2 , 'MS' , 'OPT2' , 34 , 4 , 6 , 'CALC' , 'FC' ,
     &     'OPT2' , 35 , 1 , 6 , 'READ' , 'FC' , 'OPT2' , 35 , 2 , 8 ,
     &     'STAR' , 'ONLY' , 'OPT2' , 43 , 1 , 2 , 'TS' , 'OPT2' , 16 ,
     &     1 , 'EOS'/
C
C
C                              state for getting optcyc.
C      dimension optcyc(11)
      data Optcyc/'OPCY' , -1 , '=' , 'OPCY' , 0 , 0 , 'I10' , 'GCL' ,
     &     41 , 0 , 'EOS'/
C
C
C                              state for procedure options.
C      dimension prc1(68)
C
      data Prc1/'PRC' , -1 , '=' , 0 , 0 , 0 , -1 , '(' , 'PRC2' , 0 ,
     &     0 , 'NUL' , 'GCL' , 0 , 0 , 'EOS' , 'PRC1' , -1 , '(' ,
     &     'PRC2' , 0 , 0 , 1 , 'S' , 'GCL' , 6 , 00000100 , 1 , 'D' ,
     &     'GCL' , 6 , 00000200 , 2 , 'SD' , 'GCL' , 6 , 00000300 , 2 ,
     &     'DS' , 'GCL' , 6 , 00000300 , 2 , 'DQ' , 'GCL' , 6 ,
     &     00000500 , 3 , 'SDQ' , 'GCL' , 6 , 00000400 , 4 , 'SDTQ' ,
     &     'GCL' , 6 , 00000600 , 2 , 'FC' , 'GCL' , 6 , 1 , 4 ,
     &     'FULL' , 'GCL' , 6 , 2 , 'EOS'/
C
C      dimension prc2(57)
      data Prc2/'PRC2' , -1 , ',' , 'PRC2' , 0 , 0 , -1 , ')' , 'GCL' ,
     &     0 , 0 , 1 , 'S' , 'PRC2' , 6 , 00000100 , 1 , 'D' , 'PRC2' ,
     &     6 , 00000200 , 2 , 'SD' , 'PRC2' , 6 , 00000300 , 2 , 'DS' ,
     &     'PRC2' , 6 , 00000300 , 2 , 'DQ' , 'PRC2' , 6 , 00000500 ,
     &     3 , 'SDQ' , 'PRC2' , 6 , 00000400 , 4 , 'SDTQ' , 'PRC2' , 6 ,
     &     00000600 , 2 , 'FC' , 'PRC2' , 6 , 1 , 4 , 'FULL' , 'PRC2' ,
     &     6 , 2 , 'EOS'/
C
C
C                              state for pseudo-pot. options.
C      dimension psd(33)
C
      data Psd/'PSD' , -1 , '=' , 0 , 0 , 0 , -1 , '(' , 'PSD2' , 0 ,
     &     0 , 'NUL' , 'GCL' , 17 , 1 , 'EOS' , 'PSD1' , -1 , '(' ,
     &     'PSD2' , 0 , 0 , 3 , 'CHF' , 'GCL' , 17 , 1 , 4 , 'READ' ,
     &     'GCL' , 17 , 2 , 'EOS'/
C
C      dimension psd2(22)
      data Psd2/'PSD2' , -1 , ',' , 'PSD2' , 0 , 0 , -1 , ')' , 'GCL' ,
     &     0 , 0 , 3 , 'CHF' , 'PSD2' , 17 , 1 , 4 , 'READ' , 'PSD2' ,
     &     17 , 2 , 'EOS'/
C
C                              state for getting stars after bases.
C      dimension stars(16)
      data Stars/'STRS' , -2 , '**' , 'GCL' , 2 , 00000200 , -1 , '*' ,
     &     'GCL' , 2 , 00000100 , 'NUL' , 'GCL' , 0 , 0 , 'EOS'/
C
C                              state for 'save' options.
C      dimension save1(39)
C
      data Save1/'SAV' , -1 , '=' , 0 , 0 , 0 , -1 , '(' , 'SAV2' , 0 ,
     &     0 , 'NUL' , 'SAV3' , 0 , 0 , 'EOS' , 'SAV1' , -1 , '(' ,
     &     'SAV2' , 0 , 0 , 2 , 'MO' , 'GCL' , 13 , 1 , 5 , 'BASI' ,
     &     'S' , 'GCL' , 12 , 1 , 2 , 'FC' , 'GCL' , 11 , 1 , 'EOS'/
C
C      dimension save2(28)
      data Save2/'SAV2' , -1 , ',' , 'SAV2' , 0 , 0 , -1 , ')' , 'GCL' ,
     &     0 , 0 , 2 , 'MO' , 'SAV2' , 13 , 1 , 5 , 'BASI' , 'S' ,
     &     'SAV2' , 12 , 1 , 2 , 'FC' , 'SAV2' , 11 , 1 , 'EOS'/
C
C      dimension save3(18)
      data Save3/'SAV3' , 'NUL' , 0 , 13 , 1 , 'EOS' , '0' , 'NUL' , 0 ,
     &     12 , 1 , 'EOS' , '0' , 'NUL' , 'GCL' , 11 , 1 , 'EOS'/
C
C
C                              state for stability options.
C      dimension stb(90)
C
      data Stb/'STB' , -1 , '=' , 0 , 0 , 0 , -1 , '(' , 'STB2' , 0 ,
     &     0 , 'NUL' , 'GCL' , 0 , 0 , 'EOS' , 'STB1' , -1 , '(' ,
     &     'STB2' , 0 , 0 , 4 , 'SYMM' , 'GCL' , 26 , 1 , 6 , 'NOSY' ,
     &     'MM' , 'GCL' , 26 , 2 , 3 , 'OPT' , 'GCL' , 27 , 1 , 5 ,
     &     'NOOP' , 'T' , 'GCL' , 27 , 2 , 4 , 'RRHF' , 'GCL' , 28 , 1 ,
     &     4 , 'RUHF' , 'GCL' , 28 , 2 , 4 , 'CRHF' , 'GCL' , 28 , 3 ,
     &     4 , 'CUHF' , 'GCL' , 28 , 4 , 4 , 'RGHF' , 'GCL' , 28 , 5 ,
     &     4 , 'CGHF' , 'GCL' , 28 , 6 , 3 , 'INT' , 'GCL' , 29 , 1 ,
     &     4 , 'REXT' , 'GCL' , 30 , 1 , 4 , 'CEXT' , 'GCL' , 31 , 1 ,
     &     'EOS'/
C
C      dimension stb2(79)
      data Stb2/'STB2' , -1 , ',' , 'STB2' , 0 , 0 , -1 , ')' , 'GCL' ,
     &     0 , 0 , 4 , 'SYMM' , 'STB2' , 26 , 1 , 6 , 'NOSY' , 'MM' ,
     &     'STB2' , 26 , 2 , 3 , 'OPT' , 'STB2' , 27 , 1 , 5 , 'NOOP' ,
     &     'T' , 'STB2' , 27 , 2 , 4 , 'RRHF' , 'STB2' , 28 , 1 , 4 ,
     &     'RUHF' , 'STB2' , 28 , 2 , 4 , 'CRHF' , 'STB2' , 28 , 3 , 4 ,
     &     'CUHF' , 'STB2' , 28 , 4 , 4 , 'RGHF' , 'STB2' , 28 , 5 , 4 ,
     &     'CGHF' , 'STB2' , 28 , 6 , 3 , 'INT' , 'STB2' , 29 , 1 , 4 ,
     &     'REXT' , 'STB2' , 30 , 1 , 4 , 'CEXT' , 'STB2' , 31 , 1 ,
     &     'EOS'/
C
C
C                              state for symmetry options.
C      dimension sym(41)
C
      data Sym/'SYM' , -1 , '=' , 0 , 0 , 0 , -1 , '(' , 'SYM2' , 0 ,
     &     0 , 'EOS' , 'SYM1' , -1 , '(' , 0 , 0 , 0 , 3 , 'INT' ,
     &     'GCL' , 36 , 1 , 5 , 'NOIN' , 'T' , 'GCL' , 36 , 2 , 4 ,
     &     'GRAD' , 'GCL' , 37 , 1 , 6 , 'NOGR' , 'AD' , 'GCL' , 37 ,
     &     2 , 'EOS'/
C
C      dimension sym2(35)
      data Sym2/'SYM2' , -1 , ',' , 'SYM2' , 0 , 0 , -1 , ')' , 'GCL' ,
     &     0 , 0 , 3 , 'INT' , 'SYM2' , 36 , 1 , 5 , 'NOIN' , 'T' ,
     &     'SYM2' , 36 , 2 , 4 , 'GRAD' , 'SYM2' , 37 , 1 , 6 , 'NOGR' ,
     &     'AD' , 'SYM2' , 37 , 2 , 'EOS' , 'END'/
C
      end

