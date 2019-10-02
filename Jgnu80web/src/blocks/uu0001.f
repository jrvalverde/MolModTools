
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 uu0001"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "uu0001.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 14 "uu0001.web"
      blockdata uu0001
      implicit none
      integer Dirtab,Dummy,First,Jmptab,Optab,Ovtab,Segtab
      common/bd0001/Dummy
      common/crtab/First(32),Dirtab(28),Ovtab(13),Optab(35),Segtab(29),J
     &mptab(21)
      data First/'TOP',-1,'#','TOP',0,0,6,'NONS','TD','TOP',0,0,1,'N','T
     &OP',0,0,1,'P','TOP',0,0,1,'F','TOP',0,0,'NUL',0,0,0,'EOS'/
      data Dirtab/'DIR',-1,'(',0,0,0,'EOL','EXI',1,-1,'NUL','OV',0,0,'EO
     &S',0,'I10',0,2,0,'EOS',0,-1,')','RET',1,1,'EOS'/
      data Ovtab/'OV','I10',0,2,0,'EOS',0,-1,'/','RET',1,2,'EOS'/
      data Optab/'OP',-1,'/','SEG',0,0,'I10',0,3,0,-1,',','OP',0,0,'EOS'
     &,0,-1,'=',0,1,3,'EOS',0,'I10','RET',2,0,'EOS',0,'NUL','OP',0,0,'EO
     &S'/
      data Segtab/'SEG','I10',0,2,0,'EOS',0,'NUL','RET',1,4,'EOS',0,-1,'
     &(','J',0,0,-1,';','DIR',0,0,-1,',','SEG',0,0,'EOS'/
      data Jmptab/'J','I10',0,2,0,'EOS',0,-1,')','RET',1,5,'EOS',0,-1,';
     &','DIR',0,0,'EOS','END'/
      
      
      end
C* :1 * 
      
