
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 uu0003"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "uu0003.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 14 "uu0003.web"
      blockdata uu0003
      implicit none
      integer Iop,Iva,Nca,Next,Noc
      dimension Nca(10)
      dimension Noc(22)
      dimension Iop(26)
      dimension Iva(13)
      dimension Next(21)
      common/option/Nca,Noc,Iop,Iva,Next
      data Nca/'NCA','I10','NOC',1,0,'EOL','EXI',0,0,'EOS'/
      data Noc/'NOC',-1,'(','NOC2',0,0,'NUL','IOP',0,0,'EOS','NOC2',-1,'
     &*','IOP',5,1,'I10','IOP',2,0,'EOS'/
      data Iop/'IOP',-1,':','IOP',0,0,-1,'/','IOP',0,0,-1,',','IOP',0,0,
     &-1,')','IOP',0,0,'I10','IVA',3,0,'EOS'/
      data Iva/'IVA',-1,'=','IVA2',0,0,'EOS','IVA2','I10','RET',4,0,'EOS
     &'/
      data Next/'NEXT','EOL','EXI',0,0,-1,';','RET',5,2,'NUL','IOP',0,0,
     &'EOS','JUMP','NUL','NCA',0,0,'EOS'/
      
      end
C* :1 * 
      
