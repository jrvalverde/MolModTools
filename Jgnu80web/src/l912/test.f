
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 test"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "test.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "test.web"
      subroutine test(I,IWD)
      implicit none
      double precision de,scalp
      integer I,Iad,Iad1,Iad2,Iad3,Ias1,Ias2,Idb1,Idb10,Idb2,Idb3,Idb4,I
     &db5,Idb6,Idb7,Idb8,Idb9,Idmm,Idummy,Idump
      integer Ieval,In,Ioab,Iout,Iprint,Ipunch,Iscr1,Iscr2,Iscr3,Iscr4,I
     &scr5,Iscr6,Iscrd,Ispect,IWD,Iwd1,Iwd2,Iwd3,Iws1,Iws2
      integer ln,Loab,Lspect,Maxbuc,Noa,Noa2,Noa3,Noaob,Noava,Noavb,Nob,
     &Nob2,Nob3,Nobva,Nobvb,Novaa,Novab,Novbb,Nrorb,Nva
      integer Nva2,Nva3,Nvavb,Nvb,Nvb2,Nvb3
      dimension Iad(3)
      common/orb/Ispect,Lspect,Nrorb,Noa,Nva,Nob,Nvb,Noaob,Noava,Noavb,N
     &obva,Nobvb,Nvavb,Noa2,Noa3,Nob2,Nob3,Nva2,Nva3,Nvb2,Nvb3,Novaa,Nov
     &ab,Novbb,Maxbuc,Ieval,Ioab,Loab
      common/bucknr/Idb1,Idb2,Idb3,Idb4,Idb5,Idb6,Idb7,Idb8,Idb9,Idb10,I
     &dmm(11),Iad1,Iad2,Iad3,Ias1,Ias2,Iwd1,Iwd2,Iwd3,Iws1,Iws2,Iscr1,Is
     &cr2,Iscr3,Iscr4,Iscr5,Iscr6,Iscrd(13)
      common/io/In,Iout,Ipunch
      common/print/Iprint
      common/dump/Idump,Idummy
      equivalence(Iad(1),Iad1)
      
      
      
      
      
      
      
      
99001 format(' ',i5,50x,d18.8)
99002 format(1H )
      
      if(Iprint.EQ.0)return
      
      if(I.EQ.2)then
      
      ln=Noa*Nob*Nva*Nvb
      elseif(I.EQ.3)then
      
      ln=Nob3*Nvb3
      else
      
      ln=Noa3*Nva3
      endif
      de=scalp(Iad(I),IWD,ln)
      if(Idump.NE.0)write(Iout,99002)
      if(Iprint.NE.0)write(Iout,99002)
      write(Iout,99001)I,de
      
      return
      
      end
C* :1 * 
      
