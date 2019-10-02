
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dd3"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dd3.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "dd3.web"
      subroutine dd3
      implicit none
      double precision F42,Four,Half,One,Onept5,Ten,Three,Two,V,Zero
      integer Iad1,Iad2,Iad3,Ias1,Ias2,Idb1,Idb10,Idb2,Idb3,Idb4,Idb5,Id
     &b6,Idb7,Idb8,Idb9,Idmm,Ieval,Ioab,Iopcl,Iscr1
      integer Iscr2,Iscr3,Iscr4,Iscr5,Iscr6,Iscrd,Ispect,Iwd1,Iwd2,Iwd3,
     &Iws1,Iws2,l,l1,l2,Loab,Lspect,max,Maxbuc,Mdv
      integer Noa,Noa2,Noa3,Noaob,Noava,Noavb,Nob,Nob2,Nob3,Nobva,Nobvb,
     &Novaa,Novab,Novbb,Nrorb,Nva,Nva2,Nva3,Nvavb,Nvb
      integer Nvb2,Nvb3
      common/orb/Ispect,Lspect,Nrorb,Noa,Nva,Nob,Nvb,Noaob,Noava,Noavb,N
     &obva,Nobvb,Nvavb,Noa2,Noa3,Nob2,Nob3,Nva2,Nva3,Nvb2,Nvb3,Novaa,Nov
     &ab,Novbb,Maxbuc,Ieval,Ioab,Loab
      common/v/V(20000),Mdv
      common/bucknr/Idb1,Idb2,Idb3,Idb4,Idb5,Idb6,Idb7,Idb8,Idb9,Idb10,I
     &dmm(11),Iad1,Iad2,Iad3,Ias1,Ias2,Iwd1,Iwd2,Iwd3,Iws1,Iws2,Iscr1,Is
     &cr2,Iscr3,Iscr4,Iscr5,Iscr6,Iscrd(13)
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      common/constr/Iopcl
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      call track('DD3   ')
      
      l1=max0(Noa,Nob)
      l2=max0(Nva,Nvb)
      l=l1**2*l2**2
      call defbuc(Iscr1,l)
      call defbuc(Iscr2,l)
      call defbuc(Iscr3,l)
      call defbuc(Iscr4,l)
      call defbuc(Iscr5,l)
      max=Mdv/2
      
      
      call mattrn(Noa,Nob,Nva,Nvb,2,Iad2,Iscr3,max)
      call trsfr(Novab,Iscr3,Iad2)
      call mattrn(Noa,Nob,Nva,Nvb,2,Iwd2,Iscr3,max)
      call trsfr(Novab,Iscr3,Iwd2)
      
      call mattrn(Noa,Nva,Noa,Nva,4,Idb5,Iscr2,max)
      call mattrn(Noa,Nva,Nva,Noa,3,Iscr2,Iscr5,max)
      call expand(Iad3,Iscr4,Nob,Nvb)
      call mattrn(Nob,Nob,Nvb,Nvb,2,Iscr4,Iscr2,max)
      call matca2(Noava,Noava,Nobvb,Iscr5,Iad2,Iscr1,-1,0)
      
      call matca2(Nobvb,Noava,Nobvb,Idb2,Iscr2,Iscr1,+1,1)
      if(Iopcl.EQ.0)then
      call transp(Noava,Nobvb,Iscr1,Iscr2)
      call sumn(Iscr2,Iscr1,Novab,One)
      endif
      call sumn(Iscr1,Iwd2,Novab,One)
      call test(2,Iscr1)
      if(Iopcl.NE.0)then
      
      call expand(Iad1,Iscr1,Noa,Nva)
      call mattrn(Noa,Noa,Nva,Nva,2,Iscr1,Iscr2,max)
      call matca2(Noava,Noava,Noava,Iscr2,Iscr5,Iscr1,-1,0)
      
      call matca1(Nobvb,Noava,Noava,Iad2,Idb2,Iscr1,+1,1)
      call mattrn(Noa,Nva,Noa,Nva,2,Iscr1,Iscr4,max)
      call sumant(Iscr4,Iscr1,Noa,Nva)
      call sumn(Iscr1,Iwd1,Noa3*Nva3,One)
      call test(1,Iscr1)
      
      call matca2(Noava,Noava,Nobvb,Iscr2,Idb2,Iwd2,+1,1)
      
      call mattrn(Nob,Nvb,Nob,Nvb,4,Idb10,Iscr2,max)
      call mattrn(Nob,Nvb,Nvb,Nob,3,Iscr2,Iscr5,max)
      call matca2(Nobvb,Noava,Nobvb,Iad2,Iscr5,Iwd2,-1,1)
      call test(2,Iscr2)
      endif
      
      call mattrn(Noa,Nva,Nob,Nvb,4,Iad2,Iscr4,max)
      call mattrn(Noa,Nvb,Nva,Nob,3,Iscr4,Iscr2,max)
      if(Iopcl.NE.0)then
      call exp78(Nob,Nva,Idb8,Iscr3)
      call mattrn(Nob,Nob,Nva,Nva,2,Iscr3,Iscr4,max)
      call matca2(Nobva,Noavb,Nobva,Iscr2,Iscr4,Iscr1,-1,0)
      endif
      
      call exp78(Noa,Nvb,Idb7,Iscr3)
      call mattrn(Noa,Noa,Nvb,Nvb,2,Iscr3,Iscr4,max)
      call matca2(Noavb,Noavb,Nobva,Iscr4,Iscr2,Iscr1,-1,Iopcl)
      if(Iopcl.EQ.0)then
      call transp(Noavb,Nobva,Iscr1,Iscr2)
      call sumn(Iscr2,Iscr1,Novab,One)
      endif
      call mattrn(Noa,Nvb,Nob,Nva,4,Iscr1,Iscr4,max)
      call mattrn(Noa,Nva,Nvb,Nob,3,Iscr4,Iscr1,max)
      call sumn(Iscr1,Iwd2,Novab,One)
      call test(2,Iscr1)
      if(Iopcl.NE.0)then
      
      call expand(Iad3,Iscr3,Nob,Nvb)
      call mattrn(Nob,Nob,Nvb,Nvb,2,Iscr3,Iscr2,max)
      call matca2(Nobvb,Nobvb,Nobvb,Iscr2,Iscr5,Iscr1,-1,0)
      
      call matcab(Noava,Nobvb,Nobvb,Iad2,Idb2,Iscr1,+1,1)
      call mattrn(Nob,Nvb,Nob,Nvb,2,Iscr1,Iscr2,max)
      call sumant(Iscr2,Iscr1,Nob,Nvb)
      call sumn(Iscr1,Iwd3,Nob3*Nvb3,One)
      call test(3,Iscr1)
      endif
      
      call mattrn(Noa,Nva,Nob,Nvb,2,Iad2,Iscr1,max)
      call trsfr(Novab,Iscr1,Iad2)
      call mattrn(Noa,Nva,Nob,Nvb,2,Iwd2,Iscr1,max)
      call trsfr(Novab,Iscr1,Iwd2)
      
      call fileio(6,0,0,0,0)
      
      return
      
      end
C* :1 * 
      
