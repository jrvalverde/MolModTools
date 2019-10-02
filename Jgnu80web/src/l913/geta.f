
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 geta"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "geta.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "geta.web"
      subroutine geta(E,METHOD,ISCHEM)
      implicit none
      double precision E,V
      integer Iad1,Iad2,Iad3,Ias1,Ias2,Idb1,Idb10,Idb2,Idb3,Idb4,Idb5,Id
     &b6,Idb7,Idb8,Idb9,Idmm,Ieval,ilocal,Ioab,Iopcl
      integer ISCHEM,Iscr1,Iscr2,Iscr3,Iscr4,Iscr5,Iscr6,Iscrd,Ispect,Iw
     &d1,Iwd2,Iwd3,Iws1,Iws2,Loab,Lspect,Maxbuc,Mdv,mdv2,METHOD
      integer nb1,Noa,Noa2,Noa3,Noaob,Noava,Noavb,Nob,Nob2,Nob3,Nobva,No
     &bvb,Novaa,Novab,Novbb,Nrorb,Nva,Nva2,Nva3,Nvavb
      integer Nvb,Nvb2,Nvb3
      dimension E(*)
      common/v/V(20000),Mdv
      common/orb/Ispect,Lspect,Nrorb,Noa,Nva,Nob,Nvb,Noaob,Noava,Noavb,N
     &obva,Nobvb,Nvavb,Noa2,Noa3,Nob2,Nob3,Nva2,Nva3,Nvb2,Nvb3,Novaa,Nov
     &ab,Novbb,Maxbuc,Ieval,Ioab,Loab
      common/constr/Iopcl
      common/bucknr/Idb1,Idb2,Idb3,Idb4,Idb5,Idb6,Idb7,Idb8,Idb9,Idb10,I
     &dmm(11),Iad1,Iad2,Iad3,Ias1,Ias2,Iwd1,Iwd2,Iwd3,Iws1,Iws2,Iscr1,Is
     &cr2,Iscr3,Iscr4,Iscr5,Iscr6,Iscrd(13)
      data ilocal/3333/
      
      
      
      
      
      
      
      
      call track('GETA  ')
      
      mdv2=Mdv/2
      
      call getde(METHOD,ISCHEM)
      
      call expol
      
      nb1=Nrorb+1
      
      call wtoada(Iwd1,Iad1,Idb1,E,Noa,Nva)
      
      
      call defbuc(ilocal,Novab)
      call mattrn(Noa,Nva,Nob,Nvb,2,Idb2,ilocal,mdv2)
      
      call wtoadb(Iwd2,Iad2,ilocal,E,E(nb1))
      
      call fileio(5,ilocal,0,0,0)
      
      if(Iopcl.NE.0)call wtoada(Iwd3,Iad3,Idb3,E(nb1),Nob,Nvb)
      
      call wtoas(Iws1,Ias1,E,Noa,Nva)
      if(Iopcl.NE.0)call wtoas(Iws2,Ias2,E(nb1),Nob,Nvb)
      
      return
      
      end
C* :1 * 
      
