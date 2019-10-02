
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dd2"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dd2.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "dd2.web"
      subroutine dd2
      implicit none
      double precision Cmo,E,F42,Filmoc,Four,Half,One,Onept5,Ten,Three,T
     &wo,V1,V2,Zero
      integer iaabb,Iad1,Iad2,Iad3,Ias1,Ias2,ibuc1,ibuc2,ibuc3,Idb1,Idb1
     &0,Idb2,Idb3,Idb4,Idb5,Idb6,Idb7,Idb8,Idb9,Idmm
      integer iend,Ieval,Ioab,Iopcl,Iscr1,Iscr2,Iscr3,Iscr4,Iscr5,Iscr6,
     &Iscrd,Ispect,ispin,Iwd1,Iwd2,Iwd3,Iws1,Iws2,length,Loab
      integer Lspect,Maxbuc,Mdv,mdv2,n,n1,n2,no,Noa,Noa2,Noa3,Noaob,Noav
     &a,Noavb,Nob,Nob2,Nob3,Nobva,Nobvb,Novaa
      integer Novab,Novbb,Nrorb,nv,Nva,Nva2,Nva3,Nvavb,Nvb,Nvb2,Nvb3
      common/orb/Ispect,Lspect,Nrorb,Noa,Nva,Nob,Nvb,Noaob,Noava,Noavb,N
     &obva,Nobvb,Nvavb,Noa2,Noa3,Nob2,Nob3,Nva2,Nva3,Nvb2,Nvb3,Novaa,Nov
     &ab,Novbb,Maxbuc,Ieval,Ioab,Loab
      common/v/V1(10000),V2(10000),Mdv
      common/moc/Cmo(6225),E(175),Filmoc(6400)
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      common/bucknr/Idb1,Idb2,Idb3,Idb4,Idb5,Idb6,Idb7,Idb8,Idb9,Idb10,I
     &dmm(11),Iad1,Iad2,Iad3,Ias1,Ias2,Iwd1,Iwd2,Iwd3,Iws1,Iws2,Iscr1,Is
     &cr2,Iscr3,Iscr4,Iscr5,Iscr6,Iscrd(13)
      common/constr/Iopcl
      
      
      
      
      
      
      call track('DD2   ')
      
      mdv2=Mdv/2
      no=max0(Noa,Nob)
      nv=max0(Nva,Nvb)
      length=(max0(no,nv)*no)**2
      call defbuc(Iscr1,length)
      call defbuc(Iscr2,length)
      
      if(Iopcl.NE.0)then
      iend=Iopcl+1
      ispin=1
      do 50 iaabb=1,iend
      if(iaabb.LE.1)then
      n1=Noa3
      n2=Nva3
      n=Noa
      ibuc1=Iad1
      ibuc2=Idb4
      ibuc3=Iwd1
      else
      
      n1=Nob3
      n2=Nvb3
      n=Nob
      ibuc1=Iad3
      ibuc2=Idb9
      ibuc3=Iwd3
      endif
      if(n1.NE.0.AND.n2.NE.0)then
      call expsym(n1,ibuc2,Iscr2)
      call matca2(n1,n1,n2,Iscr2,ibuc1,ibuc3,+1,1)
      endif
      ispin=3
50    continue
      endif
      
      ispin=2
      if(Noaob.NE.0.AND.Nvavb.NE.0)then
      call exp78(Noa,Nob,Idb6,Iscr1)
      call mattrn(Noa,Noa,Nob,Nob,2,Iscr1,Iscr2,mdv2)
      call matca2(Noaob,Noaob,Nvavb,Iscr2,Iad2,Iwd2,+1,1)
      endif
      
      call fileio(6,0,0,0,0)
      
      return
      
      end
C* :1 * 
      
