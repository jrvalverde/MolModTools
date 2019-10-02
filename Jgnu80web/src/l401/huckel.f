
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 huckel"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "huckel.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 41 "huckel.web"
      subroutine huckel(A,B,MD,NBASIS,IAN,NATOMS,ENEG,IOS,IBASIS,IFDON1,
     &IFDON2,I5OR6D,IFPONH)
      implicit none
      real*8 A,B,ENEG,gfloat,one,pt0015,pt0045,pt32,pt4375,pt875,scale,t
     &wo,xii
      integer i,I5OR6D,ia,Ialt,IAN,IBASIS,Iblock,Icmp,Icmplt,Idgn,Idon1,
     &Idon2,Idump,IFDON1,IFDON2,IFPONH,Iguess,im1,Imix,In
      integer IOS,Iout,Ipolh,Iprint,Iproj,Ipunch,Iscale,Ismear,Itst,Iuhf
     &,j,Jbasis,l,MD,NATOMS,NBASIS
      common/io/In,Iout,Ipunch
      common/ops401/Iguess,Iproj,Iuhf,Icmp,Ialt,Imix,Idgn,Iscale,Ismear,
     &Iblock,Icmplt,Itst,Jbasis,Ipolh,Idon1,Idon2,Iprint,Idump
      
      dimension A(MD,MD),B(MD,MD),ENEG(36,11),IAN(NATOMS)
      
      data pt0045/.0045D0/,pt0015/.0015D0/,pt32/.32D0/,one/1.0D0/,two/2.
     &0D0/
      data pt875/.875D0/
      data pt4375/.4375D0/
      
      
      if(Idump.NE.0)write(Iout,99001)
99001 format('  HUCKEL')
      
      scale=pt875
      if(IBASIS.NE.0)scale=scale+pt4375
      if(Iscale.NE.0)scale=gfloat(Iscale)*pt4375
      do 100 i=3,36
      xii=gfloat(i*i)
      ENEG(i,1)=-pt32*xii
100   continue
      do 200 i=11,36
      xii=gfloat(i*i*i)
      ENEG(i,2)=-pt0045*xii
      ENEG(i,3)=-pt0015*xii
200   continue
      
      
      ENEG(1,1)=-0.537
      ENEG(2,1)=-0.735
      ENEG(3,2)=-0.300
      ENEG(3,3)=-0.100
      ENEG(4,2)=-0.683
      ENEG(4,3)=-0.240
      ENEG(5,2)=-1.10
      ENEG(5,3)=-0.33
      ENEG(6,2)=-1.55
      ENEG(6,3)=-0.37
      ENEG(7,2)=-1.86
      ENEG(7,3)=-0.42
      ENEG(8,2)=-2.50
      ENEG(8,3)=-0.46
      ENEG(9,2)=-3.001
      ENEG(9,3)=-0.52
      ENEG(11,4)=-0.570
      ENEG(11,5)=-0.110
      ENEG(12,4)=-0.787
      ENEG(12,5)=-0.250
      ENEG(13,4)=-0.906
      ENEG(13,5)=-0.371
      ENEG(14,4)=-1.300
      ENEG(14,5)=-0.700
      ENEG(15,4)=-1.500
      ENEG(15,5)=-0.900
      ENEG(16,4)=-1.800
      ENEG(16,5)=-1.100
      ENEG(17,4)=-2.100
      ENEG(17,5)=-1.300
      
      
      ENEG(1,2)=one
      ENEG(1,3)=one
      ENEG(2,2)=two
      ENEG(2,3)=two
      do 300 i=3,10
      ENEG(i,4)=gfloat(i)
      ENEG(i,6)=gfloat(i)
      ENEG(i,5)=gfloat(i)
300   continue
      do 400 i=11,18
      ENEG(i,7)=gfloat(i)
      ENEG(i,6)=gfloat(i)
      ENEG(i,8)=gfloat(i)
400   continue
      do 500 i=19,36
      ENEG(i,9)=gfloat(i)
      ENEG(i,10)=gfloat(i)
      ENEG(i,11)=gfloat(i)
500   continue
      
      
      l=1
      do 700 i=1,NATOMS
      ia=IAN(i)
      A(l,l)=ENEG(ia,1)
      l=l+1
      if(ia.LE.2)then
      
      
      if(IBASIS+IFPONH.LT.1)then
      elseif(IBASIS+IFPONH.EQ.1)then
      A(l,l)=ENEG(ia,2)
      l=l+1
      if(IBASIS.LE.0)then
      A(l,l)=ENEG(ia,2)
      A(l+1,l+1)=ENEG(ia,2)
      l=l+2
      endif
      else
      goto 550
      endif
      goto 700
      endif
550   A(l,l)=ENEG(ia,2)
      A(l+1,l+1)=ENEG(ia,3)
      A(l+2,l+2)=ENEG(ia,3)
      A(l+3,l+3)=ENEG(ia,3)
      l=l+4
      if(ia.LE.2)goto 700
      if(ia.LE.10)then
      
      
      if(IBASIS+IFDON1.LT.1)goto 700
      if(IBASIS+IFDON1.NE.1)then
      A(l,l)=ENEG(ia,4)
      A(l+1,l+1)=ENEG(ia,5)
      A(l+2,l+2)=ENEG(ia,5)
      A(l+3,l+3)=ENEG(ia,5)
      l=l+4
      goto 600
      elseif(IBASIS.LE.0)then
      goto 600
      endif
      endif
      A(l,l)=ENEG(ia,4)
      A(l+1,l+1)=ENEG(ia,5)
      A(l+2,l+2)=ENEG(ia,5)
      A(l+3,l+3)=ENEG(ia,5)
      l=l+4
      if(ia.LE.10)goto 700
      if(ia.GT.18)goto 650
      
      
      if(IBASIS+IFDON2.LT.1)goto 700
      if(IBASIS+IFDON2.NE.1)then
      A(l,l)=ENEG(ia,7)
      A(l+1,l+1)=ENEG(ia,8)
      A(l+2,l+2)=ENEG(ia,8)
      A(l+3,l+3)=ENEG(ia,8)
      l=l+4
      elseif(IBASIS.GT.0)then
      goto 650
      endif
600   A(l,l)=ENEG(ia,6)
      A(l+1,l+1)=ENEG(ia,6)
      A(l+2,l+2)=ENEG(ia,6)
      A(l+3,l+3)=ENEG(ia,6)
      A(l+4,l+4)=ENEG(ia,6)
      l=l+5
      if(I5OR6D.GT.0)then
      A(l,l)=ENEG(ia,6)
      l=l+1
      endif
      goto 700
650   A(l,l)=ENEG(ia,7)
      A(l+1,l+1)=ENEG(ia,8)
      A(l+2,l+2)=ENEG(ia,8)
      A(l+3,l+3)=ENEG(ia,8)
      l=l+4
      if(ia.GT.18)call geserr(9)
700   continue
      
      
      call tread(IOS,B,MD,MD,NBASIS,NBASIS,1)
      
      
      call blockh(B,MD,IAN,NATOMS,IBASIS,IFDON1,IFDON2,I5OR6D,IFPONH)
      
      
      
      do 800 i=2,NBASIS
      im1=i-1
      do 750 j=1,im1
      A(i,j)=B(i,j)*(A(i,i)+A(j,j))*scale
      A(j,i)=A(i,j)
750   continue
800   continue
      if(Idump.NE.0)call gesprt(5,A,IOS,MD,NBASIS,NBASIS)
      return
      end
C* :1 * 
      
