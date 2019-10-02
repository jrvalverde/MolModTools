
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 matmul"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "matmul.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "matmul.web"
      subroutine matmul(NBASIS,A,B,KAT,KBT,KCT)
      implicit none
      double precision A,a0,ai,ais,ar,ars,B,bi,Big,br,Filscr,Four,One,On
     &ept5,Pt5,S,Small,T,Three,Two
      double precision Zero
      integer i,i1,j,j1i,j1r,k,KAT,KBT,KCT,Ksm,Kspin,Ksw,Mdim,Mdsq,Mshif
     &s,Mtt,NBASIS,Nesk,Nest,Nest1
      integer Nse,Nsep,Ntt
      logical Cmp,Rhf
      dimension A(*),B(*)
      common/const/Zero,Pt5,One,Onept5,Two,Three,Four,Big,Small
      common/scfcon/Cmp,Rhf,Ksm,Kspin,Ksw(2),Nesk(2),Nse,Nsep,Nest,Nest1
      common/maxdm/Mdim,Mtt,Ntt,Mdsq,Mshifs
      common/scr/S(70),T(70),Filscr(5492)
      
      
      
      if(mod(KAT,2).NE.0)call herm(NBASIS,A,KAT)
      if(mod(KBT,2).NE.0)call herm(NBASIS,B,KBT)
      
      if(Cmp)then
      
      
      if(KCT.EQ.2)then
      
      
      if(KBT.GT.1)goto 500
      ars=Zero
      if(KAT.NE.0)then
      
      do 10 i=1,Ntt
      ars=ars+A(i)*B(i)+A(i+Mtt)*B(i+Mtt)
10    continue
      else
      do 20 i=1,Ntt
      ars=ars+A(i)*B(i)-A(i+Mtt)*B(i+Mtt)
20    continue
      endif
      goto 300
      elseif(KAT.LE.1.OR.KBT.LE.1)then
      if(KAT.GT.1)then
      
      do 40 i=1,NBASIS
      do 25 j=1,NBASIS
      j1r=(j-1)*Mdim
      S(j)=A(i+j1r)
25    continue
      do 30 j=1,NBASIS
      j1r=(j-1)*Mdim
      j1i=j1r+Mdsq
      ars=Zero
      ais=Zero
      do 26 k=1,NBASIS
      ar=S(k)
      br=B(k+j1r)
      bi=B(k+j1i)
      ars=ars+ar*br
      ais=ais+ar*bi
26    continue
      A(i+j1r)=ars
      A(i+j1r)=ars
      A(i+j1i)=ais
30    continue
40    continue
      elseif(KBT.GT.1)then
      
      do 60 i=1,NBASIS
      do 45 j=1,NBASIS
      j1r=(j-1)*Mdim
      j1i=j1r+Mdsq
      S(j)=A(i+j1r)
      T(j)=A(i+j1i)
45    continue
      do 50 j=1,NBASIS
      j1r=(j-1)*Mdim
      j1i=j1r+Mdsq
      ars=Zero
      ais=Zero
      do 46 k=1,NBASIS
      ar=S(k)
      ai=T(k)
      br=B(k+j1r)
      ars=ars+ar*br
      ais=ais+ai*br
46    continue
      A(i+j1r)=ars
      A(i+j1i)=ais
50    continue
60    continue
      else
      
      do 80 i=1,NBASIS
      do 65 j=1,NBASIS
      j1r=(j-1)*Mdim
      j1i=j1r+Mdsq
      S(j)=A(i+j1r)
      T(j)=A(i+j1i)
65    continue
      do 70 j=1,NBASIS
      j1r=(j-1)*Mdim
      j1i=j1r+Mdsq
      ars=Zero
      ais=Zero
      do 66 k=1,NBASIS
      ar=S(k)
      ai=T(k)
      br=B(k+j1r)
      bi=B(k+j1i)
      ars=ars+ar*br-ai*bi
      ais=ais+ar*bi+ai*br
66    continue
      A(i+j1r)=ars
      A(i+j1i)=ais
70    continue
80    continue
      endif
      
      if(KCT.NE.0)call herm(NBASIS,A,0)
      return
      endif
      
      
      elseif(KCT.EQ.2)then
      goto 500
      endif
      do 200 i=1,NBASIS
      do 100 j=1,NBASIS
      j1r=(j-1)*Mdim
      S(j)=A(j1r+i)
100   continue
      do 150 j=1,NBASIS
      j1r=(j-1)*Mdim
      a0=Zero
      do 120 k=1,NBASIS
      a0=a0+S(k)*B(j1r+k)
120   continue
      A(i+j1r)=a0
150   continue
200   continue
      if(KCT.NE.0)call herm(NBASIS,A,2)
      return
300   ars=ars+ars
      i1=0
      do 400 i=1,NBASIS
      i1=i1+i
      ars=ars-A(i1)*B(i1)
400   continue
      A(1)=ars
      return
      
500   ars=Zero
      do 600 i=1,Ntt
      ars=ars+A(i)*B(i)
600   continue
      goto 300
      
      end
C* :1 * 
      
