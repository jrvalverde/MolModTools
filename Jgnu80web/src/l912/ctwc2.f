
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ctwc2"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ctwc2.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "ctwc2.web"
      subroutine ctwc2(W,CA,CB,WT,NPIJ,S,INITW,NBASIS)
      implicit none
      double precision a0,CA,CB,F42,Four,Half,One,Onept5,S,Ten,Three,Two
     &,W,WT,Zero
      integer ia,ia1,ib,ib1,Ieval,ij,ija,Ioab,Ispect,Loab,Lspect,m,Maxbu
     &c,mu,mu1,NBASIS,nbsq,Noa,Noa2,Noa3
      integer Noaob,Noava,Noavb,Nob,Nob2,Nob3,Nobva,Nobvb,Novaa,Novab,No
     &vbb,NPIJ,Nrorb,nu,Nva,Nva2,Nva3,Nvavb,Nvb,Nvb2
      integer Nvb3
      dimension W(*),CA(*),CB(*),WT(*),S(*)
      logical INITW
      common/orb/Ispect,Lspect,Nrorb,Noa,Nva,Nob,Nvb,Noaob,Noava,Noavb,N
     &obva,Nobvb,Nvavb,Noa2,Noa3,Nob2,Nob3,Nva2,Nva3,Nvb2,Nvb3,Novaa,Nov
     &ab,Novbb,Maxbuc,Ieval,Ioab,Loab
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      
      
      
      
      
      
      call track('CTWC2 ')
      
      if(NPIJ.LE.0)return
      nbsq=NBASIS**2
      if(Nvavb.LE.0)return
      
      m=0
      ija=0
      do 100 ij=1,NPIJ
      ia1=0
      do 50 ia=1,Nva
      
      do 20 nu=1,NBASIS
      a0=Zero
      mu1=ija+nu
      do 10 mu=1,NBASIS
      a0=a0+CA(ia1+mu)*WT(mu1)
      mu1=mu1+NBASIS
10    continue
      S(nu)=a0
20    continue
      ib1=0
      if(INITW)then
      
      do 30 ib=1,Nvb
      a0=Zero
      do 25 nu=1,NBASIS
      a0=a0+CB(ib1+nu)*S(nu)
25    continue
      m=m+1
      W(m)=W(m)+a0
      ib1=ib1+NBASIS
30    continue
      else
      
      do 40 ib=1,Nvb
      a0=Zero
      do 35 nu=1,NBASIS
      a0=a0+CB(ib1+nu)*S(nu)
35    continue
      m=m+1
      W(m)=a0
      ib1=ib1+NBASIS
40    continue
      endif
      
      ia1=ia1+NBASIS
50    continue
      ija=ija+nbsq
100   continue
      
      return
      
      end
C* :1 * 
      
