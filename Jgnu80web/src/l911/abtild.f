
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 abtild"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "abtild.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "abtild.web"
      subroutine abtild(ATILDA,CA,A,CB,S,NBASIS)
      implicit none
      double precision A,ATILDA,CA,CB,F42,Four,gabs,Half,One,Onept5,S,Te
     &n,Three,thresh,Two,Zero
      integer ia,ib,Ieval,ind,ind1,ind2,ind3,ind4,Ioab,Ispect,Loab,Lspec
     &t,Maxbuc,mu,nb2,NBASIS,Noa,Noa2,Noa3,Noaob
      integer Noava,Noavb,Nob,Nob2,Nob3,Nobva,Nobvb,Novaa,Novab,Novbb,Nr
     &orb,nu,Nva,Nva2,Nva3,Nvavb,Nvb,Nvb2,Nvb3
      dimension ATILDA(*),CA(*),A(*),CB(*),S(*)
      common/orb/Ispect,Lspect,Nrorb,Noa,Nva,Nob,Nvb,Noaob,Noava,Noavb,N
     &obva,Nobvb,Nvavb,Noa2,Noa3,Nob2,Nob3,Nva2,Nva3,Nvb2,Nvb3,Novaa,Nov
     &ab,Novbb,Maxbuc,Ieval,Ioab,Loab
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      data thresh/1.D-8/
      
      
      
      
      
      
      
      
      call track('ABTILD')
      
      if(Nva.LE.0.OR.Nvb.LE.0)return
      nb2=NBASIS*NBASIS
      do 100 ind=1,nb2
      ATILDA(ind)=Zero
100   continue
      
      ind1=0
      ind4=0
      do 300 ia=1,Nva
      do 150 nu=1,NBASIS
      S(nu)=Zero
150   continue
      ind2=0
      do 200 ib=1,Nvb
      ind1=ind1+1
      if(gabs(A(ind1)).GE.thresh)then
      do 160 nu=1,NBASIS
      S(nu)=S(nu)+A(ind1)*CB(ind2+nu)
160   continue
      endif
      ind2=ind2+NBASIS
200   continue
      
      ind3=0
      do 250 mu=1,NBASIS
      ind4=ind4+1
      do 220 nu=1,NBASIS
      ATILDA(ind3+nu)=ATILDA(ind3+nu)+CA(ind4)*S(nu)
220   continue
      ind3=ind3+NBASIS
250   continue
300   continue
      
      return
      
      end
C* :1 * 
      
