
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 sls"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "sls.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "sls.web"
      subroutine sls(IOP,A,MDIM,NBASIS)
      implicit none
      double precision A,Big,Four,One,Onept5,Pt5,Small,Three,Two,Zero
      integer i,i1,ii,imdim,IOP,j,jj,Ksm,Kspin,Ksw,MDIM,mdsq,mm,mtt,n1,N
     &BASIS,Nesk,Nest,Nest1,Nse
      integer Nsep,ntt
      logical Cmp,Rhf
      dimension A(MDIM,*)
      common/scfcon/Cmp,Rhf,Ksm,Kspin,Ksw(2),Nesk(2),Nse,Nsep,Nest,Nest1
      common/const/Zero,Pt5,One,Onept5,Two,Three,Four,Big,Small
      
      
      mdsq=MDIM**2
      mtt=MDIM*(MDIM+1)/2
      ntt=NBASIS*(NBASIS+1)/2
      if(IOP.NE.2)then
      
      mm=0
      do 50 i=1,NBASIS
      imdim=i+MDIM
      do 20 j=1,i
      mm=mm+1
      A(mm,1)=A(j,i)
      if(Cmp)A(mm+mdsq,1)=A(j,imdim)
20    continue
50    continue
      if(.NOT.Cmp)return
      do 100 i=1,ntt
      A(i+mtt,1)=A(i+mdsq,1)
100   continue
      return
      
      elseif(Cmp)then
      do 150 i=1,ntt
      A(mdsq+i,1)=A(mtt+i,1)
150   continue
      endif
      n1=NBASIS+1
      mm=ntt+1
      do 300 ii=1,NBASIS
      i=n1-ii
      imdim=i+MDIM
      i1=i+1
      do 200 jj=1,i
      j=i1-jj
      mm=mm-1
      A(j,i)=A(mm,1)
      if(Cmp)A(j,imdim)=A(mm+mdsq,1)
200   continue
300   continue
      do 400 i=1,NBASIS
      imdim=i+MDIM
      if(Cmp)A(i,imdim)=Zero
      do 350 j=1,i
      A(i,j)=A(j,i)
      if(Cmp)A(i,j+MDIM)=-A(j,imdim)
350   continue
400   continue
      return
      
      end
C* :1 * 
      
