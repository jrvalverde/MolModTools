
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 symort"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "symort.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "symort.web"
      subroutine symort(S,T,BLK,NDIM,N,EVAL)
      implicit none
      double precision BLK,danger,diagth,eigenv,EVAL,one,S,sij,smlest,T,
     &toosml,zero
      integer i,j,k,Lfnao,Lfnarc,Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo,Lfnnab,
     &Lfnnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa
      integer Lfnpr,N,NDIM
      
      
      
      
      dimension S(N,N),T(NDIM,NDIM),BLK(N,N),EVAL(N)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      data zero,one/0.0D0,1.0D0/
      
      
      data diagth,danger/1.0D-12,1.0D3/
      
      do 100 i=1,N
      do 50 j=1,N
      sij=zero
      do 20 k=1,N
      sij=sij+T(k,i)*T(k,j)
20    continue
      S(i,j)=sij
50    continue
100   continue
      call jacobi(N,S,EVAL,BLK,N,N,0)
      smlest=one
      toosml=diagth*danger
      do 200 i=1,N
      eigenv=EVAL(i)
      if(eigenv.LT.toosml)goto 600
      EVAL(i)=one/sqrt(eigenv)
      if(eigenv.LT.smlest)smlest=eigenv
200   continue
      do 300 i=1,N
      do 250 j=1,i
      sij=zero
      do 220 k=1,N
      sij=sij+EVAL(k)*BLK(i,k)*BLK(j,k)
220   continue
      S(i,j)=sij
      S(j,i)=sij
250   continue
300   continue
      
      do 500 i=1,N
      do 350 j=1,N
      EVAL(j)=zero
      do 320 k=1,N
      EVAL(j)=EVAL(j)+T(i,k)*S(k,j)
320   continue
350   continue
      do 400 j=1,N
      T(i,j)=EVAL(j)
400   continue
500   continue
      return
      
600   write(Lfnpr,99001)eigenv,toosml
99001 format(/1x,'An eigenvalue of the overlap matrix of the ','symmetri
     &zed Jacobi transf. ','matrix of ',e13.5,' has been found.'/1x,'Thi
     &s is lower than the allowed threshold of ',e13.5)
      stop
      end
C* :1 * 
      
