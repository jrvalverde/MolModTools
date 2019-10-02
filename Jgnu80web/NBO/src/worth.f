
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 worth"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "worth.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 27 "worth.web"
      subroutine worth(S,T,BLK,LIST,NDIM,NBAS,N,OCC,EVAL,BIGBLK)
      implicit none
      double precision BIGBLK,BLK,danger,diagth,eigenv,EVAL,OCC,one,S,si
     &j,smlest,T,tik,tki,tkj,toosml,wtmax,wtthr,zero
      integer i,im1,ip,j,jp,jp1,k,kp,Lfnao,Lfnarc,Lfndaf,Lfndef,Lfndm,Lf
     &nin,Lfnmo,Lfnnab,Lfnnao,Lfnnbo,Lfnnho,Lfnnlm
      integer Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lfnpr,LIST,N,NBAS,NDIM,
     &ntime
      
      
      
      
      dimension S(NDIM,NDIM),T(NDIM,NDIM),BLK(N,N)
      dimension OCC(NDIM),LIST(NDIM),EVAL(NDIM),BIGBLK(NDIM,NDIM)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      data zero,one/0.0D0,1.0D0/
      data ntime/0/
      
      
      data wtthr,diagth,danger/1.0D-3,1.0D-12,1.0D3/
      
      ntime=ntime+1
      wtmax=zero
      do 100 i=1,N
      ip=LIST(i)
      if(OCC(ip).GT.wtmax)wtmax=OCC(ip)
100   continue
      do 200 i=1,N
      ip=LIST(i)
      EVAL(ip)=OCC(ip)/wtmax
      if(EVAL(ip).LT.wtthr)EVAL(ip)=wtthr
200   continue
      do 300 j=1,N
      jp=LIST(j)
      do 250 i=1,NBAS
      T(i,jp)=T(i,jp)*EVAL(jp)
250   continue
300   continue
      do 400 i=1,N
      ip=LIST(i)
      do 350 j=1,NBAS
      sij=zero
      do 320 k=1,NBAS
      tki=T(k,ip)
      if(tki.NE.zero)sij=sij+tki*S(k,j)
320   continue
      BIGBLK(j,i)=sij
350   continue
400   continue
      do 500 i=1,N
      do 450 j=1,i
      jp=LIST(j)
      sij=zero
      do 420 k=1,NBAS
      tkj=T(k,jp)
      if(tkj.NE.zero)sij=sij+BIGBLK(k,i)*tkj
420   continue
      S(j,i)=sij
450   continue
500   continue
      call jacobi(N,S,EVAL,BLK,NDIM,N,0)
      
      smlest=one
      toosml=diagth*danger
      do 600 i=1,N
      eigenv=EVAL(i)
      if(eigenv.LT.toosml)goto 1100
      EVAL(i)=one/sqrt(eigenv)
      if(eigenv.LT.smlest)smlest=eigenv
600   continue
      do 700 i=1,N
      do 650 j=1,i
      sij=zero
      do 620 k=1,N
      sij=sij+EVAL(k)*BLK(i,k)*BLK(j,k)
620   continue
      S(j,i)=sij
650   continue
700   continue
      
      do 900 i=1,NBAS
      do 750 j=1,N
      EVAL(j)=zero
      do 720 k=1,j
      kp=LIST(k)
      tik=T(i,kp)
      if(tik.NE.zero)EVAL(j)=EVAL(j)+tik*S(k,j)
720   continue
      jp1=j+1
      do 740 k=jp1,N
      kp=LIST(k)
      tik=T(i,kp)
      if(tik.NE.zero)EVAL(j)=EVAL(j)+tik*S(j,k)
740   continue
750   continue
      do 800 j=1,N
      jp=LIST(j)
      T(i,jp)=EVAL(j)
800   continue
900   continue
      do 1000 i=1,NBAS
      im1=i-1
      do 950 j=1,im1
      S(j,i)=S(i,j)
950   continue
      S(i,i)=one
1000  continue
      return
      
1100  write(Lfnpr,99001)eigenv,toosml
      stop
      
99001 format(//1x,'An eigenvalue of the weighted PRE-NAO overlap',' matr
     &ix of ',f10.5,' has been',/,1x,'found, which is lower than',' the 
     &allowed threshold of ',f10.5,'.  This is',/,1x,'probably',' caused
     & by either an error in the data given to the analysis',' program',
     &/,1x,'or by numerical problems caused by near linear',' dependenci
     &es among the basis',/,1x,'functions.')
      end
C* :1 * 
      
