
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 orthyb"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "orthyb.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "orthyb.web"
      subroutine orthyb(Q,S,TA,EVAL,C,IALARM,IFLG)
      implicit none
      double precision C,EVAL,one,Q,S,TA,temp,toosml,zero
      integer i,ia,IALARM,Iatcr,Iatno,IFLG,il,Ilu,Ino,Ispin,Iznuc,j,k,Lf
     &nao,Lfnarc,Lfndaf,Lfndef,Lfndm,Lfnin,Lfnmo
      integer Lfnnab,Lfnnao,Lfnnbo,Lfnnho,Lfnnlm,Lfnpna,Lfnpnb,Lfnpnh,Lf
     &npnl,Lfnppa,Lfnpr,Ll,MAXATM,MAXBAS,Munit,Mxao,Mxaolm,Mxbo,Natoms,N
     &bas
      integer Ndim,nh,Norbs
      
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),I
     &lu(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      dimension Q(Mxao,Ndim),S(Mxbo,Mxbo),TA(Mxao,Mxao),EVAL(Mxbo),C(Mxb
     &o,Mxbo)
      data zero,one/0.0D0,1.0D0/
      data toosml/1.0D-4/
      IALARM=0
      do 200 ia=1,Natoms
      il=Ll(ia)
      nh=Ino(ia)
      if(nh.GT.Mxao)goto 300
      if(nh.GT.1)then
      do 20 j=1,nh
      do 10 i=1,Mxao
      TA(i,j)=Q(i,il+j-1)
10    continue
20    continue
      do 40 j=1,nh
      do 30 i=j,nh
      temp=zero
      do 25 k=1,Mxao
      temp=temp+TA(k,i)*TA(k,j)
25    continue
      S(i,j)=temp
      S(j,i)=temp
30    continue
40    continue
      call jacobi(nh,S,EVAL,C,Mxbo,Mxbo,0)
      do 60 i=1,nh
      if(EVAL(i).LT.toosml)goto 300
      EVAL(i)=one/dsqrt(EVAL(i))
60    continue
      do 80 j=1,nh
      do 70 i=j,nh
      temp=zero
      do 65 k=1,nh
      temp=temp+EVAL(k)*C(i,k)*C(j,k)
65    continue
      S(i,j)=temp
      S(j,i)=temp
70    continue
80    continue
      do 100 j=1,nh
      do 90 i=1,Mxao
      temp=zero
      do 85 k=1,nh
      temp=temp+TA(i,k)*S(k,j)
85    continue
      C(i,j)=temp
90    continue
100   continue
      do 120 j=1,nh
      do 110 i=1,Mxao
      Q(i,il+j-1)=C(i,j)
110   continue
120   continue
      endif
200   continue
      return
      
      
300   IALARM=ia
      if(IFLG.EQ.0)write(Lfnpr,99001)EVAL(i),ia
      return
      
99001 format(4x,'Bad eigenvalue (',f15.7,') of S-NHO for atom ',i2)
      end
C* :1 * 
      
