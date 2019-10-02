
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 annil"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "annil.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 28 "annil.web"
      subroutine annil(KOP,NAE,NBE,NBASIS,IRW,IPRINT,A,B,VEC,EIG)
      implicit none
      double precision A,B,EIG,factor,fmsq,fna,fnafnb,fnb,fnorm,four,gfl
     &oat,one,rd,rd2,s2aa,s2sd,s4sd,s6sd,six,sum
      double precision sz,szp1,szp2,ten,term1,term2,term3,term4,three,tn
     &e,trasq,trpq1,trpq2,trpq3,tt1,tt2,two,VEC,zero
      integer i,ibase,iflag,ij,ik,In,Iout,ipass,ipq,IPRINT,Ipunch,iq,IRW
     &,irwpa,irwpb,irwps,irwpt,irws,irwv1,irwv2
      integer itemp,j,jk,k,KOP,len,NAE,NBASIS,NBE,nsq,ntt
      double precision alpha,beta,total,spin
      dimension A(*),B(*),VEC(*),EIG(*),IRW(13)
      dimension irwv1(9),irwv2(9),factor(9),ipq(2)
      common/io/In,Iout,Ipunch
      data zero/0.0D0/,one/1.0D0/,two/2.0D0/,three/3.0D0/
      data four/4.0D0/,six/6.0D0/,ten/10.0D0/
      data ipq/6,1/
      data alpha/6HALPHA /,beta/6HBETA  /,total/6HTOTAL /,spin/6HSPIN  /
      data irws/514/,irwpa/528/,irwpb/530/,irwpt/532/,irwps/534/
      data irwv1/2,3,7,8,5,6,1,4,9/,irwv2/10,6,1,4,9,2,3,7,8/
      
      
      
      
      
      
      
      
      
      
      
      
      
      
99001 format(46H ANNIHILATION OF THE LARGEST SPIN CONTAMINANT,)
99002 format(26H S**2 BEFORE ANNIHILATION ,f10.4,10H,   AFTER ,f10.4)
99003 format(1x,10(1H<),a6,36H DENSITY MATRIX (AFTER ANNIHILATION),10(1H
     &>))
99004 format(' ',47x,10H BYPASSED.)
      
      nsq=NBASIS*NBASIS
      ntt=(NBASIS*(NBASIS+1))/2
      write(Iout,99001)
      len=13*nsq
      call rwfspc(iflag,len)
      if(KOP.NE.1)then
      if(iflag.NE.0)then
      write(Iout,99004)
      else
      
      fna=gfloat(NAE)
      fnb=gfloat(NBE)
      sz=(fna-fnb)/two
      call tread(irws,B,NBASIS,NBASIS,NBASIS,NBASIS,1)
      call tread(irwpa,A,NBASIS,NBASIS,NBASIS,NBASIS,1)
      call matrec(A,B,VEC,NBASIS,NBASIS,NBASIS,NBASIS,1)
      call twrite(IRW(1),A,nsq,1,nsq,1,0)
      call tread(irwpb,A,NBASIS,NBASIS,NBASIS,NBASIS,1)
      call matrec(A,B,VEC,NBASIS,NBASIS,NBASIS,NBASIS,1)
      call twrite(IRW(6),A,nsq,1,nsq,1,0)
      do 20 ipass=1,2
      itemp=ipq(ipass)
      call tread(IRW(itemp),A,nsq,1,nsq,1,0)
      ibase=iabs(ipass-2)+1
      ibase=(ibase-1)*5
      iq=ipass
      do 10 i=2,5
      iq=iabs(iq-2)+1
      itemp=ipq(iq)
      call tread(IRW(itemp),B,nsq,1,nsq,1,0)
      call matrec(A,B,VEC,NBASIS,NBASIS,NBASIS,NBASIS,1)
      call twrite(IRW(i+ibase),A,nsq,1,nsq,1,0)
10    continue
20    continue
      call tread(IRW(6),B,nsq,1,nsq,1,0)
      call matrec(A,B,VEC,NBASIS,NBASIS,NBASIS,NBASIS,1)
      
      trpq3=trasq(NBASIS,A)
      call tread(IRW(2),A,nsq,1,nsq,1,0)
      trpq1=trasq(NBASIS,A)
      call tread(IRW(9),A,nsq,1,nsq,1,0)
      trpq2=trasq(NBASIS,A)
      
      szp1=sz+one
      szp2=sz+two
      rd=sz*szp1+fnb
      rd2=rd*rd
      tne=fna+fnb
      tt1=trpq1*trpq1-trpq2
      tt2=two*rd+tne-two
      fnafnb=fna*fnb
      s2sd=rd-trpq1
      s4sd=rd2+fnafnb+two*tt1-tt2*trpq1
      term1=rd2*rd+rd*fnafnb+fnafnb*tt2
      term2=(three*rd2+three*rd*(tne-two)+(tne-two)**2+fnafnb+four*(fna-
     &one)*(fnb-one))*trpq1
      term3=two*(three*rd+three*tne-ten)*tt1
      term4=six*(trpq1**3-three*trpq1*trpq2+two*trpq3)
      s6sd=term1-term2+term3-term4
      term1=two*szp1*szp2
      term2=szp1*szp1*szp2*szp2
      s2aa=(s6sd-term1*s4sd+term2*s2sd)/(s4sd-term1*s2sd+term2)
      write(Iout,99002)s2sd,s2aa
      rd=fnb-two*szp1
      fmsq=rd*rd-two*rd*trpq1+fnafnb-tne*trpq1+two*(trpq1+tt1)
      fnorm=one/fmsq
      
      term1=fmsq-fnb+trpq1
      term2=fna-trpq1
      term3=tne-four*trpq1-three+two*rd
      term4=two*trpq1-fna+one-rd
      factor(1)=term4
      factor(2)=term3
      factor(3)=term4
      factor(4)=one
      factor(5)=four
      factor(6)=term2
      factor(7)=term1
      factor(8)=-two
      factor(9)=-two
      call paann(nsq,irwv1,IRW,IRW(11),A,B,factor,fnorm)
      
      term1=term1+fnb-fna
      term2=fnb-trpq1
      term4=term4+fna-fnb
      factor(1)=four
      factor(2)=term1
      factor(3)=term2
      factor(4)=-two
      factor(5)=-two
      factor(6)=term4
      factor(7)=one
      factor(8)=term4
      factor(9)=term3
      call paann(nsq,irwv2,IRW,IRW(12),A,B,factor,fnorm)
      
      call tread(irws,B,NBASIS,NBASIS,NBASIS,NBASIS,1)
      call diag(NBASIS,NBASIS,B,A,EIG,VEC)
      do 40 i=1,NBASIS
      EIG(i)=one/EIG(i)
40    continue
      ij=0
      do 60 j=1,NBASIS
      do 50 i=1,NBASIS
      ik=i
      jk=j
      ij=ij+1
      sum=zero
      do 45 k=1,NBASIS
      sum=sum+EIG(k)*A(ik)*A(jk)
      ik=ik+NBASIS
      jk=jk+NBASIS
45    continue
      B(ij)=sum
50    continue
60    continue
      
      call tread(IRW(11),A,nsq,1,nsq,1,0)
      call matrec(A,B,VEC,NBASIS,NBASIS,NBASIS,NBASIS,1)
      call twrite(IRW(13),A,NBASIS,NBASIS,NBASIS,NBASIS,1)
      
      call tread(IRW(12),A,nsq,1,nsq,1,0)
      call matrec(A,B,VEC,NBASIS,NBASIS,NBASIS,NBASIS,4)
      
      call tread(IRW(13),A,ntt,1,ntt,1,0)
      call linear(B,A(1+ntt),NBASIS,NBASIS)
      do 80 i=1,ntt
      B(i)=A(i)+A(i+ntt)
      B(i+ntt)=A(i)-A(i+ntt)
80    continue
      
      if(IPRINT.GE.2)then
      write(Iout,99003)alpha
      call ltoutd(NBASIS,A(1),1)
      write(Iout,99003)beta
      call ltoutd(NBASIS,A(1+ntt),1)
      write(Iout,99003)total
      call ltoutd(NBASIS,B(1),1)
      write(Iout,99003)spin
      call ltoutd(NBASIS,B(1+ntt),1)
      endif
      if(KOP.EQ.2)then
      call twrite(irwpa,A(1),ntt,1,ntt,1,0)
      call twrite(irwpb,A(1+ntt),ntt,1,ntt,1,0)
      call twrite(irwpt,B(1),ntt,1,ntt,1,0)
      call twrite(irwps,B(1+ntt),ntt,1,ntt,1,0)
      endif
      goto 100
      endif
      endif
      return
100   return
      
      end
C* :1 * 
      
