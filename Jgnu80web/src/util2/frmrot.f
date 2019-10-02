
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 frmrot"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "frmrot.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "frmrot.web"
      subroutine frmrot(RP,RD,RF,MAXTYP,IPURE)
      implicit none
      double precision d,f,f15,fact,fact1,five,four,gsqrt,half,one,pt5,r
     &1,r2,r3,r3ov2,r4,RD,RF,root15,root3
      double precision root5,RP,t,three,two,z1,z2,z3,zero
      integer i,id,ifd,ij,ip,IPURE,j,jd,jfd,k,kd,l,MAXTYP,mu,nu,num
      dimension RP(3,3),RD(6,6),RF(10,10)
      dimension id(6),jd(6)
      dimension kd(6,3),ifd(10),jfd(10)
      dimension d(5),f(7)
      data id/1,2,3,2,3,3/,jd/1,2,3,1,1,2/
      data kd/1,4,7,5,6,10,5,2,8,4,10,9,6,9,3,10,7,8/
      data ifd/1,2,3,2,4,5,3,3,6,6/
      data jfd/1,2,3,1,1,1,1,2,2,1/
      data one/1.0D0/,two/2.0D0/,three/3.0D0/,four/4.0D0/,five/5.0D0/
      data f15/15.0D0/,half/0.5D0/,pt5/0.5D0/
      data zero/0.0D0/
      
      
      
      
      
      
      
      root3=gsqrt(three)
      r3ov2=half*root3
      root5=gsqrt(five)
      root15=gsqrt(f15)
      r1=pt5*gsqrt(five/two)
      r2=three/(two*root5)
      r3=pt5*root3
      r4=pt5*gsqrt(three/two)
      z1=four/root5
      z2=one/root5
      z3=three/root5
      if(MAXTYP.GT.1)then
      
      do 50 k=1,6
      i=id(k)
      j=jd(k)
      
      do 20 l=1,6
      mu=id(l)
      nu=jd(l)
      fact=one
      fact1=one
      if(i.EQ.j.AND.mu.NE.nu)fact1=root3
      if(mu.EQ.nu.AND.i.NE.i)fact1=one/root3
      if(mu.EQ.nu)fact=zero
      
      RD(k,l)=RP(i,mu)*RP(j,nu)*fact1+fact*RP(i,nu)*RP(j,mu)*fact1
20    continue
50    continue
      if(MAXTYP.NE.2)then
      
      do 60 i=1,100
      RF(i,1)=zero
60    continue
      
      do 80 ij=1,10
      i=ifd(ij)
      j=jfd(ij)
      do 70 nu=1,3
      do 65 mu=1,6
      k=kd(mu,nu)
      RF(ij,k)=RF(ij,k)+RD(i,mu)*RP(j,nu)
65    continue
70    continue
80    continue
      endif
      
      if(IPURE.NE.1.AND.IPURE.NE.3)then
      
      
      num=6
100   do 120 i=1,num
      d(1)=RD(3,i)-pt5*(RD(1,i)+RD(2,i))
      d(2)=RD(5,i)
      d(3)=RD(6,i)
      d(4)=r3ov2*(RD(1,i)-RD(2,i))
      d(5)=RD(4,i)
      
      do 110 j=1,5
      RD(j,i)=d(j)
110   continue
120   continue
      
      do 140 i=1,5
      ip=i+1
      do 130 j=ip,6
      t=RD(i,j)
      RD(i,j)=RD(j,i)
      RD(j,i)=t
130   continue
140   continue
      
      if(num.NE.5)then
      num=5
      goto 100
      endif
      endif
      
      if(MAXTYP.NE.2)then
      if(IPURE.LT.2)then
      
      
      num=10
150   do 160 i=1,num
      
      f(1)=RF(3,i)-r2*(RF(6,i)+RF(9,i))
      f(2)=r4*(z1*RF(7,i)-RF(1,i)-z2*RF(4,i))
      f(3)=r4*(z1*RF(8,i)-RF(2,i)-z2*RF(5,i))
      f(4)=r3*(RF(6,i)-RF(9,i))
      f(5)=RF(10,i)
      f(6)=r1*(RF(1,i)-z3*RF(4,i))
      f(7)=r1*(z3*RF(5,i)-RF(2,i))
      
      do 155 j=1,7
      RF(j,i)=f(j)
155   continue
160   continue
      
      do 170 i=1,9
      ip=i+1
      do 165 j=ip,10
      t=RF(j,i)
      RF(j,i)=RF(i,j)
      RF(i,j)=t
165   continue
170   continue
      if(num.NE.7)then
      num=7
      goto 150
      endif
      endif
      endif
      endif
      
      return
      
      end
C* :1 * 
      
