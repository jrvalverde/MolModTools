
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ldata"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ldata.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "ldata.web"
      subroutine ldata
      implicit none
      real*8 Dfac,Fac,four,Fpi,Fprod,gatan,one,Pi,Pi3haf,Pi5hf2,Piqurt,S
     &qpi,Sqpi2,two,Twopi,Zlm
      integer i,Indjx,indjxd,Indjy,indjyd,Indjz,indjzd,Indlp,indlpd,k,k1
     &,l1,Lf,lfd,Lmf,lmfd,Lml,lmld,Lmx,lmxd
      integer Lmy,lmyd,Lmz,lmzd
      
      
      common/dfac/Dfac(23)
      common/fact/Fac(13),Fprod(7,7)
      common/pifac/Pi,Twopi,Fpi,Pi3haf,Pi5hf2,Piqurt,Sqpi,Sqpi2
      common/ztabcm/Zlm(130),Lf(7),Lmf(49),Lml(49),Lmx(130),Lmy(130),Lmz
     &(130)
      common/ndex/Indjx(35),Indjy(35),Indjz(35),Indlp(20)
      dimension lfd(7),lmfd(49),lmld(49),lmxd(130),lmyd(130),lmzd(130),i
     &ndjxd(35),indjyd(35),indjzd(35),indlpd(20)
      save one,two,four,lfd,lmfd,lmld,lmxd,lmyd,lmzd
      save indjxd,indjyd,indjzd,indlpd
      data one/1.0D0/,two/2.0D0/,four/4.0D0/
      data indjxd/1,2,1,1,3,1,1,2,2,1,4,1,1,2,3,3,2,1,1,2,5,1,1,4,2,4,2,
     &1,1,3,3,1,3,2,3/
      data indjyd/1,1,2,1,1,3,1,2,1,2,1,4,1,3,2,1,1,2,3,2,1,5,1,2,4,1,1,
     &4,2,3,1,3,2,3,3/
      data indjzd/1,1,1,2,1,1,3,1,2,2,1,1,4,1,1,2,3,3,2,2,1,1,5,1,1,2,4,
     &2,4,1,3,3,2,2,2/
      data indlpd/1,4,3,2,7,10,6,9,8,5,13,18,19,12,17,20,14,16,15,11/
      data lfd/1,2,5,10,17,26,37/
      data lmfd/1,2,3,4,5,7,8,10,11,12,14,16,18,20,22,23,25,28,30,34,36,
     &39,41,43,45,47,50,53,57,61,64,67,70,72,76,78,81,84,87,93,97,103,10
     &6,110,113,116,120,124,127/
      data lmld/1,2,3,4,6,7,9,10,11,13,15,17,19,21,22,24,27,29,33,35,38,
     &40,42,44,46,49,52,56,60,63,66,69,71,75,77,80,83,86,92,96,102,105,1
     &09,112,115,119,123,126,130/
      data lmxd/0,1,0,0,2,0,1,0,0,0,1,3,1,2,0,1,1,0,0,0,0,1,2,0,4,2,0,3,
     &1,2,0,0,2,1,1,0,0,0,0,0,1,1,2,0,3,1,5,3,1,0,2,4,3,1,1,3,2,0,2,0,1,
     &1,1,0,0,0,0,0,0,1,1,2,2,0,0,3,1,4,2,0,5,3,1,5,3,1,4,2,0,4,2,0,3,1,
     &1,3,2,0,2,0,2,0,1,1,1,0,0,0,0,0,0,0,1,1,1,2,0,2,0,3,1,3,1,4,2,0,6,
     &4,2,0/
      data lmyd/0,0,0,1,0,2,0,0,0,1,1,0,2,0,2,0,0,0,0,1,1,1,1,3,0,2,4,0,
     &2,0,2,2,0,0,0,0,0,0,1,1,1,1,1,3,1,3,0,2,4,4,2,0,0,2,2,0,0,2,0,2,0,
     &0,0,0,0,0,1,1,1,1,1,1,1,3,3,1,3,1,3,5,1,3,5,0,2,4,0,2,4,0,2,4,0,2,
     &2,0,0,2,0,2,0,2,0,0,0,0,0,0,0,1,1,1,1,1,1,1,3,1,3,1,3,1,3,1,3,5,0,
     &2,4,6/
      data lmzd/0,0,1,0,0,0,1,2,0,1,0,0,0,1,1,2,0,3,1,2,0,1,0,0,0,0,0,1,
     &1,2,2,0,0,3,1,4,2,0,3,1,2,0,1,1,0,0,0,0,0,1,1,1,2,2,0,0,3,3,1,1,4,
     &2,0,5,3,1,4,2,0,3,1,2,0,2,0,1,1,0,0,0,0,0,0,1,1,1,2,2,2,0,0,0,3,3,
     &1,1,4,4,2,2,0,0,5,3,1,6,4,2,0,5,3,1,4,2,0,3,3,1,1,2,2,0,0,1,1,1,0,
     &0,0,0/
      
      Pi=four*gatan(one)
      Twopi=two*Pi
      Fpi=four*Pi
      Sqpi=sqrt(Pi)
      Sqpi2=sqrt(Pi/two)
      Pi3haf=Pi*sqrt(Pi)
      Pi5hf2=Twopi*Pi3haf
      Piqurt=Twopi/sqrt(Pi5hf2)
      Dfac(1)=one
      Dfac(2)=one
      Dfac(3)=one
      do 100 i=4,23
      Dfac(i)=Dfac(i-2)*float(i-2)
100   continue
      Fac(1)=one
      do 200 i=1,12
      Fac(i+1)=Fac(i)*float(i)
200   continue
      do 300 l1=1,7
      do 250 k1=1,l1
      k=k1-1
      Fprod(k1,l1)=Fac(l1+k)/(Fac(k1)*Fac(l1-k1+1))
250   continue
300   continue
      call imove(7,lfd,Lf)
      call imove(49,lmfd,Lmf)
      call imove(49,lmld,Lml)
      call imove(130,lmxd,Lmx)
      call imove(130,lmyd,Lmy)
      call imove(130,lmzd,Lmz)
      call imove(35,indjxd,Indjx)
      call imove(35,indjyd,Indjy)
      call imove(35,indjzd,Indjz)
      call imove(20,indlpd,Indlp)
      return
      end
C* :1 * 
      
