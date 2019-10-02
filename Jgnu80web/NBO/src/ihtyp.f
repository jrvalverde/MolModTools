
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ihtyp"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ihtyp.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 19 "ihtyp.web"
      function ihtyp(IBO,JBO)
      implicit none
      integer i,iat,Iathy,ib,IBO,Ibxm,ictr,ig,ihtyp,ir,iv,j,jat,jb,JBO,j
     &ctr,Label,Larc,Lstocc,MAXATM
      integer MAXBAS,Nbotyp,Nbouni
      logical bdfind
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbbas/Label(MAXBAS,6),Nbouni(MAXBAS),Nbotyp(MAXBAS),Lstocc(
     &MAXBAS),Ibxm(MAXBAS),Larc(MAXBAS),Iathy(MAXBAS,3)
      
      data iv,ig,ir/'v','g','r'/
      
      
      ihtyp=ir
      if(Nbouni(IBO).EQ.Nbouni(JBO))then
      ictr=mod(Nbotyp(IBO),10)
      ib=Ibxm(IBO)
      jctr=mod(Nbotyp(JBO),10)
      jb=Ibxm(JBO)
      do 50 i=1,ictr
      iat=Label(ib,i+3)
      do 20 j=1,jctr
      jat=Label(jb,j+3)
      if(iat.EQ.jat)then
      ihtyp=ig
      return
      elseif(bdfind(iat,jat))then
      ihtyp=iv
      endif
20    continue
50    continue
      endif
      return
      end
C* :1 * 
      
