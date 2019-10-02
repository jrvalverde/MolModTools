
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 idigit"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "idigit.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "idigit.web"
      subroutine idigit(KINT,IK,ND,MAXD)
      implicit none
      integer iblnk,id,IK,jint,KINT,MAXD,ND
      dimension IK(MAXD)
      data iblnk/' '/
      
      
      jint=KINT
      ND=MAXD
      do 100 id=MAXD,1,-1
      IK(id)=mod(jint,10)+48
      if(IK(id).GT.48)ND=id
      jint=jint/10
100   continue
      ND=MAXD-ND+1
      
      
      do 200 id=1,ND
      IK(id)=IK(id+MAXD-ND)
200   continue
      do 300 id=ND+1,MAXD
      IK(id)=iblnk
300   continue
      return
      end
C* :1 * 
      
