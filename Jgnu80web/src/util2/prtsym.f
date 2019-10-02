
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 prtsym"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "prtsym.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "prtsym.web"
      subroutine prtsym(ORBSYM,NE,NB,IOUT)
      implicit none
      integer blank,i,IOUT,lcur,len,low,lprin,n,NB,NE,rprin,tcur,top
      integer ORBSYM(NB)
      integer buf(80),tmp(8)
      integer bracks
      data rprin/')'/,lprin/'('/,blank/1H /
      data bracks/') ('/
      
      
      lcur=0
      low=1
      top=NE
      n=0
      tcur=0
      call pad(buf,tcur,80,blank)
      call putbc('      OCCUPIED: ',16,buf,n)
      call puticr(lprin,buf,n)
      
100   do 200 i=low,top
      call getb(2,tmp,len,ORBSYM,lcur)
      call putb(tmp,len,buf,n)
      if(n.LT.60)then
      
      if(i.NE.top)call pakstr(bracks,3,buf,n)
      else
      call puticr(rprin,buf,n)
      call strout(IOUT,buf,n,1)
      tcur=0
      call pad(buf,tcur,80,blank)
      n=16
      call puticr(lprin,buf,n)
      endif
200   continue
      
      call puticr(rprin,buf,n)
      if(n.GT.18)call strout(IOUT,buf,n,1)
      
      if(top.EQ.NB)return
      low=NE+1
      top=NB
      n=0
      tcur=0
      call pad(buf,tcur,80,blank)
      call putbc('      VIRTUAL:  ',16,buf,n)
      call puticr(lprin,buf,n)
      goto 100
      
      end
C* :1 * 
      
