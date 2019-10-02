
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 prnpot"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "prnpot.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 29 "prnpot.web"
      subroutine prnpot(IAN,C,NVAL,COEF,EXPON,KF,KL,MAX,LSKP,ICOR,I,IFLA
     &G)
      implicit none
      double precision C,COEF,EXPON
      integer I,IAN,ICOR,IFLAG,In,ind,Iout,ipass,Ipunch,istart,istop,iva
     &len,j,jm1,k,KF,KL,lorb,LSKP,MAX
      integer mx1,negn,nterms,NVAL
      dimension IAN(*),C(*)
      dimension lorb(5)
      dimension NVAL(40),COEF(40),EXPON(40),KF(5),KL(5)
      common/io/In,Iout,Ipunch
      data lorb/1HS,1HP,1HD,1HF,1HG/
      
      
      
      
      
99001 format(2x,i3,8x,i3,82x,3F10.6)
99002 format(12x,'NO PSEUDOPOTENTIAL ON THIS CENTER')
99003 format(2x,i3,8x,i3,10x,i3,69x,3F10.6)
99004 format(1x,a1,'ANDUP',/,i4)
99005 format(1x,a1,'-',a1,/,i4)
99006 format(1x,i2,5x,f12.7,4x,f14.8)
99007 format(1x,i2,5x,f12.7,4x,f14.8)
99008 format(12x,'PSEUDOPOTENTIAL SAME AS ON CENTER',2x,i3)
      
      
      mx1=MAX+1
      if(LSKP.NE.1)then
      
      ivalen=IAN(I)-ICOR
      write(Iout,99003)I,IAN(I),ivalen,(C(3*I-3+j),j=1,3)
      if(IFLAG.NE.0)then
      write(Iout,99008)IFLAG
      return
      endif
      else
      write(Iout,99001)I,IAN(I),(C(3*I-3+j),j=1,3)
      write(Iout,99002)
      return
      endif
      
      ipass=0
      do 100 j=1,mx1
      nterms=KL(j)-KF(j)+1
      ipass=ipass+1
      if(ipass.LE.1)then
      write(Iout,99004)lorb(mx1),nterms
      else
      
      jm1=j-1
      write(Iout,99005)lorb(jm1),lorb(mx1),nterms
      endif
      ind=0
      istart=KF(j)
      istop=KL(j)
      do 50 k=istart,istop
      ind=ind+1
      negn=-(2-NVAL(k))
      if(negn*(-999).LT.1)negn=0
      negn=negn+2
      if(ind.LE.1)then
      write(Iout,99006)negn,EXPON(k),COEF(k)
      else
      
      write(Iout,99007)negn,EXPON(k),COEF(k)
      endif
50    continue
100   continue
      return
      
      end
C* :1 * 
      
