
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 qpasy"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "qpasy.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "qpasy.web"
      function qpasy(N,LA1,LB1,ALP,XKA1,XKB1,IFLAG)
      implicit none
      real*8 ALP,Alpha,coe,cutoff,Dfac,four,one,prefac,Q,qcomp,qnew,qold
     &1,qold2,qpasy,Rk,sum,T,term,tk,two
      real*8 xka,XKA1,xkb,XKB1
      integer IFLAG,j,la,LA1,lb,LB1,N,nprime
      
      common/qstore/Q(9,7),Alpha,Rk,T
      common/dfac/Dfac(23)
      save one,two,four,cutoff
      data one/1.0D0/,two/2.0D0/,four/4.0D0/,cutoff/1.0D-13/
      
      if(IFLAG.EQ.3)then
      xka=XKB1
      xkb=XKA1
      la=LB1
      lb=LA1
      else
      xka=XKA1
      xkb=XKB1
      la=LA1
      lb=LB1
      endif
      Alpha=one
      Rk=xkb/sqrt(ALP)
      T=Rk*Rk/four
      
      tk=xka*xka/(two*ALP)
      qold1=qcomp(N+la,lb)
      sum=qold1/Dfac(la+la+3)
      if(tk.NE.0)then
      nprime=N+la+2
      qnew=qcomp(nprime,lb)
      term=qnew*tk/Dfac(la+la+5)
      sum=sum+term
      j=2
      nprime=N+la+j+j
      coe=one/Dfac(la+la+3)
50    qold2=qold1*coe
      qold1=qnew*coe
      qnew=(T+float(nprime+nprime-5)/two)*qold1+float((lb-nprime+4)*(lb+
     &nprime-3))*qold2/four
      term=qnew*tk*tk/(float((j-1)*(la+la+j+j-1))*float(j*(la+la+j+j+1))
     &)
      sum=sum+term
      if(abs(term/sum).GT.cutoff)then
      j=j+1
      nprime=N+la+j+j
      coe=tk/float((j-2)*(la+la+j+j-3))
      goto 50
      endif
      endif
      if(la.EQ.0)prefac=one/sqrt(ALP**(N+la+1))
      if(la.NE.0)prefac=(xka**la)/sqrt(ALP**(N+la+1))
      qpasy=prefac*sum
      return
      end
C* :1 * 
      
