
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 getc"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "getc.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "getc.web"
      subroutine getc(FIP,FIQ,HPQ,CC)
      implicit none
      double precision CC,F15,Fact,FIP,FIQ,Five,Four,H4p,H4q,Hepsi,Hpax,
     &Hpay,Hpaz,Hpbx,Hpby,Hpbz,HPQ,Hpqx,Hpqy,Hpqz
      double precision Hqcx,Hqcy,Hqcz,Hqdx,Hqdy,Hqdz,One,signu,term,term
     &ip,termiq,termrp,termrq,Three,Two,Xign,Zero
      integer ii,iimax,iip,ind,Indc,Indf,indfp,indfq,ip,ipmax,ipp,iq,iqm
     &ax,iqp,itemp,jtemp,la,Lamax,lap,lb
      integer Lbmax,lbp,lc,Lcmax,lcp,ld,Ldmax,ldp,Lpmax,Lpqmax,Lqmax
      integer rp,rpp,rpmax
      integer rq,rqp,rqmax
      integer u,up,umax
      dimension FIP(*),FIQ(*),HPQ(*),CC(*)
      common/factor/Fact(15)
      common/indf/Indf(16)
      common/indc/Indc(256)
      common/max/Lamax,Lbmax,Lcmax,Ldmax,Lpmax,Lqmax,Lpqmax
      common/h310/Hpax(4),Hpay(4),Hpaz(4),Hpbx(4),Hpby(4),Hpbz(4),Hqcx(4
     &),Hqcy(4),Hqcz(4),Hqdx(4),Hqdy(4),Hqdz(4),Hpqx(13),Hpqy(13),Hpqz(1
     &3),H4p(7),H4q(7),Hepsi(13)
      common/xign/Xign(13)
      common/con310/Zero,One,Two,Three,Four,Five,F15
      
      
      
      
      do 100 lap=1,Lamax
      la=lap-1
      
      do 50 lbp=1,Lbmax
      lb=lbp-1
      ipmax=la+lb+1
      indfp=Indf(4*la+lb+1)
      
      do 40 lcp=1,Lcmax
      lc=lcp-1
      
      do 20 ldp=1,Ldmax
      ld=ldp-1
      iimax=la+lb+lc+ld+1
      ind=Indc(64*la+16*lb+4*lc+ld+1)
      iqmax=lc+ld+1
      indfq=Indf(4*lc+ld+1)
      
      do 5 iip=1,iimax
      CC(iip-1+ind)=Zero
5     continue
      
      do 15 ipp=1,ipmax
      ip=ipp-1
      termip=FIP(indfp+ip)*Fact(ip+1)/H4p(ip+1)
      rpmax=(ip/2)+1
      
      do 12 rpp=1,rpmax
      rp=rpp-1
      termrp=termip*(H4p(rp+1)/(Fact(rp+1)*Fact(ip-2*rp+1)))
      
      do 10 iqp=1,iqmax
      iq=iqp-1
      termiq=termrp*(Xign(iq+1)*FIQ(indfq+iq)*Fact(iq+1)/H4q(iq+1))
      rqmax=(iq/2)+1
      
      do 8 rqp=1,rqmax
      rq=rqp-1
      itemp=ip+iq-2*(rp+rq)+1
      termrq=termiq*((H4q(rq+1)/(Fact(rq+1)*Fact(iq-2*rq+1)))*Fact(itemp
     &))
      umax=((ip+iq)/2-(rp+rq))+1
      
      do 6 up=1,umax
      u=up-1
      ii=ip+iq-2*(rp+rq)-u
      signu=Xign(u+1)
      
      itemp=ip+iq-2*(rp+rq+u)+1
      jtemp=ip+iq-2*(rp+rq)-u+1
      term=termrq*((signu*HPQ(itemp))/(Fact(u+1)*Fact(itemp)*Hepsi(jtemp
     &)))
      
      CC(ind+ii)=CC(ind+ii)+term
6     continue
8     continue
10    continue
12    continue
15    continue
      
      
20    continue
40    continue
50    continue
100   continue
      
      return
      
      end
C* :1 * 
      
