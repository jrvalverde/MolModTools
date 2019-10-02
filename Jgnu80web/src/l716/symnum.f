
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 symnum"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "symnum.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "symnum.web"
      double precision function symnum(LINEAR)
      implicit none
      double precision f24,gfloat,one,twelve,two
      integer i,ifwg,iord,n,numer
      integer fwg(132),pg(3),num(10)
      logical LINEAR,test
      data ifwg/552/
      data one,two,twelve,f24/1.0D0,2.0D0,12.0D0,24.0D0/
      data num/1H0,1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9/
      
      
      
      
      
      
      
      call tread(ifwg,fwg,66,1,66,1,0)
      pg(1)=fwg(33)
      pg(2)=fwg(34)
      pg(3)=fwg(35)
      
      
      symnum=one
      LINEAR=pg(2).EQ.iord('*')
      n=numer(pg)
      test=.FALSE.
      do 100 i=1,10
      test=test.OR.pg(3).EQ.num(i)
100   continue
      if(.NOT.test)n=n/10
      
      
      if(pg(1).NE.iord('C'))then
      
      
      if(pg(1).EQ.iord('D'))then
      symnum=two*gfloat(n)
      if(LINEAR)symnum=two
      
      
      elseif(pg(1).NE.iord('S'))then
      
      
      if(pg(1).EQ.iord('T'))symnum=twelve
      if(pg(1).EQ.iord('O'))symnum=f24
      else
      symnum=gfloat(n)/two
      endif
      elseif(.NOT.(LINEAR.OR.pg(2).EQ.iord('I').OR.pg(2).EQ.iord('S')))t
     &hen
      symnum=gfloat(n)
      endif
      
      return
      
      end
C* :1 * 
      
