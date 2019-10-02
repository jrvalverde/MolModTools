
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 rcoord"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "rcoord.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 29 "rcoord.web"
      subroutine rcoord(NATOMS,MULTIP,ICHARG,IAN,C,IOP,TOANG)
      implicit none
      integer*4 cnumb,end,eof,found,fp,i,IAN,iatom,ICHARG,In,int,intger,
     &IOP,Iout,Ipunch,isubst,len,MAXATM,MULTIP,NATOMS
      integer*4 nul,str,string
      parameter(MAXATM=100)
      double precision C(*),float,TOANG
      dimension IAN(*),IOP(50)
      common/io/In,Iout,Ipunch
      data fp/2HFP/,int/3HINT/,str/3HSTR/,end/3HEND/,nul/3HNUL/
      
      
99001 format(1x,'NATOMS OUTSIDE VALID RANGE IN RCOORD IN LINK 101, ','NA
     &TOMS = ',i5)
99002 format(1x,'INPUT ERROR IN RCOORD IN LINK 101')
99003 format(1x,'NO COORDINATE INPUT FOUND')
99004 format(1x,13x,'CHARGE =',i3,' MULTIPLICITY =',i3)
      
      
      call ffset(IOP(34))
      
      call ffread(eof)
      if(eof.NE.1)then
      call ffget(string,len,intger,float,found)
      if(found.EQ.int)ICHARG=intger
      if(found.EQ.nul)ICHARG=0
      if(found.EQ.int.OR.found.EQ.nul)then
      call ffget(string,len,intger,float,found)
      if(found.NE.int)goto 100
      MULTIP=intger
      write(Iout,99004)ICHARG,MULTIP
      
      iatom=0
      cnumb=0
20    call ffread(eof)
      if(eof.NE.1)then
      
      call ffget(string,len,intger,float,found)
      if(found.NE.end)then
      iatom=iatom+1
      if(iatom.GT.MAXATM)then
      write(Iout,99001)iatom
      call lnk1e
      endif
      if(found.EQ.int)IAN(iatom)=intger
      if(found.EQ.str)IAN(iatom)=isubst(string)
      if(found.NE.int.AND.found.NE.str)goto 100
      do 25 i=1,3
      call ffget(string,len,intger,float,found)
      if(found.NE.fp)goto 60
      cnumb=cnumb+1
      C(cnumb)=float
25    continue
      goto 20
      endif
      endif
      
      NATOMS=iatom
      
      if(mod(IOP(20),2).EQ.1)return
      do 40 i=1,cnumb
      C(i)=C(i)/TOANG
40    continue
      return
60    write(Iout,99002)
      call fferr(fp,found)
      endif
      
100   write(Iout,99002)
      call fferr(int,found)
      endif
      
      write(Iout,99003)
      call lnk1e
      call killer
      stop
      
      end
C* :1 * 
      
