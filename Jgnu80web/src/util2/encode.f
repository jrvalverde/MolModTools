
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 encode"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "encode.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "encode.web"
      subroutine encode(LENGTH,FRAC,STRING,VALUE)
      implicit none
      integer ival,LENGTH,nchr,np,npi
      double precision VALUE
      double precision val,valx,sval,ten,chunk
      logical point
      integer FRAC,space,cursor,STRING(*)
      data ten/10.0D0/
      
      
      nchr(valx)=int(dlog10(valx)+1)
      point=.FALSE.
      cursor=0
      val=dabs(VALUE)+5.0D00*10.0D00**(-FRAC-2)
      
      np=1
      if(val.GT.9.0D00)np=nchr(val)
      space=LENGTH-1-FRAC-np
      if(VALUE.LT.0.)space=space-1
      call pad(STRING,cursor,space,' ')
      if(VALUE.LT.0.)call putchr('-',STRING,cursor)
      if(LENGTH.LE.FRAC+2)goto 200
      
100   npi=0
      if(val.GT.0.9)npi=nchr(val)
      if(npi.LT.np)call pad(STRING,cursor,cursor+np-npi,'0')
      if(npi.EQ.0)goto 300
      np=npi
      
200   if(np+cursor.GT.LENGTH)goto 400
      np=np-9
      if(np.LT.0)np=0
      chunk=0.2D00+ten**np
      sval=val/dint(chunk)
      ival=int(sval)
      if(ival.NE.0)then
      call decchr(ival,STRING,cursor)
      sval=0.2D0+ival*dint(chunk)
      val=val-dint(sval)
      goto 100
      endif
      
300   if(point.OR.cursor.EQ.LENGTH)return
      call putchr('.',STRING,cursor)
      point=.TRUE.
      np=LENGTH-cursor
      chunk=0.2D00+ten**np
      val=val*dint(chunk)
      goto 100
      
400   cursor=0
      call pad(STRING,cursor,LENGTH,'*')
      return
      
      end
C* :1 * 
      
