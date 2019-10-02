
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 intprt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "intprt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "intprt.web"
      subroutine intprt(KOP,IBUF,DBUF)
      implicit none
      double precision DBUF,Valint,vhold,whold,xhold
      integer i,Ia,Ib,Ibasd,Ibase,Ibfpad,IBUF,Icon,Ifil,igo,ih,ihold,ij,
     &iline,In,Intcnt,Iout,Ipunch,Iq,Ireset
      integer Ismode,Istat,Itotal,Iux,Ja,Jb,jhold,jq,Kb,khold,kl,Kntt1,K
     &ntt2,KOP,Last,Lb,lhold,Limint,lq,m
      integer Mindx,Mode,nbuf,Nrpext,Ntx,Nwiib,Nwpi
      integer P,Q,R,Sindx
      integer Dbase,dcount,Dbasd
      dimension Ia(2),ihold(4),jhold(4),khold(4),lhold(4)
      dimension vhold(4),whold(4),xhold(4)
      dimension IBUF(*),DBUF(*)
      common/packed/Ib,Jb,Kb,Lb,Valint,Ja
      common/ibf/Ismode,Mode,Istat,Last,Ntx,Iux(5),Icon,Nrpext,Kntt1,Knt
     &t2,Ibase,Ibasd(2),Dbase,Dbasd(2),Ireset(2),Iq,Ifil,Intcnt,Itotal,L
     &imint,Nwpi,Nwiib,Ibfpad
      common/io/In,Iout,Ipunch
      equivalence(Valint,Ia(1)),(P,Ib),(Q,Jb),(R,Kb,Mindx),(Sindx,Lb)
      data nbuf/0/
      
      
      
      
      
      
      
      
      
      
99001 format(/1x,'REGULAR INTEGRALS IN STANDARD PRINT MODE:')
99002 format(/1x,'REGULAR INTEGRALS IN DEBUG PRINT MODE:')
99003 format(/1x,'RAFFENETTI TYPE 1 INTEGRALS')
99004 format(/1x,'RAFFENETTI TYPE 2 INTEGRALS')
99005 format(/1x,'RAFFENETTI TYPE 3 INTEGRALS')
99006 format('+','                            IN STANDARD PRINT MODE:')
99007 format('+','                            IN DEBUG PRINT MODE:')
99008 format(/1x,'BUFFER NUMBER ',i5/)
99009 format(1x,4('(',3(i3,','),i3,') ',d13.7,1x),' #',i3)
99010 format(1x,4('(',i7,',',i7,') ',d13.7,1x),' #',i3)
99011 format(1x,4(18x,d13.7,1x))
      
      
      if(nbuf.EQ.0)then
      if(Mode.EQ.2)then
      
      write(Iout,99003)
      elseif(Mode.EQ.3)then
      
      write(Iout,99004)
      elseif(Mode.EQ.4)then
      
      write(Iout,99005)
      else
      
      if(KOP.EQ.1)write(Iout,99001)
      if(KOP.EQ.2)write(Iout,99002)
      goto 100
      endif
      
      if(KOP.EQ.1)write(Iout,99006)
      if(KOP.EQ.2)write(Iout,99007)
      endif
      
      
100   nbuf=nbuf+1
      write(Iout,99008)nbuf
      ih=1
      iline=1
      
      
      if(Mode.EQ.2)then
      
      
      igo=1
      m=0
      elseif(Mode.EQ.3)then
      
      
      igo=2
      m=0
      dcount=1
      goto 300
      elseif(Mode.EQ.4)then
      
      
      igo=3
      m=0
      dcount=1
      goto 400
      else
      
      
      
      if(Kntt1.GT.0)then
      jq=Ireset(1)+Ibase
      lq=jq+(Kntt1-1)*Nwpi
      do 120 m=jq,lq,Nwpi
      Ja=IBUF(m)
      Ia(1)=IBUF(m+1)
      Ia(2)=IBUF(m+2)
      call unpck4
      ihold(ih)=Ib
      jhold(ih)=Jb
      khold(ih)=Kb
      lhold(ih)=Lb
      vhold(ih)=Valint
      ih=ih+1
      if(ih.GT.4)then
      write(Iout,99009)(ihold(i),jhold(i),khold(i),lhold(i),vhold(i),i=1
     &,4),iline
      ih=1
      iline=iline+1
      endif
120   continue
      endif
      
      
      if(Kntt2.GT.0)then
      lq=Ireset(2)+Ibase
      jq=lq-(Kntt2-1)*Nwpi
      do 140 m=jq,lq,Nwpi
      Ja=IBUF(m)
      Ia(1)=IBUF(m+1)
      Ia(2)=IBUF(m+2)
      call unpck4
      vhold(ih)=Valint
      if(KOP.EQ.1)then
      
      Sindx=Sindx+1
      if(Sindx.EQ.2)then
      
      ihold(ih)=P
      jhold(ih)=Q
      khold(ih)=Q
      lhold(ih)=R
      elseif(Sindx.EQ.3)then
      
      ihold(ih)=P
      jhold(ih)=Q
      khold(ih)=R
      lhold(ih)=Q
      elseif(Sindx.EQ.4)then
      
      ihold(ih)=P
      jhold(ih)=Q
      khold(ih)=P
      lhold(ih)=Q
      elseif(Sindx.EQ.5)then
      
      ihold(ih)=P
      jhold(ih)=P
      khold(ih)=Q
      lhold(ih)=R
      elseif(Sindx.EQ.6)then
      
      ihold(ih)=P
      jhold(ih)=Q
      khold(ih)=R
      lhold(ih)=R
      elseif(Sindx.EQ.7)then
      
      if(Mindx.EQ.2)then
      
      ihold(ih)=P
      jhold(ih)=P
      khold(ih)=Q
      lhold(ih)=Q
      elseif(Mindx.EQ.3)then
      
      ihold(ih)=P
      jhold(ih)=P
      khold(ih)=P
      lhold(ih)=Q
      else
      
      ihold(ih)=P
      jhold(ih)=Q
      khold(ih)=Q
      lhold(ih)=Q
      endif
      elseif(Sindx.EQ.8)then
      
      ihold(ih)=P
      jhold(ih)=P
      khold(ih)=P
      lhold(ih)=P
      else
      
      ihold(ih)=P
      jhold(ih)=Q
      khold(ih)=P
      lhold(ih)=R
      endif
      else
      ihold(ih)=Ib
      jhold(ih)=Jb
      khold(ih)=Kb
      lhold(ih)=Lb
      Sindx=Sindx+1
      if(Sindx.EQ.1.OR.Sindx.EQ.2.OR.Sindx.EQ.3.OR.Sindx.EQ.5.OR.Sindx.E
     &Q.6.OR.Sindx.EQ.7)then
      elseif(Sindx.EQ.8)then
      
      jhold(ih)=0
      khold(ih)=0
      else
      
      khold(ih)=0
      endif
      endif
      
      ih=ih+1
      if(ih.GT.4)then
      write(Iout,99009)(ihold(i),jhold(i),khold(i),lhold(i),vhold(i),i=1
     &,4),iline
      ih=1
      iline=iline+1
      endif
140   continue
      endif
      
      
      ih=ih-1
      if(ih.EQ.0)return
      write(Iout,99009)(ihold(i),jhold(i),khold(i),lhold(i),vhold(i),i=1
     &,ih)
      return
      endif
200   m=m+1
      if(m.GT.Kntt1)goto 600
      Ja=IBUF(m+Ibase)
      call unpck2
      ihold(ih)=Ib
      jhold(ih)=Jb
      vhold(ih)=DBUF(m+Dbase)
      goto 500
300   m=m+1
      if(m.GT.Kntt1)goto 600
      Ja=IBUF(m+Ibase)
      call unpck2
      ihold(ih)=Ib
      jhold(ih)=Jb
      vhold(ih)=DBUF(dcount+Dbase)
      whold(ih)=DBUF(dcount+Dbase+1)
      dcount=dcount+Ismode
      goto 500
400   m=m+1
      if(m.GT.Kntt1)goto 600
      Ja=IBUF(m+Ibase)
      call unpck2
      ihold(ih)=Ib
      jhold(ih)=Jb
      vhold(ih)=DBUF(dcount+Dbase)
      whold(ih)=DBUF(dcount+Dbase+1)
      xhold(ih)=DBUF(dcount+Dbase+2)
      dcount=dcount+Ismode
      
500   if(KOP.NE.1)then
      ij=ihold(ih)
      kl=jhold(ih)
      call getij(ij,ihold(ih),jhold(ih))
      call getij(kl,khold(ih),lhold(ih))
      endif
      ih=ih+1
      if(ih.GT.4)then
      if(KOP.EQ.1)write(Iout,99010)(ihold(i),jhold(i),vhold(i),i=1,4),il
     &ine
      if(KOP.EQ.2)write(Iout,99009)(ihold(i),jhold(i),khold(i),lhold(i),
     &vhold(i),i=1,4),iline
      if(Ismode.GE.2)write(Iout,99011)(whold(i),i=1,4)
      if(Ismode.EQ.3)write(Iout,99011)(xhold(i),i=1,4)
      ih=1
      iline=iline+1
      endif
      if(igo.EQ.1)goto 200
      if(igo.EQ.2)goto 300
      if(igo.EQ.3)goto 400
      
      
600   ih=ih-1
      if(ih.EQ.0)return
      if(KOP.EQ.1)write(Iout,99010)(ihold(i),jhold(i),vhold(i),i=1,ih)
      if(KOP.EQ.2)write(Iout,99009)(ihold(i),jhold(i),khold(i),lhold(i),
     &vhold(i),i=1,ih)
      if(Ismode.GE.2)write(Iout,99011)(whold(i),i=1,ih)
      if(Ismode.EQ.3)write(Iout,99011)(xhold(i),i=1,ih)
      return
      
      end
C* :1 * 
      
