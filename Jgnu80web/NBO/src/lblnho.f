
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 lblnho"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "lblnho.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "lblnho.web"
      subroutine lblnho(INHO,INBO,ICTR,NCTR)
      implicit none
      integer i,i3,Iaolbl,Iatcr,Iatno,ib,iblnk,Ibxm,ic,icr,ICTR,ihyp,il,
     &ileft,ilp,INBO,INHO,Ino,ip,ir
      integer iright,istar,iy,Iznuc,Label,Larc,lbl,Lbl1,Ll,Lorb,Lorbc,Ls
     &tocc,Lu,MAXATM,MAXBAS,MAXD,nameat,Naolbl,Nbolbl,Nbotyp
      integer Nbouni,NCTR,nd,Nholbl,Nlew,Norbs,Nval
      parameter(MAXD=2)
      integer istr(MAXD),ibyte(4)
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbatom/Iatno(MAXATM),Ino(MAXATM),Norbs(MAXATM),Ll(MAXATM),L
     &u(MAXATM),Iznuc(MAXATM),Iatcr(MAXATM)
      common/nbbas/Label(MAXBAS,6),Nbouni(MAXBAS),Nbotyp(MAXBAS),Lstocc(
     &MAXBAS),Ibxm(MAXBAS),Larc(MAXBAS),Lbl1(MAXBAS),Lorbc(MAXBAS),Lorb(
     &MAXBAS)
      common/nblbl/Nlew,Nval,Iaolbl(10,MAXBAS),Naolbl(10,MAXBAS),Nholbl(
     &10,MAXBAS),Nbolbl(10,MAXBAS)
      
      data iblnk,ic,il,ip,ir,iy,i3,istar,ihyp/' ','c','l','p','r','y','3
     &','*','-'/
      data icr,ilp/'CR','LP'/
      data ileft,iright/'(',')'/
      
      do 100 i=1,10
      Nholbl(i,INHO)=iblnk
100   continue
      ib=Ibxm(INBO)
      
      
      if(NCTR.EQ.1)then
      lbl=nameat(Iatno(Label(ib,4)))
      call debyte(lbl,ibyte)
      Nholbl(1,INHO)=ibyte(1)
      Nholbl(2,INHO)=ibyte(2)
      call idigit(Label(ib,4),istr,nd,MAXD)
      if(nd.EQ.1)then
      Nholbl(4,INHO)=istr(1)
      else
      Nholbl(3,INHO)=istr(1)
      Nholbl(4,INHO)=istr(2)
      endif
      Nholbl(5,INHO)=ileft
      if(Label(ib,1).EQ.icr)then
      Nholbl(6,INHO)=ic
      Nholbl(7,INHO)=ir
      Nholbl(8,INHO)=iright
      elseif(Label(ib,1).EQ.ilp)then
      Nholbl(6,INHO)=il
      Nholbl(7,INHO)=ip
      if(Label(ib,2).EQ.istar)then
      Nholbl(8,INHO)=istar
      Nholbl(9,INHO)=iright
      else
      Nholbl(8,INHO)=iright
      endif
      else
      Nholbl(6,INHO)=ir
      Nholbl(7,INHO)=iy
      Nholbl(8,INHO)=istar
      Nholbl(9,INHO)=iright
      endif
      
      
      else
      lbl=nameat(Iatno(Label(ib,3+ICTR)))
      call debyte(lbl,ibyte)
      Nholbl(1,INHO)=ibyte(1)
      Nholbl(2,INHO)=ibyte(2)
      call idigit(Label(ib,3+ICTR),istr,nd,MAXD)
      if(nd.EQ.1)then
      Nholbl(4,INHO)=istr(1)
      else
      Nholbl(3,INHO)=istr(1)
      Nholbl(4,INHO)=istr(2)
      endif
      Nholbl(5,INHO)=ileft
      if(NCTR.EQ.2)then
      lbl=nameat(Iatno(Label(ib,6-ICTR)))
      call debyte(lbl,ibyte)
      Nholbl(6,INHO)=ibyte(1)
      Nholbl(7,INHO)=ibyte(2)
      call idigit(Label(ib,6-ICTR),istr,nd,MAXD)
      if(nd.EQ.1)then
      Nholbl(9,INHO)=istr(1)
      else
      Nholbl(8,INHO)=istr(1)
      Nholbl(9,INHO)=istr(2)
      endif
      Nholbl(10,INHO)=iright
      else
      Nholbl(6,INHO)=i3
      Nholbl(7,INHO)=ihyp
      Nholbl(8,INHO)=ic
      Nholbl(9,INHO)=iright
      endif
      endif
      return
      end
C* :1 * 
      
