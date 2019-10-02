
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 sgeom"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "sgeom.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "sgeom.web"
      subroutine sgeom
      implicit none
      double precision abx,aby,abz,Acx,Acy,Acy2,Acz,Aqx,Aqz,Ax,Ay,Az,Bx,
     &By,Bz,cdx,cdy,cdz,Cosg,Cx
      double precision Cy,Cz,Dx,Dy,Dz,one,P11,P12,P13,p2,P21,P22,P23,P31
     &,p3131,P32,P33,p3333,pmq1,pmq2
      double precision pmq3,ppq1,ppq2,ppq3,pt0001,pt5,pt7,pt9,Q11,Q12,Q1
     &3,Q21,Q22,Q23,Q31,Q32,Q33,Qperp,Qperp2,Rab
      double precision Rabsq,Rcd,Rcdsq,Sing,sinp,temp,tenm12,zero
      integer Itype,Jnktyp,Jtype
      common/cgeom/Ax,Ay,Az,Bx,By,Bz,Cx,Cy,Cz,Dx,Dy,Dz,Rab,Rabsq,Rcd,Rcd
     &sq,P11,P12,P13,P21,P22,P23,P31,P32,P33,Q11,Q12,Q13,Q21,Q22,Q23,Q31
     &,Q32,Q33
      common/type/Itype,Jtype,Jnktyp(10)
      common/qgeom/Acx,Acy,Acz,Acy2,Cosg,Sing,Aqx,Aqz,Qperp,Qperp2
      data zero/0.0D0/,pt0001/1.0D-4/,one/1.0D0/
      data pt9/0.9D0/,pt5/0.5D0/,tenm12/1.0D-12/,pt7/0.7D0/
      
      
      
      
      
      
      abx=Bx-Ax
      aby=By-Ay
      abz=Bz-Az
      Rabsq=abx**2+aby**2+abz**2
      Rab=dsqrt(Rabsq)
      cdx=Dx-Cx
      cdy=Dy-Cy
      cdz=Dz-Cz
      Rcdsq=cdx**2+cdy**2+cdz**2
      Rcd=dsqrt(Rcdsq)
      if(Rab.NE.0)then
      P31=abx/Rab
      P32=aby/Rab
      P33=abz/Rab
      else
      
      P31=zero
      P32=zero
      P33=one
      endif
      if(Rcd.NE.0)then
      Q31=cdx/Rcd
      Q32=cdy/Rcd
      Q33=cdz/Rcd
      else
      
      Q31=zero
      Q32=zero
      Q33=one
      endif
      Cosg=P31*Q31+P32*Q32+P33*Q33
      Cosg=dmin1(one,Cosg)
      Cosg=dmax1(-one,Cosg)
      Sing=dsqrt(one-Cosg*Cosg)
      if(dabs(Cosg).LE.pt9)then
      P21=(P32*Q33-P33*Q32)/Sing
      P22=(P33*Q31-P31*Q33)/Sing
      P23=(P31*Q32-P32*Q31)/Sing
      else
      ppq1=P31+Q31
      ppq2=P32+Q32
      ppq3=P33+Q33
      pmq1=P31-Q31
      pmq2=P32-Q32
      pmq3=P33-Q33
      P21=pmq2*ppq3-ppq2*pmq3
      P22=pmq3*ppq1-ppq3*pmq1
      P23=pmq1*ppq2-ppq1*pmq2
      p2=dsqrt(P21*P21+P22*P22+P23*P23)
      Sing=pt5*p2
      if(Sing.GE.tenm12)then
      temp=one/p2
      P21=P21*temp
      P22=P22*temp
      P23=P23*temp
      
      elseif(dabs(P31).LE.pt7)then
      
      p3131=P31*P31
      p3131=dmin1(one,p3131)
      sinp=dsqrt(one-p3131)
      P21=zero
      P22=P33/sinp
      P23=-P32/sinp
      else
      
      p3333=P33*P33
      p3333=dmin1(one,p3333)
      sinp=dsqrt(one-p3333)
      P21=P32/sinp
      P22=-P31/sinp
      P23=zero
      endif
      endif
      Q21=P21
      Q22=P22
      Q23=P23
      P11=P22*P33-P23*P32
      P12=P23*P31-P21*P33
      P13=P21*P32-P22*P31
      Q11=Q22*Q33-Q23*Q32
      Q12=Q23*Q31-Q21*Q33
      Q13=Q21*Q32-Q22*Q31
      Acx=(Cx-Ax)*P11+(Cy-Ay)*P12+(Cz-Az)*P13
      Acy=(Cx-Ax)*P21+(Cy-Ay)*P22+(Cz-Az)*P23
      if(dabs(Acy).LE.pt0001)Acy=zero
      Acz=(Cx-Ax)*P31+(Cy-Ay)*P32+(Cz-Az)*P33
      Acy2=Acy*Acy
      return
      
      end
C* :1 * 
      
