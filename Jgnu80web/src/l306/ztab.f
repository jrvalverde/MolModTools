
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ztab"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ztab.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 14 "ztab.web"
      subroutine ztab
      implicit none
      real*8 f10,f105,f11,f1155,f128,f13,f1365,f14,f15,f16,f165,f18,f20,
     &f21,f231,f256,f27,f273,f30,f3003
      real*8 f315,f33,f3465,f35,f385,f45,f512,f63,f64,f66,f693,f7,f70,f8
     &,f819,f9,f9009,five,four,Fpi
      integer Lf,Lmf,Lml,Lmx,Lmy,Lmz
      real*8 one,Pi,Pi3haf,Pi5hf2,Piqurt,six,Sqpi,Sqpi2,temp,three,two,T
     &wopi,Zlm
      
      
      common/ztabcm/Zlm(130),Lf(7),Lmf(49),Lml(49),Lmx(130),Lmy(130),Lmz
     &(130)
      common/pifac/Pi,Twopi,Fpi,Pi3haf,Pi5hf2,Piqurt,Sqpi,Sqpi2
      save one,two,three,four,five,six
      save f7,f8,f9,f10,f11,f13,f14,f15,f16,f18,f20,f21,f27
      save f30,f33,f35,f45,f63,f64,f66,f70,f105,f128,f165
      save f231,f256,f273,f315,f385,f512,f693,f819,f1155,f1365
      save f3003,f3465,f9009
      data one/1.0D0/,two/2.0D0/,three/3.0D0/,four/4.0D0/,five/5.0D0/,si
     &x/6.0D0/
      data f7/7.0D0/,f8/8.0D0/,f9/9.0D0/,f10/10.0D0/,f11/11.0D0/,f13/13.
     &0D0/f14/14.0D0/,f15/15.0D0/,f16/16.0D0/,f18/18.0D0/,f20/20.0D0/,f2
     &1/21.0D0/,f27/27.0D0/,f30/30.0D0/,f33/33.0D0/,f35/35.0D0/,f45/45.0
     &D0/,f63/63.0D0/,f64/64.0D0/,f66/66.0D0/,f70/70.0D0/,f105/105.0D0/,
     &f128/128.0D0/,f165/165.0D0/,f231/231.0D0/,f256/256.0D0/,f273/273.0
     &D0/,f315/315.0D0/,f385/385.0D0/,f512/512.0D0/,f693/693.0D0/,f819/8
     &19.0D0/,f1155/1155.0D0/,f1365/1365.0D0/,f3003/3003.0D0/,f3465/3465
     &.0D0/,f9009/9009.0D0/
      
      Zlm(1)=sqrt(one/Fpi)
      Zlm(2)=sqrt(three/Fpi)
      Zlm(3)=Zlm(2)
      Zlm(4)=Zlm(2)
      Zlm(5)=sqrt(f15/Fpi)/two
      Zlm(6)=-Zlm(5)
      Zlm(7)=two*Zlm(5)
      Zlm(8)=three*sqrt(five/Fpi)/two
      Zlm(9)=-Zlm(8)/three
      Zlm(10)=Zlm(7)
      Zlm(11)=Zlm(7)
      Zlm(12)=sqrt(f35/(f8*Fpi))
      Zlm(13)=-three*Zlm(12)
      Zlm(14)=sqrt(f105/(four*Fpi))
      Zlm(15)=-Zlm(14)
      Zlm(16)=five*sqrt(f21/(f8*Fpi))
      Zlm(17)=-Zlm(16)/five
      Zlm(18)=five*sqrt(f7/Fpi)/two
      Zlm(19)=-three*Zlm(18)/five
      Zlm(20)=Zlm(16)
      Zlm(21)=Zlm(17)
      Zlm(22)=two*Zlm(14)
      Zlm(23)=-Zlm(13)
      Zlm(24)=-Zlm(12)
      Zlm(25)=sqrt(f315/(f64*Fpi))
      Zlm(26)=-six*Zlm(25)
      Zlm(27)=Zlm(25)
      Zlm(28)=sqrt(f315/(f8*Fpi))
      Zlm(29)=-three*Zlm(28)
      temp=sqrt(f45/Fpi)/four
      Zlm(30)=f7*temp
      Zlm(31)=-Zlm(30)
      Zlm(32)=temp
      Zlm(33)=-temp
      temp=sqrt(f45/(f8*Fpi))
      Zlm(34)=f7*temp
      Zlm(35)=-three*temp
      temp=sqrt(f9/Fpi)/f8
      Zlm(36)=f35*temp
      Zlm(37)=-f30*temp
      Zlm(38)=three*temp
      Zlm(39)=Zlm(34)
      Zlm(40)=Zlm(35)
      temp=sqrt(f45/(four*Fpi))
      Zlm(41)=f7*temp
      Zlm(42)=-temp
      Zlm(43)=-Zlm(29)
      Zlm(44)=-Zlm(28)
      Zlm(45)=sqrt(f315/(four*Fpi))
      Zlm(46)=-Zlm(45)
      Zlm(47)=sqrt(f693/(f128*Fpi))
      Zlm(48)=-f10*Zlm(47)
      Zlm(49)=five*Zlm(47)
      Zlm(50)=sqrt(f3465/(f64*Fpi))
      Zlm(51)=-six*Zlm(50)
      Zlm(52)=Zlm(50)
      temp=sqrt(f385/(f128*Fpi))
      Zlm(53)=f9*temp
      Zlm(54)=-f27*temp
      Zlm(55)=three*temp
      Zlm(56)=-temp
      temp=sqrt(f1155/Fpi)/four
      Zlm(57)=three*temp
      Zlm(58)=-Zlm(57)
      Zlm(59)=-temp
      Zlm(60)=+temp
      temp=sqrt(f165/Fpi)/f8
      Zlm(61)=f21*temp
      Zlm(62)=-f14*temp
      Zlm(63)=temp
      temp=sqrt(f11/Fpi)/f8
      Zlm(64)=f63*temp
      Zlm(65)=-f70*temp
      Zlm(66)=f15*temp
      Zlm(67)=Zlm(61)
      Zlm(68)=Zlm(62)
      Zlm(69)=Zlm(63)
      temp=sqrt(f1155/Fpi)/two
      Zlm(70)=three*temp
      Zlm(71)=-temp
      Zlm(72)=-Zlm(54)
      Zlm(73)=-Zlm(55)
      Zlm(74)=-Zlm(53)
      Zlm(75)=-Zlm(56)
      Zlm(76)=sqrt(f3465/Fpi)/two
      Zlm(77)=-Zlm(76)
      Zlm(78)=Zlm(49)
      Zlm(79)=Zlm(48)
      Zlm(80)=Zlm(47)
      temp=sqrt(f3003/(f512*Fpi))
      Zlm(81)=six*temp
      Zlm(82)=-f20*temp
      Zlm(83)=Zlm(81)
      Zlm(84)=sqrt(f9009/(f128*Fpi))
      Zlm(85)=-f10*Zlm(84)
      Zlm(86)=five*Zlm(84)
      temp=sqrt(f819/(f256*Fpi))
      Zlm(87)=f11*temp
      Zlm(88)=-f66*temp
      Zlm(89)=Zlm(87)
      Zlm(90)=-temp
      Zlm(91)=six*temp
      Zlm(92)=-temp
      temp=sqrt(f1365/(f128*Fpi))
      Zlm(93)=f11*temp
      Zlm(94)=-f33*temp
      Zlm(95)=f9*temp
      Zlm(96)=-three*temp
      temp=sqrt(f1365/(f512*Fpi))
      Zlm(97)=f33*temp
      Zlm(98)=-Zlm(97)
      Zlm(99)=-f18*temp
      Zlm(100)=+f18*temp
      Zlm(101)=temp
      Zlm(102)=-temp
      temp=sqrt(f273/Fpi)/f8
      Zlm(103)=f33*temp
      Zlm(104)=-f30*temp
      Zlm(105)=five*temp
      temp=sqrt(f13/Fpi)/f16
      Zlm(106)=f231*temp
      Zlm(107)=-f315*temp
      Zlm(108)=f105*temp
      Zlm(109)=-five*temp
      Zlm(110)=Zlm(103)
      Zlm(111)=Zlm(104)
      Zlm(112)=Zlm(105)
      temp=sqrt(f1365/(f128*Fpi))
      Zlm(113)=f33*temp
      Zlm(114)=-f18*temp
      Zlm(115)=temp
      Zlm(116)=-Zlm(94)
      Zlm(117)=-Zlm(93)
      Zlm(118)=-Zlm(95)
      Zlm(119)=-Zlm(96)
      temp=sqrt(f819/Fpi)/four
      Zlm(120)=f11*temp
      Zlm(121)=-Zlm(120)
      Zlm(122)=-temp
      Zlm(123)=temp
      Zlm(124)=Zlm(86)
      Zlm(125)=Zlm(85)
      Zlm(126)=Zlm(84)
      Zlm(127)=sqrt(f3003/(f512*Fpi))
      Zlm(128)=-f15*Zlm(127)
      Zlm(129)=-Zlm(128)
      Zlm(130)=-Zlm(127)
      return
      end
C* :1 * 
      
