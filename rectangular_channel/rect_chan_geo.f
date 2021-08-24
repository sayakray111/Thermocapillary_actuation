      subroutine rect_chan_geo
     +
     +   (Ncl
     +   ,Iwall
     +   ,wall
     +   )
	 
      Implicit Double Precision (a-h,o-z)
      Dimension NE(nsg),Itp(nsg), xcntr(nsg), ycntr(nsg), Actis(nsg),
      Dimension RT(nsg)
      Dimension x2(200),y2(200),t2(200),s2(200)
      Dimension xm(200),ym(200),tm(200),sm(200)
      Dimension xg2(nsg,200),yg2(nsg,200),tg2(nsg,200),sg2(nsg,200)
      Dimension x0(500),y0(500),t0(500),s0(500)
      Dimension tnX0(nsg*200),tnY0(nsg*200)
      Dimension phi0(nsg,200),dphidn0(nsg,200)
      Dimension AL(nsg*200,nsg*200) BL(nsg*200) SOL(nsg*200)
      Dimension elml(nsg,200)
      Dimension actis(10),xcntr(10),ycntr(10)
      Dimension tnx0(500),tny0(500),vnx0(500),vny0(500)
	
c-------------------------------------
c common blocks 
c-------------------------------------

      common xxx01/ Iflow,nsg,ngl,NE,Itp
      common xxx02/ xg2,yg2,tg2,sg2,dphidn0,phi0
      common xxx03/ actis,xcntr,ycntr
	common xxx04/ tnx0,tny0,vnx0,vny0
	common xxx05/ Vx,Vy
	common xxx06/ xwmin,ywmin,xwmax,ywmax
	common xxx07/ x0,y0,t0,s0
	
	if(Iflow.eq.1)then
	  
          open(4,file='details_boundary.dat')
          read(4,*) NGL
          read(4,*) opening 
          read(4,*) depth
          read(4,*) NE(1),RT(1),first_pt1,last_pt1
          read(4,*) NE(2),RT(2)
          read(4,*) NE(3),RT(3)
          read(4,*) NE(4),RT(4)
          read(4,*) xwmin,ywmin
          
          call elm_line(NE(1),RT(1),first_pt1,0.0D0,last_pt1,0.0D0,0,x2
          + ,y2,s2,xm,ym,sm)
          
          do i=1,NE(1)+1
              xg2(1,i) = x2(i)
              yg2(1,i) = y2(i)
          end do
          
	  
	end if
	  
      Return
      end
	  