      program rect_chan

c============================
c Author: S.Ray
c Micorfluidics Lab, IITKGP 2020
c============================

c-------------------------------------
c Solving for a rectangular channel 
c------------------------------------- 

      Implicit Double Precision (a-h,o-z)
      Dimension NE(nsg),Itp(nsg), xcntr(nsg), ycntr(nsg), Actis(nsg), RT(nsg)
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
c constants 
c---------------------------------------

      surface_ten_ref = 0.0D0
      free_vel = 0.0D0
      height = 0.0D0
      coeff_visco = 0.0D0
      temp_grad = 0.0D0
      thermal_diff = 0.0D0
      drop_rad = 0.0D0
      beta = 0.0D0
      capillary_num = (coeff_visco*free_vel)/surface_ten_ref
      Marangoni_num = (beta*temp_grad*drop_rad)/(coeff_visco*free_vel)
      Peclet_num = (free_vel*drop_rad)/thermal_diff
      Reynolds_num = (density_fluid*free_vel*drop_rad)/coeff_visco

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
	common xxx08/ 

c-------------------------------------
c input
c-------------------------------------

  94  Continue

      write (6,*) 
      write (6,*) " Please enter:"
      write (6,*) 
      write (6,*) "  1 for a rectangular cavity(thick) on a plane wall "
      write (6,*) "  2 for a rectangular cavity(thin) on a plane wall"
      write (6,*) "  0 to quit"
      write (6,*) " ----------"

      read (5,*) Iflow
	  
	  if(Iflow.eq.0) go to 99
	  
	  if(Iflow.neq.1 and Iflow.ne.2)then
	     write (6,*) "Invalid option"
		 write (6,*) "Try again"
		 go to 94
	  end if
	  
  99  continue 
      Itry = 1
  98  continue
      
c-------------------------------------
c Creating the geometry
c-------------------------------------
    
	call rect_chan_geo(ncl,Iwall,wall)
	  


      Stop
      end

