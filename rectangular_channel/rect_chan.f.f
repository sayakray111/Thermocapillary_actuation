c============================
c Author: S.Ray
c Micorfluidics Lab, IITKGP 2020
c============================

c-------------------------------------
c Solving for a rectangular channel 
c------------------------------------- 

Implicit Double Precision (a-h,o-z)
Dimension NE(nsg),Ncol(ncol)
Dimension x2(nsg,200),y2(nsg,200),t2(nsg,200)
Dimension xm(nsg,200),ym(nsg,200),tm(nsg,200)
Dimension x1(nsg,200),y1(nsg,200),t1(nsg,200)
Dimension x0(nsg*200),y0(nsg*200),t0(nsg*200)
Dimension phi(nsg,200),dphidn(nsg,200)
Dimension AL(nsg*200,nsg*200) BL(nsg*200) SOL(nsg*200)
Dimension elml(nsg,200)
Dimension Gauss(10)

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
capillary_num = (coeff_visco*free_vel)/surface_ten_ref
Marangoni_num = (beta*temp_grad*drop_rad)/(coeff_visco*free_vel)
Peclet_num = (free_vel*drop_rad)/thermal_diff
grashof_num = (g*density_fluid*




