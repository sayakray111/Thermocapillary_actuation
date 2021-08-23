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

