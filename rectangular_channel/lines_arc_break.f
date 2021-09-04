c============================
c Author: S.Ray
c Micorfluidics Lab, IITKGP 2020
c This routine creates a boundary mesh for the advancing front method.
c============================    
      
          subroutine lines_arc_break(ptsx,ptsy)
         
          Implicit Double Precision (a-h,o-z)
          Dimension ptsx(500),ptsy(500)
          Integer*8 :: line_points(500,2),curve_points(500,2)
          Integer :: i,pl,p1,inc,flag
          real*8 :: slope1,slope2
 		
          error_margin = 0.0001D0
          nlinepts = 0
          ncurvepts = 0
          common xxx01/ ptsx,ptsy
          common xxx02/ line_points,curve_points
          common xxx03/ nlinepts,ncurvepts

          error_margin = 0.0001D0
c---------------------------------------
c Filling the matrices with zero values
c------------------------------------------
          do ll = 1,500
           line_points(ll,1) = 0.0D
           line_points(ll,2) = 0.0D
          end do
          do ll = 1,500
           curve_points(ll,1) = 0.0D0
           curve_points(ll,2) = 0.0D0
          end do
c------------------------------------------------
c Initializing some flags and values.
c-------------------------------------------------
          inc = 1
          p1 = 0
          pl = 1
          flag = 0
          i=1
c-------------------------------------------------
c First input the points 
c----------------------------------------------------
          call boundary_points()
c------------------------------------------------------------------
c Taking care of the first two corners
c---------------------------------------------------------------------
          slope1=(ptsx(1+1)-ptsx(1))
          slope2=(ptsx(1+2)-ptsx(1))
          if(abs(slope1-slope2).le.0.0D0)then
              i = 1
          else
              curve_points(1,1) = 1
              curve_points(1,2) = 2
              pl=pl+1
          end if
c----------------------------------------------------------------------------
c detecting the rest of the curved/straight segments------------------------
c---------------------------------------------------------------------------
          do while(i<=size(ptsx))
           slope1=(ptsx(i+1)-ptsx(i))
           do k = 2,size(ptsx)-i
            slope2=(ptsx(i+k)-ptsx(i))
            if(abs(slope1-slope2).Le.error_margin)then
             if(flag.eq.0)then
               p1=p1+1
             end if
             line_points(p1,1)=i
             line_points(p1,2)=i+k
             flag = 1
            else 
             curve_points(pl,1)=i+k-1
             curve_points(pl,2)=i+k
             pl=pl+1
             flag = 1
             exit
            end if
           end do
           inc = k-1
           i=i+inc
           flag = 0
          end do
c-----------------------------------------------------------------------
c Taking care of the last segment between the last two points 
c---------------------------------------------------------------
          am = size(ptsx)-1
          slope1=(ptsx(am+1)-ptsx(am))
          slope2=(ptsx(1)-ptsx(am))
          if(abs(slope1-slope2).le.0.0D0)then
              p1=p1+1
              line_points(p1,1) = am
              line_points(p1,2) = 1
          else
              curve_points(pl,1) = size(ptsx)
              curve_points(pl,2) = 1
              pl=pl+1
          end if
c----------------------------------------------------------------------------
c find out the number of non sharp gradients ----------------------
c---------------------------------------------------------------------------
          do jk = 1,size(line_points)
           if(line_points(jk,1).neq.0)then
            nlinepts+=1
           end if
          end do    
c----------------------------------------------------------------------------
c find out the number of sharp gradient segments ----------------------
c---------------------------------------------------------------------------    
          do jk = 1,size(curve_points)
           if(curve_points(jk,1).neq.0)then
            ncurvepts+=1
           end if
          end do
c-----------------------------------------------------------------------------------------------
c Cleaning the curved sections data and assigning zero elements to the common points
c---------------------------------------------------------------------------------------------		
          do h = 1,nlinepts
           do a = 1,ncurvepts
            if(line_points(h,1).eq.curve_points(a,1))then
             curve_points(a,1) = 0
              curve_points(a,2) = 0
            end if
           end do
          end do

          return 
          end