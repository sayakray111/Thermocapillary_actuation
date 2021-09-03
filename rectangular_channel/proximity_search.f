      subroutine proximity_search(xi,yi,h,number_points)
      
      Implicit Double Precision (a-h,o-z)
      Dimension prptsx(500),prptsy(500)
      Dimension xg(nsg*200,2),yg(nsg*200,2)
      common /xxx01/ prptsx,prptsy
      common /xxx02/ xg,yg
      common /xxx03/ first_edge,last_edge
      alpha = 0.1
      radius = alpha*h
      do i = 1,number_points
          do j = 1,2
              
          end do
      end do
      Return
      end