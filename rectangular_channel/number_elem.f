      subroutine number_elem()
      
      Implicit Double precision (a-h,o-z)
      Dimension p(ntot,3),n(ntot,6)
      Dimension ptsx(500),ptsy(500),ptsz(500)
      Dimension Corner_points(500)
      Dimension Nodes_x_elem(500,6), Nodes_y_elem(500,6)
     +         ,Nodes_z_elem(500,6)
      Dimension Elem_conect(ne,6)
      Integer*4 :: num_lines,i,ntot,val,k
      Character(Len=1000) :: Line,iden
      common /xxx01/ ptsx,ptsy,ptsz,ntot
      common /xxx02/ Corner_points
      common /xxx03/ Nodes_x_elem,Nodes_y_elem,Nodes_z_elem,Elem_connect
      common /xxx04/ ne
      i = 1
      iden='$Elements'
      num_lines = 10000
      open (2,file="rectangular_surface.msh")
      do while(i<=num_lines)
          read(2,*) Line
          if(Line==iden)then
              exit
          end if
          i=i+1
      end do
      read(2,*) val,ntot,l1,l2
      read(2,*)
      write(*,*) 'The number of elements = ',ntot
      Return
      end