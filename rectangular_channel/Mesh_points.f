c-----------------------------------------------------------------------------------
c This subroutine takes in mesh file created in gmsh and calculates the points
c----------------------------------------------------------------------------------
      
      subroutine Mesh_points()
      
      Implicit Double Precision (a-h,o-z)

      Dimension p(ntot,3)
      Dimension ptsx(500),ptsy(500),ptsz(500)
      Dimension Corner_points(500)
      Dimension Nodes_x_elem(500,6), Nodes_y_elem(500,6)
     +         ,Nodes_z_elem(500,6)
      Dimension Elem_conect(ne,6)
      return
      end