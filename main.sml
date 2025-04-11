

val _ =
  let
    val (v, f) = read_triangle_mesh "./sphere.obj"
    
    val ns = Geometry3D.per_face_normals v f
    val nv = Geometry3D.per_vertex_normals v f
    val mass = Geometry3D.mass v f
    
  in
    print(Real.toString (Seq.nth mass 0) ^ "\n ");
    print(Real.toString (Seq.nth mass 1) ^ "\n ");
    print(Real.toString (Seq.nth mass 2) ^ "\n ");
    print(Real.toString (Seq.nth mass 3) ^ "\n ");
    print(Real.toString (Seq.nth mass 4) ^ "\n ");
    print(Real.toString (Seq.nth mass 379) ^ "\n ");
    print(Real.toString (Seq.nth mass 380) ^ "\n ");
    print(Real.toString (Seq.nth mass 381) ^ "\n ")
    
    
  end
