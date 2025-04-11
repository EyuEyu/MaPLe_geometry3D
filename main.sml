val _ =
  let
    val (v, f) = read_triangle_mesh "./sphere.obj"
    
    val ns = Geometry3D.per_face_normals v f
    val nv = Geometry3D.per_vertex_normals v f
    
  in
    print(Geometry3D.Vector.vectorToString (Seq.nth nv 0) ^ "\n ");
    print(Geometry3D.Vector.vectorToString (Seq.nth nv 1) ^ "\n ");
    print(Geometry3D.Vector.vectorToString (Seq.nth nv 381) ^ "\n ")
    
  end
