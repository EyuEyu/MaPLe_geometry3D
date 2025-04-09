val _ =
  let
    val mesh = read_triangle_mesh "./sphere.obj"
    val v = #vertices mesh
    val f = #faces mesh
    
    val ns = Geometry3D.per_face_normals v f
    


  in
    print(Seq.length ns)
    
  end
