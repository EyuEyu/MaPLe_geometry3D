
val _ =
  let
    val (v, f) = read_triangle_mesh "./sphere.obj"
    
    val ns = MGL.per_face_normals v f
    val nv = MGL.per_vertex_normals v f
    val mass = MGL.mass v f
    val mass_atomic = MGL.mass_atomic v f
    val ce = MGL.cotmatrix_entries v f
    val cea = MGL.cot_triplet_array v f

    val test = MGL.test1 cea
    
  in
    (*
    print(Geometry3D.Vector.vectorToString (Seq.nth ns 0) ^ "\n");
    print(Geometry3D.Vector.vectorToString (Seq.nth nv 0) ^ "\n");
    print(Real.toString (Seq.nth mass 0) ^ "\n");
    print(Real.toString (Seq.nth mass_atomic 0) ^ "\n");
    print(Geometry3D.Vector.vectorToString (Seq.nth ce 0) ^ "\n")
    *)
    print(Real.toString test ^ "\n")
    
  end
