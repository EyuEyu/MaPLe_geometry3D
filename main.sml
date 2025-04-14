

val _ =
  let
    val (v, f) = read_triangle_mesh "./sphere.obj"
    
    val ns = Geometry3D.per_face_normals v f
    val nv = Geometry3D.per_vertex_normals v f
    val mass = Geometry3D.mass v f
    val mass_atomic = Geometry3D.mass_atomic v f
    val mass_seq = Geometry3D.mass_seq v f  
  in
    print("Parallel mass\n");
    print(Real.toString (Seq.nth mass 0) ^ "\n ");
    print(Real.toString (Seq.nth mass 1) ^ "\n ");
    print(Real.toString (Seq.nth mass 2) ^ "\n ");
    print(Real.toString (Seq.nth mass 3) ^ "\n ");
    print(Real.toString (Seq.nth mass 4) ^ "\n ");
    print(Real.toString (Seq.nth mass 379) ^ "\n ");
    print(Real.toString (Seq.nth mass 380) ^ "\n ");
    print(Real.toString (Seq.nth mass 381) ^ "\n ");

    print("Parallel mass atomic\n");
    print(Real.toString (Seq.nth mass_atomic 0) ^ "\n ");
    print(Real.toString (Seq.nth mass_atomic 1) ^ "\n ");
    print(Real.toString (Seq.nth mass_atomic 2) ^ "\n ");
    print(Real.toString (Seq.nth mass_atomic 3) ^ "\n ");
    print(Real.toString (Seq.nth mass_atomic 4) ^ "\n ");
    print(Real.toString (Seq.nth mass_atomic 379) ^ "\n ");
    print(Real.toString (Seq.nth mass_atomic 380) ^ "\n ");
    print(Real.toString (Seq.nth mass_atomic 381) ^ "\n ");

    print("Sequential mass\n");
    print(Real.toString (Seq.nth mass_seq 0) ^ "\n ");
    print(Real.toString (Seq.nth mass_seq 1) ^ "\n ");
    print(Real.toString (Seq.nth mass_seq 2) ^ "\n ");
    print(Real.toString (Seq.nth mass_seq 3) ^ "\n ");
    print(Real.toString (Seq.nth mass_seq 4) ^ "\n ");
    print(Real.toString (Seq.nth mass_seq 379) ^ "\n ");
    print(Real.toString (Seq.nth mass_seq 380) ^ "\n ");
    print(Real.toString (Seq.nth mass_seq 381) ^ "\n ")
    
  end
