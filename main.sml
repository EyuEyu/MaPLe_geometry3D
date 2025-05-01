
val _ =
  let
    val (v, f) = read_triangle_mesh "./sphere.obj"
    
    val ns = MGL.per_face_normals v f
    val nv = MGL.per_vertex_normals v f
    val nv_atomic = MGL.per_vertex_normals_atomic v f
    val mass = MGL.mass v f
    val mass_atomic = MGL.mass_atomic v f
    val ce = MGL.cotmatrix_entries v f
    val cot = MGL.cotmatrix v f
  
    val s1 = Benchmark.run (fn _ => MGL.mass_atomic v f)

  in
    (*
    print(Geometry3D.Vector.toString (Seq.nth ns 0) ^ "\n");
    print(Geometry3D.Vector.toString (Seq.nth nv 0) ^ "\n");
    print(Real.toString (Seq.nth mass 0) ^ "\n");
    print(Real.toString (Seq.nth mass_atomic 0) ^ "\n");
    print(Geometry3D.Vector.toString (Seq.nth ce 0) ^ "\n")
    *)
    print(Geometry3D.Vector.toString (Seq.nth nv 0) ^ "\n");
    print(Geometry3D.Vector.toString (Seq.nth nv_atomic 0) ^ "\n")
    (* print(Real.toString test ^ "\n");
    print(Real.toString test2 ^ "\n");
    print(Real.toString (#2 (Seq.nth cea2 0)) ^ "\n") *)
  end
