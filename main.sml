structure M = MatCOO(structure I = Int
                     structure R = Real)

val _ =
  let
    val (v, f) = read_triangle_mesh "./data/human.obj"
    
    (*
    val ns = MGL.per_face_normals v f
    val nv = MGL.per_vertex_normals v f
    val nv_atomic = MGL.per_vertex_normals_atomic v f
    val mass = MGL.mass v f
    val mass_atomic = MGL.mass_atomic v f
    val ce = MGL.cotmatrix_entries v f
    val cot = MGL.cotmatrix v f
    val iter = MGL.iteration_step v f 0.001
    *)

    val (cotmat, weight) = MGL.iteration_preps v f 
    val _ = Benchmark.run (fn _ => MGL.iteration_step v cotmat weight) 
    

  in
    (*
    print(Geometry3D.Vector.toString (Seq.nth ns 0) ^ "\n");
    print(Geometry3D.Vector.toString (Seq.nth nv 0) ^ "\n");
    print(Real.toString (Seq.nth mass 0) ^ "\n");
    print(Real.toString (Seq.nth mass_atomic 0) ^ "\n");
    print(Geometry3D.Vector.toString (Seq.nth ce 0) ^ "\n");
    print(Geometry3D.Vector.toString (Seq.nth nv 0) ^ "\n");
    print(Geometry3D.Vector.toString (Seq.nth nv_atomic 0) ^ "\n")
    
    print(Geometry3D.Vector.toString (Seq.nth nv 77) ^ "\n");
    print(Geometry3D.Vector.toString (Seq.nth nv 78) ^ "\n");
    print(Geometry3D.Vector.toString (Seq.nth nv 79) ^ "\n");
    print(Geometry3D.Vector.toString (Seq.nth nv 80) ^ "\n");
    print(Geometry3D.Vector.toString (Seq.nth nv 81) ^ "\n");
    
    print(Geometry3D.Vector.toString (Seq.nth iter 0 ) ^ "\n");
    print(Geometry3D.Vector.toString (Seq.nth iter 1) ^ "\n");
    print(Geometry3D.Vector.toString (Seq.nth iter 2) ^ "\n");
    print(Geometry3D.Vector.toString (Seq.nth iter 3) ^ "\n");
    print(Geometry3D.Vector.toString (Seq.nth iter 4) ^ "\n");
    *)
    42

  end
