structure MGL:
sig
  structure M : sig
    type t
  end

  type Vertex = Geometry3D.Vertex
  type Face = Geometry3D.Face
  type Vec = Geometry3D.Vector.t
  type MatCoo = M.t

  val per_face_normals          : Vertex Seq.t -> Face Seq.t -> Vec Seq.t
  val per_vertex_normals        : Vertex Seq.t -> Face Seq.t -> Vec Seq.t
  val per_vertex_normals_atomic : Vertex Seq.t -> Face Seq.t -> Vec Seq.t
  val mass                      : Vertex Seq.t -> Face Seq.t -> real Seq.t
  val mass_atomic               : Vertex Seq.t -> Face Seq.t -> real Seq.t
  val cotmatrix_entries         : Vertex Seq.t -> Face Seq.t -> (real * real * real) Seq.t
  val cotmatrix_triplet         : Vertex Seq.t -> Face Seq.t -> ((int * int) * real) Seq.t
  val cotmatrix                 : Vertex Seq.t -> Face Seq.t -> MatCoo
  val iteration_preps           : Vertex Seq.t -> Face Seq.t -> (MatCoo * real Seq.t)
  val iteration_step            : Vertex Seq.t -> MatCoo -> real Seq.t -> Vec Seq.t

end =
struct

  structure Vector = Geometry3D.Vector
  structure M = MatCOO(structure I = Int
                       structure R = Real)

  type Vertex = Geometry3D.Vertex
  type Face = Geometry3D.Face
  type Vec = Geometry3D.Vector.t
  type MatCoo = int Seq.t * int Seq.t * real Seq.t
  type MatCoo = M.t

  fun loop i stop acc f = 
    if i >= stop then acc
    else
      let 
        val acc2 = f i acc
      in
        loop (i + 1) stop acc2 f
      end
  
  fun atomic_array_update f (arr, i) x is_equal =
    let
      fun loop current =
        let
          val desired = f (current, x)
        in
          if is_equal(desired, current) then
            ()
          else
            let
              val current' =
                MLton.Parallel.arrayCompareAndSwap (arr, i) (current, desired)
            in
              if is_equal(current', current) then () else loop current'
            end
        end
    in
      loop (Array.sub (arr, i))
    end

  
  fun per_face_normals v f =
    let 
      val n = Seq.length f

      fun do_face_normals idx v f =
        let
          val (i1, i2, i3) = Seq.nth f idx

          val v1 = Seq.nth v i1
          val v2 = Seq.nth v i2
          val v3 = Seq.nth v i3

          val e1 = Vector.sub v2 v1
          val e2 = Vector.sub v3 v1

          val normal = Vector.cross e1 e2
          val unitNormal = Vector.normalize normal
        in
          unitNormal
        end

    in
      ArraySlice.full (SeqBasis.tabulate 256 (0, n) (fn i => do_face_normals i v f))
    end

  fun per_vertex_normals v f =
    let 
      val n = Seq.length v
      val nf = Seq.length f
      val face_normals = per_face_normals v f

      fun do_vertex_normal idx v f (face_normals : Vec Seq.t) = 
        let
          val weight : Vec = loop 0 nf (0.0, 0.0, 0.0) (fn i => fn ww => 
            let
              val (v1, v2, v3) = Seq.nth f i
            in
              if ((v1 = idx) orelse (v2 = idx) orelse (v3 = idx))
              then Vector.add ww (Seq.nth face_normals i)
              else
                ww
            end
          )
        in 
          Vector.normalize weight
        end

    in
      ArraySlice.full (SeqBasis.tabulate 128 (0, n) (fn i => do_vertex_normal i v f face_normals))
    end
  
  fun per_vertex_normals_atomic v f = 
    let 
      val nv = Seq.length v
      val nf = Seq.length f
      val face_normals = per_face_normals v f
      val result = ForkJoin.alloc nv
    in 
      Parallel.parforg 64 (0, nv) (fn i => Array.update (result, i, Vector.zero));
      Parallel.parforg 64 (0, nf) (fn i =>
        let
          val normal = Seq.nth face_normals i
          val (i1, i2, i3) = Seq.nth f i
          val v1 = Seq.nth v i1
          val v2 = Seq.nth v i2
          val v3 = Seq.nth v i3
        in
          atomic_array_update Vector.add_tuple_input (result, i1) normal Vector.eq_tuple_input;
          atomic_array_update Vector.add_tuple_input (result, i2) normal Vector.eq_tuple_input;
          atomic_array_update Vector.add_tuple_input (result, i3) normal Vector.eq_tuple_input
        end
      );
      Parallel.parforg 64 (0, nv) (fn i =>
        Array.update (result, i , Vector.normalize (Array.sub(result, i)))
      );
      ArraySlice.full result
    end 

  fun mass v f =
    let 
      val n = Seq.length v
      val nf = Seq.length f

      fun do_mass idx v f = 
        let
          val weight : real = loop 0 nf 0.0 (fn i => fn ww => 
            let
              val (i1, i2, i3) = Seq.nth f i
              val v1 = Seq.nth v i1
              val v2 = Seq.nth v i2
              val v3 = Seq.nth v i3
            in
              if (i1 = idx) then
                ww + (Vector.voronoi_areas_v1 v1 v2 v3)
              else if (i2 = idx) then
                ww + (Vector.voronoi_areas_v1 v2 v3 v1)
              else if (i3 = idx) then
                ww + (Vector.voronoi_areas_v1 v3 v1 v2)
              else
                ww
            end
          )
        in 
          weight
        end
    in
      ArraySlice.full (SeqBasis.tabulate 5 (0, n) (fn i => do_mass i v f))
    end
  
  fun mass_atomic v f = 
    let
      val nv = Seq.length v
      val nf = Seq.length f
      val result = ForkJoin.alloc nv
    in
      Parallel.parforg 32 (0, nv) (fn i => Array.update (result, i, 0.0)); 
      Parallel.parforg 32 (0, nf) (fn i =>
        let
          val (i1, i2, i3) = Seq.nth f i
          val v1 = Seq.nth v i1
          val v2 = Seq.nth v i2
          val v3 = Seq.nth v i3
        in
          atomic_array_update Real.+ (result, i1) (Vector.voronoi_areas_v1 v1 v2 v3) Real.==;
          atomic_array_update Real.+ (result, i2) (Vector.voronoi_areas_v1 v2 v3 v1) Real.==;
          atomic_array_update Real.+ (result, i3) (Vector.voronoi_areas_v1 v3 v1 v2) Real.==
        end
      );
      ArraySlice.full result
    end

  fun cotmatrix_entries v f =
    let 
      val n = Seq.length f

      fun do_face_cot idx v f =
        let
          val (i1, i2, i3) = Seq.nth f idx

          val v1 = Seq.nth v i1
          val v2 = Seq.nth v i2
          val v3 = Seq.nth v i3
        in
          (
            Vector.cotangent (Vector.sub v2 v1) (Vector.sub v3 v1), 
            Vector.cotangent (Vector.sub v3 v2) (Vector.sub v1 v2), 
            Vector.cotangent (Vector.sub v1 v3) (Vector.sub v2 v3)
          )
        end
    in
      ArraySlice.full (SeqBasis.tabulate 256 (0, n) (fn i => do_face_cot i v f))
    end
  
  fun cotmatrix_triplet  v f =
    let 
      val ce = cotmatrix_entries v f
      val nf = Seq.length f
      val nv = Seq.length v

      val ht = Hashtable.make {
        hash = fn (i, j) => i * nv + j,
        eq = fn ((i1, j1), (i2 , j2)) => i1 = i2 andalso j1 = j2,
        capacity = 14 * nf
      }
      
      fun cmp (((i1, j1), _), ((i2, j2), _)) =
        if i1 < i2 then LESS
        else if i1 > i2 then GREATER
        else
          if j1 < j2 then LESS
          else if j1 > j2 then GREATER
          else EQUAL
    in
      Parallel.parfor (0, nf) (fn i => 
        let
          val (v1, v2, v3) = Seq.nth f i
          val (c1, c2, c3) = Seq.nth ce i
        in
          Hashtable.insert_combine ht ((v1, v2), c3) Real.+;
          Hashtable.insert_combine ht ((v2, v1), c3) Real.+;
          Hashtable.insert_combine ht ((v1, v1), ~c3) Real.+;
          Hashtable.insert_combine ht ((v2, v2), ~c3) Real.+;

          Hashtable.insert_combine ht ((v2, v3), c1) Real.+;
          Hashtable.insert_combine ht ((v3, v2), c1) Real.+;
          Hashtable.insert_combine ht ((v2, v2), ~c1) Real.+;
          Hashtable.insert_combine ht ((v3, v3), ~c1) Real.+;

          Hashtable.insert_combine ht ((v3, v1), c2) Real.+;
          Hashtable.insert_combine ht ((v1, v3), c2) Real.+;
          Hashtable.insert_combine ht ((v3, v3), ~c2) Real.+;
          Hashtable.insert_combine ht ((v1, v1), ~c2) Real.+
        end
      );      

      Mergesort.sort cmp (Hashtable.to_seq ht)

    end

  fun cotmatrix v f = 
    let 
      val cot_array = cotmatrix_triplet v f
      val nv = Seq.length v
      val n = Seq.length cot_array
      val i_seq = ForkJoin.alloc n
      val j_seq = ForkJoin.alloc n
      val v_seq = ForkJoin.alloc n
    in
      Parallel.parforg 128 (0, n) (fn k => 
        let
          val triplet = Seq.nth cot_array k
          val (i, j) = #1 triplet
          val v      = #2 triplet
        in
          Array.update (i_seq, k, i);
          Array.update (j_seq, k, j);
          Array.update (v_seq, k, v)
        end
      );

      M.Mat {
      width = nv,
      height = nv,
      row_indices = ArraySlice.full i_seq,
      col_indices = ArraySlice.full j_seq,
      values = ArraySlice.full v_seq
      }
    end

  fun iteration_preps v f =
    let 
      val nv = Seq.length v
      val cotmat = cotmatrix v f
      val weight = ForkJoin.alloc nv
    in 
      Parallel.parforg 128 (0, nv) (fn i => Array.update (weight, i, 1.0));
      (cotmat, M.mxv cotmat (ArraySlice.full weight))
    end

  fun iteration_step v cotmat weight =
    let 
      val nv = Seq.length v
      val vx = Seq.map (fn (x, _, _) => x) v
      val xx = M.mxv cotmat vx
      val vy = Seq.map (fn (_, y, _) => y) v
      val yy = M.mxv cotmat vy
      val vz = Seq.map (fn (_, _, z) => z) v
      val zz = M.mxv cotmat vz
    in 
      ArraySlice.full (SeqBasis.tabulate 256 (0, nv) (fn i => 
        Vector.scale ( (Seq.nth xx i), (Seq.nth yy i), (Seq.nth zz i) ) (1.0 / Seq.nth weight i)
        )
      )
    end

end