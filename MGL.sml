structure MGL:
sig
  type Vertex = Geometry3D.Vertex
  type Face = Geometry3D.Face
  type Vec = Geometry3D.Vector.t

  val loop : int -> int -> 'a -> (int -> 'a -> 'a) -> 'a

  val per_face_normals   : Vertex Seq.t -> Face Seq.t -> Vec Seq.t
  val per_vertex_normals : Vertex Seq.t -> Face Seq.t -> Vec Seq.t
  val mass               : Vertex Seq.t -> Face Seq.t -> real Seq.t
  val mass_atomic        : Vertex Seq.t -> Face Seq.t -> real Seq.t

end =
struct

  type Vertex = Geometry3D.Vertex
  type Face = Geometry3D.Face
  type Vec = Geometry3D.Vector.t

  structure Vector = Geometry3D.Vector

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
      ArraySlice.full (SeqBasis.tabulate 5 (0, n) (fn i => do_face_normals i v f))
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
      ArraySlice.full (SeqBasis.tabulate 5 (0, n) (fn i => do_vertex_normal i v f face_normals))
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
      Parallel.parfor (0, nv) (fn i => Array.update (result, i, 0.0)); 
      Parallel.parfor (0, nf) (fn i =>
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

end