structure Geometry3D =
struct

    type Vertex = real * real * real
    type Face = int * int * int
    type Mesh = Vertex Seq.t * Face Seq.t

    structure Vector =
    struct
      type t = real * real * real

      fun vectorToString (x, y, z) =
        "(" ^ Real.toString x ^ ", " ^ Real.toString y ^ ", " ^ Real.toString z ^ ")"

      fun add (x1, y1, z1) (x2, y2, z2) : t = (x1 + x2, y1 + y2, z1 + z2)
      fun sub (x1, y1, z1) (x2, y2, z2) : t = (x1 - x2, y1 - y2, z1 - z2)

      fun dot (x1, y1, z1) (x2, y2, z2) : real = x1 * x2 + y1 * y2 + z1 * z2
      fun cross (x1, y1, z1) (x2, y2, z2) : t = (y1 * z2 - z1 * y2,
                                                    z1 * x2 - x1 * z2,
                                                    x1 * y2 - y1 * x2)

      fun length (x, y, z) : real = Math.sqrt (x * x + y * y + z * z)

      fun scale (x, y, z) c : t = (c * x, c * y, c * z)

      fun normalize (x, y, z) : t =
        let
            val len = length (x, y, z)
        in
            if (Real.== (len, 0.0)) then (0.0, 0.0, 0.0)
            else (x / len, y / len, z/ len)
        end

      fun triangleArea (v1 : t) (v2 : t) (v3 : t) : real = 
        (length (cross (sub v2 v1) (sub v3 v1))) / 2.0

      fun angle_v1 (v1 : t) (v2 : t) (v3 : t) : real = 
        let
          val u = sub v2 v1
          val v = sub v3 v1
          val cos_theta = (dot u v) / ((length u) * (length v))
          val modifier = Real.min(1.0, Real.max(~1.0, cos_theta))
        in
          Math.acos(modifier)
        end
      
      fun voronoi_areas_v1 (v1 : t) (v2 : t) (v3 : t) : real = 
        let
          val a = angle_v1 v1 v2 v3
          val b = angle_v1 v2 v3 v1
          val c = angle_v1 v3 v1 v2
        in
          if a > Math.pi / 2.0 then
            (triangleArea v1 v2 v3) / 2.0
          else if b > Math.pi / 2.0 orelse c > Math.pi / 2.0 then
            (triangleArea v1 v2 v3) / 4.0
          else
            let
              val len_ba = length (sub v2 v1)
              val len_ca = length (sub v3 v1)
            in
              (len_ba * len_ba * (1.0 / Math.tan(c)) + len_ca * len_ca * (1.0 / Math.tan(b))) / 8.0
            end
        end
      
      fun barycentric_areas_v1 (v1 : t) (v2 : t) (v3 : t) : real = 
        (triangleArea v1 v2 v3) / 3.0

    end

    fun forloop(i, stop, foo) =
      if i >= stop then
        ()   (* done *)
      else
        ( foo(i)                 (* do something at the current index, i *)
        ; forloop(i+1, stop, foo)   (* and then continue the loop at index i+1 *)
        )

    fun loop i stop acc f = 
      if i >= stop then acc
      else
        let 
          val acc2 = f i acc
        in
          loop (i + 1) stop acc2 f
        end

    fun atomic_combine_with f (arr, i) x =
      let
        fun loop current =
          let
            val desired = f (current, x)
          in
            if desired = current then
              ()
            else
              let
                val current' =
                  MLton.Parallel.arrayCompareAndSwap (arr, i) (current, desired)
              in
                if current' = current then () else loop current'
              end
          end
      in
        loop (Array.sub (arr, i))
      end
    
    fun atomic_array_update f (arr, locks, i) x = 
      let    
        fun loop locked =
          let
            val locked' =
              MLton.Parallel.arrayCompareAndSwap (locks, i) (locked, false)
          in
            if not locked then
              Array.update (arr, i, f (Array.sub (arr, i), x))
            else 
              loop locked'
          end
      in
        loop (Array.sub (locks, i))
      end

    fun per_face_normals v f =
      let 
        val n = Seq.length f

        fun do_face_normals idx v f : Vector.t =
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
        Parallel.tabulate (0, n) (fn i => do_face_normals i v f)
      end
    
    fun per_vertex_normals v f =
      let 
        val n = Seq.length v
        val face_normals = per_face_normals v f

        fun do_vertex_normal idx v f (face_normals : Vector.t Seq.t) = 
          let
            val nf = Seq.length f

            val weight : Vector.t = loop 0 nf (0.0, 0.0, 0.0) (fn i => fn ww => 
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
        Parallel.tabulate (0, n) (fn i => do_vertex_normal i v f face_normals)
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
        Parallel.tabulate (0, n) (fn i => do_mass i v f)
      end
    
    fun mass_atomic v f = 
      let
        val nv = Seq.length v
        val nf = Seq.length f
        val result = ForkJoin.alloc nv
        val locks = ForkJoin.alloc nv
        
      in
        (* parallelly initialize all locks and elements in result to 0.0 *)
        Parallel.parfor (0, nv) (fn i => Array.update (locks, i, false));
        Parallel.parfor (0, nv) (fn i => Array.update (result, i, 0.0)); 

        (* parallelly calculate mass based on each face *)
        Parallel.parfor (0, nf) (fn i =>
          let
            val (i1, i2, i3) = Seq.nth f i
            val v1 = Seq.nth v i1
            val v2 = Seq.nth v i2
            val v3 = Seq.nth v i3
          in
            atomic_array_update (op+) (result, locks, i1) (Vector.voronoi_areas_v1 v1 v2 v3)
            (* atomic_combine_with (op+) (result, i1) (Vector.voronoi_areas_v1 v1 v2 v3) *)
          end
        );

        Parallel.tabulate (0, nv) (fn i => Array.sub (result, i))
      end

    fun mass_seq v f = 
      let
        val nv = Seq.length v
        val nf = Seq.length f
        val result = ForkJoin.alloc nv
        
      in
        (* initialize all locks and elements in result to 0.0 *)
        forloop (0, nv, fn i => Array.update (result, i, 0.0)); 
        (* calculate mass based on each face *)
        forloop (0, nf, fn i =>
          let
            val (i1, i2, i3) = Seq.nth f i
            val v1 = Seq.nth v i1
            val v2 = Seq.nth v i2
            val v3 = Seq.nth v i3
          in
            Array.update (result, i1, Array.sub (result, i1) + (Vector.voronoi_areas_v1 v1 v2 v3))
          end
        );
        Parallel.tabulate (0, nv) (fn i => Array.sub (result, i))
      end
end
