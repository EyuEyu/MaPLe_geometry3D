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
    end

    fun loop i stop acc f = 
      if i >= stop then acc
      else
        let 
          val acc2 = f (i, acc)
        in
          loop (i + 1) stop acc2 f
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

            val weight : Vector.t = loop 0 nf (0.0, 0.0, 0.0) (fn (i, ww) => 
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

end
