fun triangle_area2 (a: vec3, b: vec3, c: vec3) : real =
  norm (cross (sub (b, a), sub (c, a)))

fun angle_at_vertex (a: vec3, b: vec3, c: vec3) : real =
  let
    val u = sub (b, a)
    val v = sub (c, a)
    val cos_theta = dot(u, v) / (norm(u) * norm(v))
    val clamped = Real.min(1.0, Real.max(~1.0, cos_theta))
  in
    Math.acos(clamped)
  end

fun compute_voronoi_areas (vertices: vec3 list, faces: face list) : real list =
  let
    val n = List.length vertices
    val init = Array.array(n, 0.0)

    fun add_area (i, a) = Array.update(init, i, Array.sub(init, i) + a)

    fun process_face (i1, i2, i3) =
      let
        val v1 = List.nth(vertices, i1)
        val v2 = List.nth(vertices, i2)
        val v3 = List.nth(vertices, i3)
        val area = triangle_area2(v1, v2, v3) / 2.0

        val a1 = angle_at_vertex(v1, v2, v3)
        val a2 = angle_at_vertex(v2, v3, v1)
        val a3 = angle_at_vertex(v3, v1, v2)

        val (a1deg, a2deg, a3deg) = (a1 * 180.0 / Math.pi, a2 * 180.0 / Math.pi, a3 * 180.0 / Math.pi)
      in
        if Real.max(a1deg, Real.max(a2deg, a3deg)) > 90.0 then
          (* obtuse triangle — handle differently *)
          if a1deg > 90.0 then
            (add_area(i1, area / 2.0); add_area(i2, area / 4.0); add_area(i3, area / 4.0))
          else if a2deg > 90.0 then
            (add_area(i2, area / 2.0); add_area(i3, area / 4.0); add_area(i1, area / 4.0))
          else
            (add_area(i3, area / 2.0); add_area(i1, area / 4.0); add_area(i2, area / 4.0))
        else
          (* non-obtuse triangle — Voronoi area is 1/2 * cotangent weighting *)
          let
            val cot1 = 1.0 / Math.tan(a1)
            val cot2 = 1.0 / Math.tan(a2)
            val cot3 = 1.0 / Math.tan(a3)
            val len1 = norm (sub(v2, v3))
            val len2 = norm (sub(v3, v1))
            val len3 = norm (sub(v1, v2))
          in
            add_area(i1, (len1 * len1 * cot2 + len3 * len3 * cot3) / 8.0);
            add_area(i2, (len2 * len2 * cot3 + len1 * len1 * cot1) / 8.0);
            add_area(i3, (len3 * len3 * cot1 + len2 * len2 * cot2) / 8.0)
          end
      end
  in
    List.app process_face faces;
    Array.foldr (op ::) [] init
  end
