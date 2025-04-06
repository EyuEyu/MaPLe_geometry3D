val _ =
  let
    val mesh = read_triangle_mesh "./sphere.obj"
    val output = meshToString mesh
    val _ = print output
  in
    0
  end
