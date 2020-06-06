extern crate cilk;
mod codegen;
mod parser;

fn main() {
    // let input = r#"
    // function main(): i32 {
    //     var a: i32; a = 10000;
    //     var c: i32; c = 8400;
    //     var b: i32;
    //     var d: i32;
    //     var e: i32;
    //     var g: i32;
    //     var f: [8401] i32;
    //
    //     b = 0;
    //     while b < c {
    //         f[b] = a / 5;
    //         b = b + 1;
    //     }
    //
    //     e = 0;
    //     c = 8400;
    //     while 0 < c {
    //         d = 0;
    //         b = c - 1;
    //         while 0 < b {
    //             g = b * 2- 1;
    //             d = d * b + f[b] * a;
    //             f[b] = d % g;
    //             d = d / g;
    //             b = b - 1;
    //         }
    //
    //         println_i32(e + d / a);
    //
    //         e = d % a;
    //         c = c - 14;
    //     }
    //
    //     return 0;
    // }
    // "#;
    // let input = r#"
    // struct A {
    //     first: [8][8] i32,
    //     second: i32
    // }
    // function f(): i32 {
    //     println_i32(2);
    //     return  0;
    // }
    // function main(): i32 {
    //     f();
    //     return 0;
    // }
    // "#;

    // let input = r#"
    // function m(c_x: f64, c_y: f64, n: i32): i32 {
    //     var x_n: f64; x_n = 0.0;
    //     var y_n: f64; y_n = 0.0;
    //     var x_n_1: f64; var y_n_1: f64;
    //     var i: i32;
    //     i = 0;
    //     while i < n {
    //         x_n_1 = x_n*x_n - y_n*y_n + c_x;
    //         y_n_1 = x_n * y_n * 2.0 + c_y;
    //         if 4.0 < x_n_1*x_n_1 + y_n_1*y_n_1 {
    //             return n;
    //         } else {
    //             x_n = x_n_1;
    //             y_n = y_n_1;
    //         }
    //         i = i + 1;
    //     }
    //     return 0;
    // }
    // function main(): i32 {
    //     var x_max: f64; x_max = 1.0;
    //     var x_min: f64; x_min = 0.0 - 2.0;
    //     var y_max: f64; y_max = 1.0;
    //     var y_min: f64; y_min = 0.0 - 1.0;
    //     var dx: f64; dx = 0.05;
    //     var dy: f64; dy = 0.05;
    //     var y: f64; var x: f64;
    //     y = y_max;
    //     while y_min < y {
    //         x = x_min;
    //         while x < x_max {
    //             if m(x, y, 300) == 0 {
    //                 printch_i32(65);
    //             } else {
    //                 printch_i32(32);
    //             }
    //             x = x + dx;
    //         }
    //         printch_i32(10);
    //         y = y - dy;
    //     }
    //     return 0;
    // }
    //     "#;

    // let input = "
    // function main(): i32 {
    //     println_f64( sin(3.14 / 2.0) );
    //     println_f64( cos(3.14 / 2.0) );
    //     println_f64( sqrt(2.0) );
    //     return 0;
    // }
    // ";

    // let input = "
    // struct Vec {
    //     x: f64,
    //     y: f64,
    //     z: f64
    // }
    // function main(): i32 {
    //     var v: * struct Vec;
    //     v = malloc(24);
    //     (*v).x = 2.3;
    //     return 0;
    // }
    // ";

    // let input = r#"
    // function main(): i32 {
    //     var i: i32;
    //     var k: i32;
    //     i = 0; while i < 10 {
    //         k = 0; while k < 10 {
    //             k = k + 1;
    //         }
    //         i = i + 1;
    //     }
    //     return 0;
    // }
    // "#;

    let input = r#"
    struct Vec {
      x: f64,
      y: f64,
      z: f64
    }

    struct Isect {
      hit: i32,
      hit_point: * struct Vec,
      normal: * struct Vec,
      color: * struct Vec,
      distance: f64,
      ray_dir: * struct Vec
    }

    struct Ray {
      origin: * struct Vec,
      dir: * struct Vec
    }

    struct Sphere {
      radius: f64,
      position: * struct Vec,
      color: * struct Vec
    }

    struct Plane {
      position: * struct Vec,
      normal  : * struct Vec,
      color   : * struct Vec
    }

    struct Env {
      light     : *struct Vec,
      sphere1: *struct Sphere,
      sphere2: *struct Sphere,
      sphere3: *struct Sphere,
      plane   : *struct Plane
    }

    function clamp(t: f64, min: f64, max: f64): f64 {
      if t < min { return min; }
      if max < t { return max; }
      return t;
    }

    function Vec_new(x: f64, y: f64, z: f64): *struct Vec {
      var vec: * struct Vec;
      vec = malloc(128);
      (*vec).x = x;
      (*vec).y = y;
      (*vec).z = z;
      return vec;
    }

    function Vec_add(a: *struct Vec, b: *struct Vec): *struct Vec {
      return Vec_new((*a).x + (*b).x, (*a).y + (*b).y, (*a).z + (*b).z);
    }

    function Vec_sub(a: *struct Vec, b: *struct Vec): *struct Vec {
      return Vec_new((*a).x - (*b).x, (*a).y - (*b).y, (*a).z - (*b).z);
    }

    function Vec_mul(a: *struct Vec, t: f64): *struct Vec {
      return Vec_new((*a).x * t, (*a).y * t, (*a).z * t);
    }

    function Vec_multi(a: *struct Vec, b: *struct Vec): *struct Vec {
      return Vec_new((*a).x * (*b).x, (*a).y * (*b).y, (*a).z * (*b).z);
    }

    function Vec_dot(a: *struct Vec, b: *struct Vec): f64 {
      return (*a).x * (*b).x + (*a).y * (*b).y + (*a).z * (*b).z;
    }

    function Vec_reflect(self: *struct Vec, normal: *struct Vec): *struct Vec {
      return Vec_add(self, Vec_mul(normal, (0.0-2.0)*Vec_dot(normal, self)));
    }

    function Vec_length(v: *struct Vec): f64 {
      return sqrt((*v).x*(*v).x + (*v).y*(*v).y + (*v).z*(*v).z);
    }

    function Vec_normalize(v: *struct Vec): *struct Vec {
      var len: f64;
      var r_len: f64;
      len = Vec_length(v);
      if 0.00000001 < len {
        r_len = 1.0 / len;
        (*v).x = (*v).x * r_len;
        (*v).y = (*v).y * r_len;
        (*v).z = (*v).z * r_len;
      }
      return v;
    }

    function Ray_new(origin: *struct Vec, dir: *struct Vec): *struct Ray {
      var ray: *struct Ray;
      ray = malloc(128);
      (*ray).origin = origin;
      (*ray).dir = dir;
      return ray;
    }

    function Isect_new(
      hit: i32,
      hit_point: *struct Vec,
      normal: *struct Vec,
      color: *struct Vec,
      distance: f64,
      ray_dir: *struct Vec): *struct Isect {
      var i: *struct Isect;
      i = malloc(128);
      (*i).hit       = hit      ;
      (*i).hit_point = hit_point;
      (*i).normal    = normal   ;
      (*i).color     = color    ;
      (*i).distance  = distance ;
      (*i).ray_dir   = ray_dir ;
      return i;
    }

    function Sphere_new(radius: f64, position: *struct Vec, color: *struct Vec): *struct Sphere {
      var s: *struct Sphere;
      s = malloc(128);
      (*s).radius   = radius;
      (*s).position = position;
      (*s).color    = color;
      return s;
    }

    function Sphere_intersect(s: *struct Sphere, light: *struct Vec, ray: *struct Ray, isect: *struct Isect): i32 {
      var rs: *struct Vec;
      var b: f64; var c: f64; var d: f64; var t: f64;
      rs = Vec_sub((*ray).origin, (*s).position);
      b = Vec_dot(rs, (*ray).dir);
      c = Vec_dot(rs, rs) - (*s).radius * (*s).radius;
      d = b * b - c;
      t = 0.0 - b - sqrt(d);
      if d <= 0.0 { return 0; }
      if t <= 0.0001 { return 0; }
      if (*isect).distance <= t { return 0; }
      (*isect).hit_point = Vec_add((*ray).origin, Vec_mul((*ray).dir, t));
      (*isect).normal = Vec_normalize(Vec_sub((*isect).hit_point, (*s).position));
      (*isect).color = Vec_mul((*s).color, clamp(Vec_dot(light, (*isect).normal), 0.1, 1.0));
      (*isect).distance = t;
      (*isect).hit = (*isect).hit + 1;
      (*isect).ray_dir = (*ray).dir;
      return 0;
    }

    function Plane_new(position: *struct Vec, normal: *struct Vec, color: *struct Vec): *struct Plane {
      var p: *struct Plane;
      p = malloc(128);
      (*p).position = position;
      (*p).normal = normal;
      (*p).color = color;
      return p;
    }

    function Plane_intersect(p: *struct Plane, light: *struct Vec, ray: *struct Ray, isect: *struct Isect): i32 {
      var d: f64;
      var v: f64;
      var t: f64;
      var d2: f64;
      var m: f64;
      var n: f64;
      var d3: f64;
      var abs_: f64;
      var f: f64;
      d = 0.0 - Vec_dot((*p).position, (*p).normal);
      v = Vec_dot((*ray).dir, (*p).normal);
      t = 0.0 - (Vec_dot((*ray).origin, (*p).normal) + d) / v;
      if t <= 0.0001 { return 0; }
      if (*isect).distance <= t { return 0; }
      (*isect).hit_point = Vec_add((*ray).origin, Vec_mul((*ray).dir, t));
      (*isect).normal = (*p).normal;
      d2 = clamp(Vec_dot(light, (*isect).normal), 0.1, 1.0);
      m = (*(*isect).hit_point).x - 2.0*floor((*(*isect).hit_point).x / 2.0);
      n = (*(*isect).hit_point).z - 2.0*floor((*(*isect).hit_point).z / 2.0);
      d3 = d2;
      if 1.0 < m { if 1.0 < n { d3 = d3 * 0.5; } }
      else { if m < 1.0 { if n < 1.0 { d3 = d3 * 0.5; } } }
      abs_ = fabs((*(*isect).hit_point).z);
      f = 0.0;
      if abs_ < 25.0 { f = 1.0 - abs_*0.04; }
      (*isect).color = Vec_mul((*p).color, d3 * f);
      (*isect).distance = t;
      (*isect).hit = (*isect).hit + 1;
      (*isect).ray_dir = (*ray).dir;
      return 0;
    }

    function Env_intersect(env: *struct Env, ray: *struct Ray, i: *struct Isect): i32 {
      Sphere_intersect((*env).sphere1, (*env).light, ray, i);
      Sphere_intersect((*env).sphere2, (*env).light, ray, i);
      Sphere_intersect((*env).sphere3, (*env).light, ray, i);
      Plane_intersect((*env).plane, (*env).light, ray, i);
      return 0;
    }

    function Env_new(): *struct Env {
      var env: *struct Env;
      env = malloc(128);
      (*env).light = Vec_new(0.577, 0.577, 0.577);
      (*env).sphere1 = Sphere_new(0.5, Vec_new( 0.0, 0.0-0.5, 0.0), Vec_new(1.0, 0.0, 0.0));
      (*env).sphere2 = Sphere_new(1.0, Vec_new( 2.0,  0.0, cos(10.0 * 0.666)), Vec_new(0.0, 1.0, 0.0));
      (*env).sphere3 = Sphere_new(1.5, Vec_new(0.0-2.0,  0.5, cos(10.0 * 0.333)), Vec_new(0.0, 0.0, 1.0));
      (*env).plane = Plane_new(Vec_new(0.0, 0.0-1.0, 0.0), Vec_new(0.0, 1.0, 0.0), Vec_new(1.0, 1.0, 1.0));
      return env;
    }

    function color_of(t: f64): i32 {
      var ret: i32;
      ret = f64_to_i32((i32_to_f64(256) * clamp(t, 0.0, 1.0)));
      if ret == 256 { return 256 - 1; }
      return ret;
    }

    function print_col(c: *struct Vec): i32 {
      print_i32(color_of((*c).x)); printch_i32(32);
      print_i32(color_of((*c).y)); printch_i32(32);
      print_i32(color_of((*c).z)); printch_i32(10);
      return 0;
    }

    function main(): i32 {
      var env: *struct Env;
      var row: i32; var col: i32;
      var x: f64; var y: f64;
      var ray: *struct Ray;
      var i: *struct Isect;
      var dest_col: *struct Vec;
      var temp_col: *struct Vec;
      var j: i32;
      var q: *struct Ray;
      var q1: *struct Ray;
      var q2: *struct Ray;

      env = Env_new();

      row = 0; while row < 300 {
        col = 0; while col < 300 {
          x = i32_to_f64(col) / (300.0 / 2.0) - 1.0;
          y = i32_to_f64(300 - row) / (300.0 / 2.0) - 1.0;

          ray = Ray_new( Vec_new(0.0, 2.0, 6.0), Vec_normalize(Vec_new(x, y, 0.0 - 1.0)) );
          i = Isect_new(0, Vec_new(0.0, 0.0, 0.0), Vec_new(0.0, 0.0, 0.0), Vec_new(0.0, 0.0, 0.0),
                        10000000.0, Vec_new(0.0, 0.0, 0.0));
              Env_intersect(env, ray, i);

          if 0 < (*i).hit {
            dest_col = (*i).color;
            temp_col = Vec_multi(Vec_new(1.0, 1.0, 1.0), (*i).color);
            j = 1; while j < 4 {
              q = Ray_new(Vec_add((*i).hit_point, Vec_mul((*i).normal, 0.0001)),
                                      Vec_reflect((*i).ray_dir, (*i).normal));
              Env_intersect(env, q, i);
              if j < (*i).hit {
                dest_col = Vec_add(dest_col, Vec_multi(temp_col, (*i).color));
                temp_col = Vec_multi(temp_col, (*i).color);
              }

              j = j + 1;
            }
          } else {
          }
          col = col + 1;
        }
        row = row + 1;
      }

      return 0;
    }
                    "#;
    let mut codegen = codegen::CodeGenerator::new();
    codegen.run(input);

    cilk::ir::cse::CommonSubexprElimination::new().run_on_module(&mut codegen.module);
    cilk::ir::mem2reg::Mem2Reg::new().run_on_module(&mut codegen.module);

    println!("{:?}", codegen.module);

    let mut jit = cilk::codegen::x64::exec::jit::JITExecutor::new(&mut codegen.module);
    let func = jit.find_function_by_name("main").unwrap();
    println!("Result: {:?}", jit.run(func, vec![]));
}

#[test]
fn pi() {
    //66/51
    let input = r#"
    function main(output: * [600] i32): i32 {
        var a: i32; a = 10000;
        var c: i32; c = 8400;
        var b: i32;
        var d: i32;
        var e: i32;
        var g: i32;
        var f: [8401] i32;
        var i: i32;

        b = 0;
        while b < c {
            f[b] = a / 5;
            b = b + 1;
        }

        e = 0;
        c = 8400;
        i = 0;
        while 0 < c {
            d = 0;
            b = c - 1;
            while 0 < b {
                g = b * 2- 1;
                d = d * b + f[b] * a;
                f[b] = d % g;
                d = d / g;
                b = b - 1;
            }

            output[i] = e + d / a;

            e = d % a;
            c = c - 14;
            i = i + 1;
        }

        return 0;
    }"#;

    let mut codegen = codegen::CodeGenerator::new();
    codegen.run(input);

    cilk::ir::cse::CommonSubexprElimination::new().run_on_module(&mut codegen.module);
    cilk::ir::mem2reg::Mem2Reg::new().run_on_module(&mut codegen.module);

    let mut jit = cilk::codegen::x64::exec::jit::JITExecutor::new(&mut codegen.module);
    let func = jit.find_function_by_name("main").unwrap();
    let output: [i32; 600] = [0; 600];
    jit.run(
        func,
        vec![cilk::codegen::x64::exec::jit::GenericValue::Address(
            output.as_ptr() as *mut u8,
        )],
    );
    assert_eq!(
        format!(
            "{:?}",
            md5::compute(
                output
                    .iter()
                    .map(|x| {
                        vec![
                            (*x as u32 & 0xff000000 >> 24) as u8,
                            (*x as u32 & 0x00ff0000 >> 16) as u8,
                            (*x as u32 & 0x0000ff00 >> 8) as u8,
                            (*x as u32 & 0x000000ff) as u8,
                        ]
                    })
                    .flatten()
                    .collect::<Vec<u8>>()
            )
        )
        .as_str(),
        "216db1f37dc7240a2b7ce62242948623"
    );
}

#[test]
fn pi2() {
    let input = r#"
    function main(output: * [3750] i32): i32 {
        var a: [52514] i32;
        var b: i32;
        var c: i32;
        var d: i32; d = 0;
        var e: i32;
        var f: i32; f = 10000;
        var g: i32;
        var h: i32; h = 0;
        var i: i32; 
        c = 52500;
        b = 0;
        while b < 52514 {
            a[b] = f / 5;
            b = b + 1;
        }
        i = 0;
        while 0 < c {
            d = d % f;
            e = d;
            b = c - 1;
            while 0 < b {
                g = 2 * b - 1;
                d = d * b + f * a[b];
                a[b] = d % g;
                d = d / g;
                b = b - 1;
            }
            println_i32(e+d/f);
            output[i] = e + d / f;
            i = i + 1;
            c = c - 14;
        }
        return 0;
    }"#;

    let mut codegen = codegen::CodeGenerator::new();
    codegen.run(input);

    cilk::ir::cse::CommonSubexprElimination::new().run_on_module(&mut codegen.module);
    cilk::ir::mem2reg::Mem2Reg::new().run_on_module(&mut codegen.module);

    let mut jit = cilk::codegen::x64::exec::jit::JITExecutor::new(&mut codegen.module);
    let func = jit.find_function_by_name("main").unwrap();
    let output: [i32; 3750] = [0; 3750];
    jit.run(
        func,
        vec![cilk::codegen::x64::exec::jit::GenericValue::Address(
            output.as_ptr() as *mut u8,
        )],
    );
    assert_eq!(
        format!(
            "{:?}",
            md5::compute(
                output
                    .iter()
                    .map(|x| {
                        vec![
                            (*x as u32 & 0xff000000 >> 24) as u8,
                            (*x as u32 & 0x00ff0000 >> 16) as u8,
                            (*x as u32 & 0x0000ff00 >> 8) as u8,
                            (*x as u32 & 0x000000ff) as u8,
                        ]
                    })
                    .flatten()
                    .collect::<Vec<u8>>()
            )
        ),
        "c8111cd6ad957f35f8c9ceda93d7e961"
    );
}
