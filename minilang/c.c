#include <stdio.h>
#include <math.h>

double clamp(double t, double min, double max) {
  if (t < min) { return min; }
  if (max < t) { return max; }
  return t;
}

struct Vec {
  double x;
  double y;
  double z;
};

struct Vec *Vec_new(double x, double y, double z) {
  struct Vec *vec = malloc(sizeof(struct Vec));
  vec->x = x;
  vec->y = y;
  vec->z = z;
  return vec;
}

struct Vec *Vec_add(struct Vec *a, struct Vec *b) {
  return Vec_new(
    a->x + b->x,
    a->y + b->y,
    a->z + b->z);
}

struct Vec *Vec_sub(struct Vec *a, struct Vec *b) {
  return Vec_new(
    a->x - b->x,
    a->y - b->y,
    a->z - b->z);
}

struct Vec *Vec_mul(struct Vec *a, double t) {
  return Vec_new(
    a->x * t,
    a->y * t,
    a->z * t);
}

struct Vec *Vec_multi(struct Vec *a, struct Vec *b) {
  return Vec_new(a->x * b->x, a->y * b->y, a->z * b->z);
}

double Vec_dot(struct Vec *a, struct Vec *b) {
  return a->x * b->x + a->y * b->y + a->z * b->z;
}

struct Vec *Vec_reflect(struct Vec *self, struct Vec *normal) {
  return Vec_add(self, Vec_mul(normal, (0.0-2.0)*Vec_dot(normal, self)));
}

double Vec_length(struct Vec *v) {
  return sqrt(v->x*v->x + v->y*v->y + v->z*v->z);
}

struct Vec *Vec_normalize(struct Vec *v) {
  double len = Vec_length(v);
  if (0.00000001 < len) {
    double r_len = 1.0 / len;
    v->x = v->x * r_len;
    v->y = v->y * r_len;
    v->z = v->z * r_len;
  }
  return v;
}

struct Ray {
  struct Vec *origin;
  struct Vec *dir;
};

struct Ray *Ray_new(struct Vec *origin, struct Vec *dir) {
  struct Ray *ray = malloc(sizeof(struct Ray));
  ray->origin = origin;
  ray->dir = dir;
  return ray;
}

struct Isect {
  int hit;
  struct Vec *hit_point;
  struct Vec *normal;
  struct Vec *color;
  double distance;
  struct Vec *ray_dir;
};

struct Isect *Isect_new(
  int hit,
  struct Vec *hit_point,
  struct Vec *normal,
  struct Vec *color,
  double distance,
  struct Vec *ray_dir) {
  struct Isect *i = malloc(sizeof(struct Isect));
  i->hit       = hit      ;
  i->hit_point = hit_point;
  i->normal    = normal   ;
  i->color     = color    ;
  i->distance  = distance ;
  i->ray_dir   = ray_dir  ;
  return i;
}

struct Sphere {
  double radius;
  struct Vec *position;
  struct Vec *color;
};

struct Sphere *Sphere_new(double radius, struct Vec *position, struct Vec *color) {
  struct Sphere *s = malloc(sizeof(struct Sphere));
  s->radius = radius;
  s->position = position;
  s->color = color;
  return s;
}

int Sphere_intersect(struct Sphere *s, struct Vec *light, struct Ray *ray, struct Isect *isect) {
  struct Vec *rs = Vec_sub(ray->origin, s->position);
  double b = Vec_dot(rs, ray->dir);
  double c = Vec_dot(rs, rs) - s->radius * s->radius;
  double d = b * b - c;
  double t = 0.0 - b - sqrt(d);
  if (0.0 < d && 0.0001 < t && t < isect->distance) {
    isect->hit_point = Vec_add(ray->origin, Vec_mul(ray->dir, t));
    isect->normal = Vec_normalize(Vec_sub(isect->hit_point, s->position));
    isect->color = Vec_mul(s->color, clamp(Vec_dot(light, isect->normal), 0.1, 1.0));
    isect->distance = t;
    isect->hit = isect->hit + 1;
    isect->ray_dir = ray->dir;
  }
  return 0;
}

struct Plane {
  struct Vec *position;
  struct Vec *normal;
  struct Vec *color;
};

struct Plane *Plane_new(struct Vec *position, struct Vec *normal, struct Vec *color) {
  struct Plane *p = malloc(sizeof(struct Plane));
  p->position = position;
  p->normal = normal;
  p->color = color;
  return p;
}

int Plane_intersect(struct Plane *p, struct Vec *light, struct Ray *ray, struct Isect *isect) {
  double d = 0.0 - Vec_dot(p->position, p->normal);
  double v = Vec_dot(ray->dir, p->normal);
  double t = 0.0 - (Vec_dot(ray->origin, p->normal) + d) / v;
  if (0.0001 < t && t < isect->distance) {
    isect->hit_point = Vec_add(ray->origin, Vec_mul(ray->dir, t));
    isect->normal = p->normal;
    double d2 = clamp(Vec_dot(light, isect->normal), 0.1, 1.0);
    // double m = fmod(isect->hit_point->x, 2.0);
    double m = isect->hit_point->x-2*floor(isect->hit_point->x/2.0);
    // double n = fmod(isect->hit_point->z, 2.0);
    double n = isect->hit_point->z-2*floor(isect->hit_point->z/2.0);

    double d3; if ((1.0 < m && 1.0 < n) || (m < 1.0 && n < 1.0)) {
                 d3 = d2 * 0.5;
               } else {
                 d3 = d2;
               }
    double abs_ = fabs(isect->hit_point->z);
    double f; if (abs_ < 25.0) {
                f = 1.0 - 0.04*abs_;
              } else {
                f = 0.0;
              }
    isect->color = Vec_mul(p->color, d3 * f);
    isect->distance = t;
    isect->hit = isect->hit + 1;
    isect->ray_dir = ray->dir;
  }
  return 0;
}

struct Env {
  struct Vec *light;
  struct Sphere *sphere1;
  struct Sphere *sphere2;
  struct Sphere *sphere3;
  struct Plane *plane;
};

int Env_intersect(struct Env *env, struct Ray *ray, struct Isect *i) {
  Sphere_intersect(env->sphere1, env->light, ray, i);
  Sphere_intersect(env->sphere2, env->light, ray, i);
  Sphere_intersect(env->sphere3, env->light, ray, i);
  Plane_intersect (env->plane,   env->light, ray, i);
  return 0;
}

struct Env *Env_new() {
  struct Env *env = malloc(sizeof(struct Env));
  env->light = Vec_new(0.577, 0.577, 0.577);
  env->sphere1 = Sphere_new(0.5, Vec_new( 0.0, 0.0-0.5, sin(0.0)), Vec_new(1.0, 0.0, 0.0));
  env->sphere2 = Sphere_new(1.0, Vec_new( 2.0,  0.0, cos(10 * 0.666)), Vec_new(0.0, 1.0, 0.0));
  env->sphere3 = Sphere_new(1.5, Vec_new(0.0-2.0,  0.5, cos(10 * 0.333)), Vec_new(0.0, 0.0, 1.0));
  env->plane = Plane_new(Vec_new(0.0, 0.0-1.0, 0.0), Vec_new(0.0, 1.0, 0.0), Vec_new(1.0, 1.0, 1.0));
  return env;
}

int color_of(double t) {
  int ret = (int)(((double)256) * clamp(t, 0.0, 1.0));
  if (ret == 256) { return 256 - 1; }
  return ret;
}

void print_col(struct Vec *c) {
  printf("%d %d %d\n", color_of(c->x), color_of(c->y), color_of(c->z));
}

int main() {
  printf("P3\n%d %d\n255\n", 200, 200);

  struct Env *env = Env_new();

  int row = 0; while (row < 300) {
    int col = 0; while (col < 300) {
      double x = ((double)col) / (((double)300) / 2.0) - 1.0;
      double y = ((double)(300 - row)) / (((double)300) / 2.0) - 1.0;

      struct Ray *ray = Ray_new( Vec_new(0.0, 2.0, 6.0), Vec_normalize(Vec_new(x, y, 0.0 - 1.0)) );
      struct Isect *i = Isect_new(0, Vec_new(0.0, 0.0, 0.0), Vec_new(0.0, 0.0, 0.0), Vec_new(0.0, 0.0, 0.0),
                    10000000.0, Vec_new(0.0, 0.0, 0.0));
      Env_intersect(env, ray, i);

      if (0 < i->hit) {
        struct Vec *dest_col = (*i).color;
        struct Vec *temp_col = Vec_multi(Vec_new(1.0, 1.0, 1.0), i->color);
        int j = 1; while (j < 4) {
          struct Ray *q = Ray_new(Vec_add(i->hit_point, Vec_mul(i->normal, 0.0001)),
                                  Vec_reflect(i->ray_dir, i->normal));
          Env_intersect(env, q, i);
          if (j < i->hit) {
            dest_col = Vec_add(dest_col, Vec_multi(temp_col, i->color));
            temp_col = Vec_multi(temp_col, i->color); 
          }

          j = j + 1;
        }
        print_col(dest_col);
      } else {
        print_col(Vec_new(ray->dir->y, ray->dir->y, ray->dir->y));
      }
      col = col + 1;
    }
    row = row + 1;
  }
}

