; ModuleID = 'c.c'
source_filename = "c.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.Vec = type { double, double, double }
%struct.Ray = type { %struct.Vec*, %struct.Vec* }
%struct.Isect = type { i32, %struct.Vec*, %struct.Vec*, %struct.Vec*, double, %struct.Vec* }
%struct.Sphere = type { double, %struct.Vec*, %struct.Vec* }
%struct.Plane = type { %struct.Vec*, %struct.Vec*, %struct.Vec* }
%struct.Env = type { %struct.Vec*, %struct.Sphere*, %struct.Sphere*, %struct.Sphere*, %struct.Plane* }

@.str = private unnamed_addr constant [10 x i8] c"%d %d %d\0A\00", align 1
@.str.1 = private unnamed_addr constant [14 x i8] c"P3\0A%d %d\0A255\0A\00", align 1

; Function Attrs: noinline nounwind optnone uwtable
define double @clamp(double, double, double) #0 {
  %4 = alloca double, align 8
  %5 = alloca double, align 8
  %6 = alloca double, align 8
  %7 = alloca double, align 8
  store double %0, double* %5, align 8
  store double %1, double* %6, align 8
  store double %2, double* %7, align 8
  %8 = load double, double* %5, align 8
  %9 = load double, double* %6, align 8
  %10 = fcmp olt double %8, %9
  br i1 %10, label %11, label %13

; <label>:11:                                     ; preds = %3
  %12 = load double, double* %6, align 8
  store double %12, double* %4, align 8
  br label %21

; <label>:13:                                     ; preds = %3
  %14 = load double, double* %7, align 8
  %15 = load double, double* %5, align 8
  %16 = fcmp olt double %14, %15
  br i1 %16, label %17, label %19

; <label>:17:                                     ; preds = %13
  %18 = load double, double* %7, align 8
  store double %18, double* %4, align 8
  br label %21

; <label>:19:                                     ; preds = %13
  %20 = load double, double* %5, align 8
  store double %20, double* %4, align 8
  br label %21

; <label>:21:                                     ; preds = %19, %17, %11
  %22 = load double, double* %4, align 8
  ret double %22
}

; Function Attrs: noinline nounwind optnone uwtable
define %struct.Vec* @Vec_new(double, double, double) #0 {
  %4 = alloca double, align 8
  %5 = alloca double, align 8
  %6 = alloca double, align 8
  %7 = alloca %struct.Vec*, align 8
  store double %0, double* %4, align 8
  store double %1, double* %5, align 8
  store double %2, double* %6, align 8
  %8 = call i8* @malloc(i64 24)
  %9 = bitcast i8* %8 to %struct.Vec*
  store %struct.Vec* %9, %struct.Vec** %7, align 8
  %10 = load double, double* %4, align 8
  %11 = load %struct.Vec*, %struct.Vec** %7, align 8
  %12 = getelementptr inbounds %struct.Vec, %struct.Vec* %11, i32 0, i32 0
  store double %10, double* %12, align 8
  %13 = load double, double* %5, align 8
  %14 = load %struct.Vec*, %struct.Vec** %7, align 8
  %15 = getelementptr inbounds %struct.Vec, %struct.Vec* %14, i32 0, i32 1
  store double %13, double* %15, align 8
  %16 = load double, double* %6, align 8
  %17 = load %struct.Vec*, %struct.Vec** %7, align 8
  %18 = getelementptr inbounds %struct.Vec, %struct.Vec* %17, i32 0, i32 2
  store double %16, double* %18, align 8
  %19 = load %struct.Vec*, %struct.Vec** %7, align 8
  ret %struct.Vec* %19
}

declare i8* @malloc(i64) #1

; Function Attrs: noinline nounwind optnone uwtable
define %struct.Vec* @Vec_add(%struct.Vec*, %struct.Vec*) #0 {
  %3 = alloca %struct.Vec*, align 8
  %4 = alloca %struct.Vec*, align 8
  store %struct.Vec* %0, %struct.Vec** %3, align 8
  store %struct.Vec* %1, %struct.Vec** %4, align 8
  %5 = load %struct.Vec*, %struct.Vec** %3, align 8
  %6 = getelementptr inbounds %struct.Vec, %struct.Vec* %5, i32 0, i32 0
  %7 = load double, double* %6, align 8
  %8 = load %struct.Vec*, %struct.Vec** %4, align 8
  %9 = getelementptr inbounds %struct.Vec, %struct.Vec* %8, i32 0, i32 0
  %10 = load double, double* %9, align 8
  %11 = fadd double %7, %10
  %12 = load %struct.Vec*, %struct.Vec** %3, align 8
  %13 = getelementptr inbounds %struct.Vec, %struct.Vec* %12, i32 0, i32 1
  %14 = load double, double* %13, align 8
  %15 = load %struct.Vec*, %struct.Vec** %4, align 8
  %16 = getelementptr inbounds %struct.Vec, %struct.Vec* %15, i32 0, i32 1
  %17 = load double, double* %16, align 8
  %18 = fadd double %14, %17
  %19 = load %struct.Vec*, %struct.Vec** %3, align 8
  %20 = getelementptr inbounds %struct.Vec, %struct.Vec* %19, i32 0, i32 2
  %21 = load double, double* %20, align 8
  %22 = load %struct.Vec*, %struct.Vec** %4, align 8
  %23 = getelementptr inbounds %struct.Vec, %struct.Vec* %22, i32 0, i32 2
  %24 = load double, double* %23, align 8
  %25 = fadd double %21, %24
  %26 = call %struct.Vec* @Vec_new(double %11, double %18, double %25)
  ret %struct.Vec* %26
}

; Function Attrs: noinline nounwind optnone uwtable
define %struct.Vec* @Vec_sub(%struct.Vec*, %struct.Vec*) #0 {
  %3 = alloca %struct.Vec*, align 8
  %4 = alloca %struct.Vec*, align 8
  store %struct.Vec* %0, %struct.Vec** %3, align 8
  store %struct.Vec* %1, %struct.Vec** %4, align 8
  %5 = load %struct.Vec*, %struct.Vec** %3, align 8
  %6 = getelementptr inbounds %struct.Vec, %struct.Vec* %5, i32 0, i32 0
  %7 = load double, double* %6, align 8
  %8 = load %struct.Vec*, %struct.Vec** %4, align 8
  %9 = getelementptr inbounds %struct.Vec, %struct.Vec* %8, i32 0, i32 0
  %10 = load double, double* %9, align 8
  %11 = fsub double %7, %10
  %12 = load %struct.Vec*, %struct.Vec** %3, align 8
  %13 = getelementptr inbounds %struct.Vec, %struct.Vec* %12, i32 0, i32 1
  %14 = load double, double* %13, align 8
  %15 = load %struct.Vec*, %struct.Vec** %4, align 8
  %16 = getelementptr inbounds %struct.Vec, %struct.Vec* %15, i32 0, i32 1
  %17 = load double, double* %16, align 8
  %18 = fsub double %14, %17
  %19 = load %struct.Vec*, %struct.Vec** %3, align 8
  %20 = getelementptr inbounds %struct.Vec, %struct.Vec* %19, i32 0, i32 2
  %21 = load double, double* %20, align 8
  %22 = load %struct.Vec*, %struct.Vec** %4, align 8
  %23 = getelementptr inbounds %struct.Vec, %struct.Vec* %22, i32 0, i32 2
  %24 = load double, double* %23, align 8
  %25 = fsub double %21, %24
  %26 = call %struct.Vec* @Vec_new(double %11, double %18, double %25)
  ret %struct.Vec* %26
}

; Function Attrs: noinline nounwind optnone uwtable
define %struct.Vec* @Vec_mul(%struct.Vec*, double) #0 {
  %3 = alloca %struct.Vec*, align 8
  %4 = alloca double, align 8
  store %struct.Vec* %0, %struct.Vec** %3, align 8
  store double %1, double* %4, align 8
  %5 = load %struct.Vec*, %struct.Vec** %3, align 8
  %6 = getelementptr inbounds %struct.Vec, %struct.Vec* %5, i32 0, i32 0
  %7 = load double, double* %6, align 8
  %8 = load double, double* %4, align 8
  %9 = fmul double %7, %8
  %10 = load %struct.Vec*, %struct.Vec** %3, align 8
  %11 = getelementptr inbounds %struct.Vec, %struct.Vec* %10, i32 0, i32 1
  %12 = load double, double* %11, align 8
  %13 = load double, double* %4, align 8
  %14 = fmul double %12, %13
  %15 = load %struct.Vec*, %struct.Vec** %3, align 8
  %16 = getelementptr inbounds %struct.Vec, %struct.Vec* %15, i32 0, i32 2
  %17 = load double, double* %16, align 8
  %18 = load double, double* %4, align 8
  %19 = fmul double %17, %18
  %20 = call %struct.Vec* @Vec_new(double %9, double %14, double %19)
  ret %struct.Vec* %20
}

; Function Attrs: noinline nounwind optnone uwtable
define %struct.Vec* @Vec_multi(%struct.Vec*, %struct.Vec*) #0 {
  %3 = alloca %struct.Vec*, align 8
  %4 = alloca %struct.Vec*, align 8
  store %struct.Vec* %0, %struct.Vec** %3, align 8
  store %struct.Vec* %1, %struct.Vec** %4, align 8
  %5 = load %struct.Vec*, %struct.Vec** %3, align 8
  %6 = getelementptr inbounds %struct.Vec, %struct.Vec* %5, i32 0, i32 0
  %7 = load double, double* %6, align 8
  %8 = load %struct.Vec*, %struct.Vec** %4, align 8
  %9 = getelementptr inbounds %struct.Vec, %struct.Vec* %8, i32 0, i32 0
  %10 = load double, double* %9, align 8
  %11 = fmul double %7, %10
  %12 = load %struct.Vec*, %struct.Vec** %3, align 8
  %13 = getelementptr inbounds %struct.Vec, %struct.Vec* %12, i32 0, i32 1
  %14 = load double, double* %13, align 8
  %15 = load %struct.Vec*, %struct.Vec** %4, align 8
  %16 = getelementptr inbounds %struct.Vec, %struct.Vec* %15, i32 0, i32 1
  %17 = load double, double* %16, align 8
  %18 = fmul double %14, %17
  %19 = load %struct.Vec*, %struct.Vec** %3, align 8
  %20 = getelementptr inbounds %struct.Vec, %struct.Vec* %19, i32 0, i32 2
  %21 = load double, double* %20, align 8
  %22 = load %struct.Vec*, %struct.Vec** %4, align 8
  %23 = getelementptr inbounds %struct.Vec, %struct.Vec* %22, i32 0, i32 2
  %24 = load double, double* %23, align 8
  %25 = fmul double %21, %24
  %26 = call %struct.Vec* @Vec_new(double %11, double %18, double %25)
  ret %struct.Vec* %26
}

; Function Attrs: noinline nounwind optnone uwtable
define double @Vec_dot(%struct.Vec*, %struct.Vec*) #0 {
  %3 = alloca %struct.Vec*, align 8
  %4 = alloca %struct.Vec*, align 8
  store %struct.Vec* %0, %struct.Vec** %3, align 8
  store %struct.Vec* %1, %struct.Vec** %4, align 8
  %5 = load %struct.Vec*, %struct.Vec** %3, align 8
  %6 = getelementptr inbounds %struct.Vec, %struct.Vec* %5, i32 0, i32 0
  %7 = load double, double* %6, align 8
  %8 = load %struct.Vec*, %struct.Vec** %4, align 8
  %9 = getelementptr inbounds %struct.Vec, %struct.Vec* %8, i32 0, i32 0
  %10 = load double, double* %9, align 8
  %11 = fmul double %7, %10
  %12 = load %struct.Vec*, %struct.Vec** %3, align 8
  %13 = getelementptr inbounds %struct.Vec, %struct.Vec* %12, i32 0, i32 1
  %14 = load double, double* %13, align 8
  %15 = load %struct.Vec*, %struct.Vec** %4, align 8
  %16 = getelementptr inbounds %struct.Vec, %struct.Vec* %15, i32 0, i32 1
  %17 = load double, double* %16, align 8
  %18 = fmul double %14, %17
  %19 = fadd double %11, %18
  %20 = load %struct.Vec*, %struct.Vec** %3, align 8
  %21 = getelementptr inbounds %struct.Vec, %struct.Vec* %20, i32 0, i32 2
  %22 = load double, double* %21, align 8
  %23 = load %struct.Vec*, %struct.Vec** %4, align 8
  %24 = getelementptr inbounds %struct.Vec, %struct.Vec* %23, i32 0, i32 2
  %25 = load double, double* %24, align 8
  %26 = fmul double %22, %25
  %27 = fadd double %19, %26
  ret double %27
}

; Function Attrs: noinline nounwind optnone uwtable
define %struct.Vec* @Vec_reflect(%struct.Vec*, %struct.Vec*) #0 {
  %3 = alloca %struct.Vec*, align 8
  %4 = alloca %struct.Vec*, align 8
  store %struct.Vec* %0, %struct.Vec** %3, align 8
  store %struct.Vec* %1, %struct.Vec** %4, align 8
  %5 = load %struct.Vec*, %struct.Vec** %3, align 8
  %6 = load %struct.Vec*, %struct.Vec** %4, align 8
  %7 = load %struct.Vec*, %struct.Vec** %4, align 8
  %8 = load %struct.Vec*, %struct.Vec** %3, align 8
  %9 = call double @Vec_dot(%struct.Vec* %7, %struct.Vec* %8)
  %10 = fmul double -2.000000e+00, %9
  %11 = call %struct.Vec* @Vec_mul(%struct.Vec* %6, double %10)
  %12 = call %struct.Vec* @Vec_add(%struct.Vec* %5, %struct.Vec* %11)
  ret %struct.Vec* %12
}

; Function Attrs: noinline nounwind optnone uwtable
define double @Vec_length(%struct.Vec*) #0 {
  %2 = alloca %struct.Vec*, align 8
  store %struct.Vec* %0, %struct.Vec** %2, align 8
  %3 = load %struct.Vec*, %struct.Vec** %2, align 8
  %4 = getelementptr inbounds %struct.Vec, %struct.Vec* %3, i32 0, i32 0
  %5 = load double, double* %4, align 8
  %6 = load %struct.Vec*, %struct.Vec** %2, align 8
  %7 = getelementptr inbounds %struct.Vec, %struct.Vec* %6, i32 0, i32 0
  %8 = load double, double* %7, align 8
  %9 = fmul double %5, %8
  %10 = load %struct.Vec*, %struct.Vec** %2, align 8
  %11 = getelementptr inbounds %struct.Vec, %struct.Vec* %10, i32 0, i32 1
  %12 = load double, double* %11, align 8
  %13 = load %struct.Vec*, %struct.Vec** %2, align 8
  %14 = getelementptr inbounds %struct.Vec, %struct.Vec* %13, i32 0, i32 1
  %15 = load double, double* %14, align 8
  %16 = fmul double %12, %15
  %17 = fadd double %9, %16
  %18 = load %struct.Vec*, %struct.Vec** %2, align 8
  %19 = getelementptr inbounds %struct.Vec, %struct.Vec* %18, i32 0, i32 2
  %20 = load double, double* %19, align 8
  %21 = load %struct.Vec*, %struct.Vec** %2, align 8
  %22 = getelementptr inbounds %struct.Vec, %struct.Vec* %21, i32 0, i32 2
  %23 = load double, double* %22, align 8
  %24 = fmul double %20, %23
  %25 = fadd double %17, %24
  %26 = call double @sqrt(double %25) #4
  ret double %26
}

; Function Attrs: nounwind
declare double @sqrt(double) #2

; Function Attrs: noinline nounwind optnone uwtable
define %struct.Vec* @Vec_normalize(%struct.Vec*) #0 {
  %2 = alloca %struct.Vec*, align 8
  %3 = alloca double, align 8
  %4 = alloca double, align 8
  store %struct.Vec* %0, %struct.Vec** %2, align 8
  %5 = load %struct.Vec*, %struct.Vec** %2, align 8
  %6 = call double @Vec_length(%struct.Vec* %5)
  store double %6, double* %3, align 8
  %7 = load double, double* %3, align 8
  %8 = fcmp olt double 1.000000e-08, %7
  br i1 %8, label %9, label %33

; <label>:9:                                      ; preds = %1
  %10 = load double, double* %3, align 8
  %11 = fdiv double 1.000000e+00, %10
  store double %11, double* %4, align 8
  %12 = load %struct.Vec*, %struct.Vec** %2, align 8
  %13 = getelementptr inbounds %struct.Vec, %struct.Vec* %12, i32 0, i32 0
  %14 = load double, double* %13, align 8
  %15 = load double, double* %4, align 8
  %16 = fmul double %14, %15
  %17 = load %struct.Vec*, %struct.Vec** %2, align 8
  %18 = getelementptr inbounds %struct.Vec, %struct.Vec* %17, i32 0, i32 0
  store double %16, double* %18, align 8
  %19 = load %struct.Vec*, %struct.Vec** %2, align 8
  %20 = getelementptr inbounds %struct.Vec, %struct.Vec* %19, i32 0, i32 1
  %21 = load double, double* %20, align 8
  %22 = load double, double* %4, align 8
  %23 = fmul double %21, %22
  %24 = load %struct.Vec*, %struct.Vec** %2, align 8
  %25 = getelementptr inbounds %struct.Vec, %struct.Vec* %24, i32 0, i32 1
  store double %23, double* %25, align 8
  %26 = load %struct.Vec*, %struct.Vec** %2, align 8
  %27 = getelementptr inbounds %struct.Vec, %struct.Vec* %26, i32 0, i32 2
  %28 = load double, double* %27, align 8
  %29 = load double, double* %4, align 8
  %30 = fmul double %28, %29
  %31 = load %struct.Vec*, %struct.Vec** %2, align 8
  %32 = getelementptr inbounds %struct.Vec, %struct.Vec* %31, i32 0, i32 2
  store double %30, double* %32, align 8
  br label %33

; <label>:33:                                     ; preds = %9, %1
  %34 = load %struct.Vec*, %struct.Vec** %2, align 8
  ret %struct.Vec* %34
}

; Function Attrs: noinline nounwind optnone uwtable
define %struct.Ray* @Ray_new(%struct.Vec*, %struct.Vec*) #0 {
  %3 = alloca %struct.Vec*, align 8
  %4 = alloca %struct.Vec*, align 8
  %5 = alloca %struct.Ray*, align 8
  store %struct.Vec* %0, %struct.Vec** %3, align 8
  store %struct.Vec* %1, %struct.Vec** %4, align 8
  %6 = call i8* @malloc(i64 16)
  %7 = bitcast i8* %6 to %struct.Ray*
  store %struct.Ray* %7, %struct.Ray** %5, align 8
  %8 = load %struct.Vec*, %struct.Vec** %3, align 8
  %9 = load %struct.Ray*, %struct.Ray** %5, align 8
  %10 = getelementptr inbounds %struct.Ray, %struct.Ray* %9, i32 0, i32 0
  store %struct.Vec* %8, %struct.Vec** %10, align 8
  %11 = load %struct.Vec*, %struct.Vec** %4, align 8
  %12 = load %struct.Ray*, %struct.Ray** %5, align 8
  %13 = getelementptr inbounds %struct.Ray, %struct.Ray* %12, i32 0, i32 1
  store %struct.Vec* %11, %struct.Vec** %13, align 8
  %14 = load %struct.Ray*, %struct.Ray** %5, align 8
  ret %struct.Ray* %14
}

; Function Attrs: noinline nounwind optnone uwtable
define %struct.Isect* @Isect_new(i32, %struct.Vec*, %struct.Vec*, %struct.Vec*, double, %struct.Vec*) #0 {
  %7 = alloca i32, align 4
  %8 = alloca %struct.Vec*, align 8
  %9 = alloca %struct.Vec*, align 8
  %10 = alloca %struct.Vec*, align 8
  %11 = alloca double, align 8
  %12 = alloca %struct.Vec*, align 8
  %13 = alloca %struct.Isect*, align 8
  store i32 %0, i32* %7, align 4
  store %struct.Vec* %1, %struct.Vec** %8, align 8
  store %struct.Vec* %2, %struct.Vec** %9, align 8
  store %struct.Vec* %3, %struct.Vec** %10, align 8
  store double %4, double* %11, align 8
  store %struct.Vec* %5, %struct.Vec** %12, align 8
  %14 = call i8* @malloc(i64 48)
  %15 = bitcast i8* %14 to %struct.Isect*
  store %struct.Isect* %15, %struct.Isect** %13, align 8
  %16 = load i32, i32* %7, align 4
  %17 = load %struct.Isect*, %struct.Isect** %13, align 8
  %18 = getelementptr inbounds %struct.Isect, %struct.Isect* %17, i32 0, i32 0
  store i32 %16, i32* %18, align 8
  %19 = load %struct.Vec*, %struct.Vec** %8, align 8
  %20 = load %struct.Isect*, %struct.Isect** %13, align 8
  %21 = getelementptr inbounds %struct.Isect, %struct.Isect* %20, i32 0, i32 1
  store %struct.Vec* %19, %struct.Vec** %21, align 8
  %22 = load %struct.Vec*, %struct.Vec** %9, align 8
  %23 = load %struct.Isect*, %struct.Isect** %13, align 8
  %24 = getelementptr inbounds %struct.Isect, %struct.Isect* %23, i32 0, i32 2
  store %struct.Vec* %22, %struct.Vec** %24, align 8
  %25 = load %struct.Vec*, %struct.Vec** %10, align 8
  %26 = load %struct.Isect*, %struct.Isect** %13, align 8
  %27 = getelementptr inbounds %struct.Isect, %struct.Isect* %26, i32 0, i32 3
  store %struct.Vec* %25, %struct.Vec** %27, align 8
  %28 = load double, double* %11, align 8
  %29 = load %struct.Isect*, %struct.Isect** %13, align 8
  %30 = getelementptr inbounds %struct.Isect, %struct.Isect* %29, i32 0, i32 4
  store double %28, double* %30, align 8
  %31 = load %struct.Vec*, %struct.Vec** %12, align 8
  %32 = load %struct.Isect*, %struct.Isect** %13, align 8
  %33 = getelementptr inbounds %struct.Isect, %struct.Isect* %32, i32 0, i32 5
  store %struct.Vec* %31, %struct.Vec** %33, align 8
  %34 = load %struct.Isect*, %struct.Isect** %13, align 8
  ret %struct.Isect* %34
}

; Function Attrs: noinline nounwind optnone uwtable
define %struct.Sphere* @Sphere_new(double, %struct.Vec*, %struct.Vec*) #0 {
  %4 = alloca double, align 8
  %5 = alloca %struct.Vec*, align 8
  %6 = alloca %struct.Vec*, align 8
  %7 = alloca %struct.Sphere*, align 8
  store double %0, double* %4, align 8
  store %struct.Vec* %1, %struct.Vec** %5, align 8
  store %struct.Vec* %2, %struct.Vec** %6, align 8
  %8 = call i8* @malloc(i64 24)
  %9 = bitcast i8* %8 to %struct.Sphere*
  store %struct.Sphere* %9, %struct.Sphere** %7, align 8
  %10 = load double, double* %4, align 8
  %11 = load %struct.Sphere*, %struct.Sphere** %7, align 8
  %12 = getelementptr inbounds %struct.Sphere, %struct.Sphere* %11, i32 0, i32 0
  store double %10, double* %12, align 8
  %13 = load %struct.Vec*, %struct.Vec** %5, align 8
  %14 = load %struct.Sphere*, %struct.Sphere** %7, align 8
  %15 = getelementptr inbounds %struct.Sphere, %struct.Sphere* %14, i32 0, i32 1
  store %struct.Vec* %13, %struct.Vec** %15, align 8
  %16 = load %struct.Vec*, %struct.Vec** %6, align 8
  %17 = load %struct.Sphere*, %struct.Sphere** %7, align 8
  %18 = getelementptr inbounds %struct.Sphere, %struct.Sphere* %17, i32 0, i32 2
  store %struct.Vec* %16, %struct.Vec** %18, align 8
  %19 = load %struct.Sphere*, %struct.Sphere** %7, align 8
  ret %struct.Sphere* %19
}

; Function Attrs: noinline nounwind optnone uwtable
define i32 @Sphere_intersect(%struct.Sphere*, %struct.Vec*, %struct.Ray*, %struct.Isect*) #0 {
  %5 = alloca %struct.Sphere*, align 8
  %6 = alloca %struct.Vec*, align 8
  %7 = alloca %struct.Ray*, align 8
  %8 = alloca %struct.Isect*, align 8
  %9 = alloca %struct.Vec*, align 8
  %10 = alloca double, align 8
  %11 = alloca double, align 8
  %12 = alloca double, align 8
  %13 = alloca double, align 8
  store %struct.Sphere* %0, %struct.Sphere** %5, align 8
  store %struct.Vec* %1, %struct.Vec** %6, align 8
  store %struct.Ray* %2, %struct.Ray** %7, align 8
  store %struct.Isect* %3, %struct.Isect** %8, align 8
  %14 = load %struct.Ray*, %struct.Ray** %7, align 8
  %15 = getelementptr inbounds %struct.Ray, %struct.Ray* %14, i32 0, i32 0
  %16 = load %struct.Vec*, %struct.Vec** %15, align 8
  %17 = load %struct.Sphere*, %struct.Sphere** %5, align 8
  %18 = getelementptr inbounds %struct.Sphere, %struct.Sphere* %17, i32 0, i32 1
  %19 = load %struct.Vec*, %struct.Vec** %18, align 8
  %20 = call %struct.Vec* @Vec_sub(%struct.Vec* %16, %struct.Vec* %19)
  store %struct.Vec* %20, %struct.Vec** %9, align 8
  %21 = load %struct.Vec*, %struct.Vec** %9, align 8
  %22 = load %struct.Ray*, %struct.Ray** %7, align 8
  %23 = getelementptr inbounds %struct.Ray, %struct.Ray* %22, i32 0, i32 1
  %24 = load %struct.Vec*, %struct.Vec** %23, align 8
  %25 = call double @Vec_dot(%struct.Vec* %21, %struct.Vec* %24)
  store double %25, double* %10, align 8
  %26 = load %struct.Vec*, %struct.Vec** %9, align 8
  %27 = load %struct.Vec*, %struct.Vec** %9, align 8
  %28 = call double @Vec_dot(%struct.Vec* %26, %struct.Vec* %27)
  %29 = load %struct.Sphere*, %struct.Sphere** %5, align 8
  %30 = getelementptr inbounds %struct.Sphere, %struct.Sphere* %29, i32 0, i32 0
  %31 = load double, double* %30, align 8
  %32 = load %struct.Sphere*, %struct.Sphere** %5, align 8
  %33 = getelementptr inbounds %struct.Sphere, %struct.Sphere* %32, i32 0, i32 0
  %34 = load double, double* %33, align 8
  %35 = fmul double %31, %34
  %36 = fsub double %28, %35
  store double %36, double* %11, align 8
  %37 = load double, double* %10, align 8
  %38 = load double, double* %10, align 8
  %39 = fmul double %37, %38
  %40 = load double, double* %11, align 8
  %41 = fsub double %39, %40
  store double %41, double* %12, align 8
  %42 = load double, double* %10, align 8
  %43 = fsub double 0.000000e+00, %42
  %44 = load double, double* %12, align 8
  %45 = call double @sqrt(double %44) #4
  %46 = fsub double %43, %45
  store double %46, double* %13, align 8
  %47 = load double, double* %12, align 8
  %48 = fcmp olt double 0.000000e+00, %47
  br i1 %48, label %49, label %106

; <label>:49:                                     ; preds = %4
  %50 = load double, double* %13, align 8
  %51 = fcmp olt double 1.000000e-04, %50
  br i1 %51, label %52, label %106

; <label>:52:                                     ; preds = %49
  %53 = load double, double* %13, align 8
  %54 = load %struct.Isect*, %struct.Isect** %8, align 8
  %55 = getelementptr inbounds %struct.Isect, %struct.Isect* %54, i32 0, i32 4
  %56 = load double, double* %55, align 8
  %57 = fcmp olt double %53, %56
  br i1 %57, label %58, label %106

; <label>:58:                                     ; preds = %52
  %59 = load %struct.Ray*, %struct.Ray** %7, align 8
  %60 = getelementptr inbounds %struct.Ray, %struct.Ray* %59, i32 0, i32 0
  %61 = load %struct.Vec*, %struct.Vec** %60, align 8
  %62 = load %struct.Ray*, %struct.Ray** %7, align 8
  %63 = getelementptr inbounds %struct.Ray, %struct.Ray* %62, i32 0, i32 1
  %64 = load %struct.Vec*, %struct.Vec** %63, align 8
  %65 = load double, double* %13, align 8
  %66 = call %struct.Vec* @Vec_mul(%struct.Vec* %64, double %65)
  %67 = call %struct.Vec* @Vec_add(%struct.Vec* %61, %struct.Vec* %66)
  %68 = load %struct.Isect*, %struct.Isect** %8, align 8
  %69 = getelementptr inbounds %struct.Isect, %struct.Isect* %68, i32 0, i32 1
  store %struct.Vec* %67, %struct.Vec** %69, align 8
  %70 = load %struct.Isect*, %struct.Isect** %8, align 8
  %71 = getelementptr inbounds %struct.Isect, %struct.Isect* %70, i32 0, i32 1
  %72 = load %struct.Vec*, %struct.Vec** %71, align 8
  %73 = load %struct.Sphere*, %struct.Sphere** %5, align 8
  %74 = getelementptr inbounds %struct.Sphere, %struct.Sphere* %73, i32 0, i32 1
  %75 = load %struct.Vec*, %struct.Vec** %74, align 8
  %76 = call %struct.Vec* @Vec_sub(%struct.Vec* %72, %struct.Vec* %75)
  %77 = call %struct.Vec* @Vec_normalize(%struct.Vec* %76)
  %78 = load %struct.Isect*, %struct.Isect** %8, align 8
  %79 = getelementptr inbounds %struct.Isect, %struct.Isect* %78, i32 0, i32 2
  store %struct.Vec* %77, %struct.Vec** %79, align 8
  %80 = load %struct.Sphere*, %struct.Sphere** %5, align 8
  %81 = getelementptr inbounds %struct.Sphere, %struct.Sphere* %80, i32 0, i32 2
  %82 = load %struct.Vec*, %struct.Vec** %81, align 8
  %83 = load %struct.Vec*, %struct.Vec** %6, align 8
  %84 = load %struct.Isect*, %struct.Isect** %8, align 8
  %85 = getelementptr inbounds %struct.Isect, %struct.Isect* %84, i32 0, i32 2
  %86 = load %struct.Vec*, %struct.Vec** %85, align 8
  %87 = call double @Vec_dot(%struct.Vec* %83, %struct.Vec* %86)
  %88 = call double @clamp(double %87, double 1.000000e-01, double 1.000000e+00)
  %89 = call %struct.Vec* @Vec_mul(%struct.Vec* %82, double %88)
  %90 = load %struct.Isect*, %struct.Isect** %8, align 8
  %91 = getelementptr inbounds %struct.Isect, %struct.Isect* %90, i32 0, i32 3
  store %struct.Vec* %89, %struct.Vec** %91, align 8
  %92 = load double, double* %13, align 8
  %93 = load %struct.Isect*, %struct.Isect** %8, align 8
  %94 = getelementptr inbounds %struct.Isect, %struct.Isect* %93, i32 0, i32 4
  store double %92, double* %94, align 8
  %95 = load %struct.Isect*, %struct.Isect** %8, align 8
  %96 = getelementptr inbounds %struct.Isect, %struct.Isect* %95, i32 0, i32 0
  %97 = load i32, i32* %96, align 8
  %98 = add nsw i32 %97, 1
  %99 = load %struct.Isect*, %struct.Isect** %8, align 8
  %100 = getelementptr inbounds %struct.Isect, %struct.Isect* %99, i32 0, i32 0
  store i32 %98, i32* %100, align 8
  %101 = load %struct.Ray*, %struct.Ray** %7, align 8
  %102 = getelementptr inbounds %struct.Ray, %struct.Ray* %101, i32 0, i32 1
  %103 = load %struct.Vec*, %struct.Vec** %102, align 8
  %104 = load %struct.Isect*, %struct.Isect** %8, align 8
  %105 = getelementptr inbounds %struct.Isect, %struct.Isect* %104, i32 0, i32 5
  store %struct.Vec* %103, %struct.Vec** %105, align 8
  br label %106

; <label>:106:                                    ; preds = %58, %52, %49, %4
  ret i32 0
}

; Function Attrs: noinline nounwind optnone uwtable
define %struct.Plane* @Plane_new(%struct.Vec*, %struct.Vec*, %struct.Vec*) #0 {
  %4 = alloca %struct.Vec*, align 8
  %5 = alloca %struct.Vec*, align 8
  %6 = alloca %struct.Vec*, align 8
  %7 = alloca %struct.Plane*, align 8
  store %struct.Vec* %0, %struct.Vec** %4, align 8
  store %struct.Vec* %1, %struct.Vec** %5, align 8
  store %struct.Vec* %2, %struct.Vec** %6, align 8
  %8 = call i8* @malloc(i64 24)
  %9 = bitcast i8* %8 to %struct.Plane*
  store %struct.Plane* %9, %struct.Plane** %7, align 8
  %10 = load %struct.Vec*, %struct.Vec** %4, align 8
  %11 = load %struct.Plane*, %struct.Plane** %7, align 8
  %12 = getelementptr inbounds %struct.Plane, %struct.Plane* %11, i32 0, i32 0
  store %struct.Vec* %10, %struct.Vec** %12, align 8
  %13 = load %struct.Vec*, %struct.Vec** %5, align 8
  %14 = load %struct.Plane*, %struct.Plane** %7, align 8
  %15 = getelementptr inbounds %struct.Plane, %struct.Plane* %14, i32 0, i32 1
  store %struct.Vec* %13, %struct.Vec** %15, align 8
  %16 = load %struct.Vec*, %struct.Vec** %6, align 8
  %17 = load %struct.Plane*, %struct.Plane** %7, align 8
  %18 = getelementptr inbounds %struct.Plane, %struct.Plane* %17, i32 0, i32 2
  store %struct.Vec* %16, %struct.Vec** %18, align 8
  %19 = load %struct.Plane*, %struct.Plane** %7, align 8
  ret %struct.Plane* %19
}

; Function Attrs: noinline nounwind optnone uwtable
define i32 @Plane_intersect(%struct.Plane*, %struct.Vec*, %struct.Ray*, %struct.Isect*) #0 {
  %5 = alloca %struct.Plane*, align 8
  %6 = alloca %struct.Vec*, align 8
  %7 = alloca %struct.Ray*, align 8
  %8 = alloca %struct.Isect*, align 8
  %9 = alloca double, align 8
  %10 = alloca double, align 8
  %11 = alloca double, align 8
  %12 = alloca double, align 8
  %13 = alloca double, align 8
  %14 = alloca double, align 8
  %15 = alloca double, align 8
  %16 = alloca double, align 8
  %17 = alloca double, align 8
  store %struct.Plane* %0, %struct.Plane** %5, align 8
  store %struct.Vec* %1, %struct.Vec** %6, align 8
  store %struct.Ray* %2, %struct.Ray** %7, align 8
  store %struct.Isect* %3, %struct.Isect** %8, align 8
  %18 = load %struct.Plane*, %struct.Plane** %5, align 8
  %19 = getelementptr inbounds %struct.Plane, %struct.Plane* %18, i32 0, i32 0
  %20 = load %struct.Vec*, %struct.Vec** %19, align 8
  %21 = load %struct.Plane*, %struct.Plane** %5, align 8
  %22 = getelementptr inbounds %struct.Plane, %struct.Plane* %21, i32 0, i32 1
  %23 = load %struct.Vec*, %struct.Vec** %22, align 8
  %24 = call double @Vec_dot(%struct.Vec* %20, %struct.Vec* %23)
  %25 = fsub double 0.000000e+00, %24
  store double %25, double* %9, align 8
  %26 = load %struct.Ray*, %struct.Ray** %7, align 8
  %27 = getelementptr inbounds %struct.Ray, %struct.Ray* %26, i32 0, i32 1
  %28 = load %struct.Vec*, %struct.Vec** %27, align 8
  %29 = load %struct.Plane*, %struct.Plane** %5, align 8
  %30 = getelementptr inbounds %struct.Plane, %struct.Plane* %29, i32 0, i32 1
  %31 = load %struct.Vec*, %struct.Vec** %30, align 8
  %32 = call double @Vec_dot(%struct.Vec* %28, %struct.Vec* %31)
  store double %32, double* %10, align 8
  %33 = load %struct.Ray*, %struct.Ray** %7, align 8
  %34 = getelementptr inbounds %struct.Ray, %struct.Ray* %33, i32 0, i32 0
  %35 = load %struct.Vec*, %struct.Vec** %34, align 8
  %36 = load %struct.Plane*, %struct.Plane** %5, align 8
  %37 = getelementptr inbounds %struct.Plane, %struct.Plane* %36, i32 0, i32 1
  %38 = load %struct.Vec*, %struct.Vec** %37, align 8
  %39 = call double @Vec_dot(%struct.Vec* %35, %struct.Vec* %38)
  %40 = load double, double* %9, align 8
  %41 = fadd double %39, %40
  %42 = load double, double* %10, align 8
  %43 = fdiv double %41, %42
  %44 = fsub double 0.000000e+00, %43
  store double %44, double* %11, align 8
  %45 = load double, double* %11, align 8
  %46 = fcmp olt double 1.000000e-04, %45
  br i1 %46, label %47, label %158

; <label>:47:                                     ; preds = %4
  %48 = load double, double* %11, align 8
  %49 = load %struct.Isect*, %struct.Isect** %8, align 8
  %50 = getelementptr inbounds %struct.Isect, %struct.Isect* %49, i32 0, i32 4
  %51 = load double, double* %50, align 8
  %52 = fcmp olt double %48, %51
  br i1 %52, label %53, label %158

; <label>:53:                                     ; preds = %47
  %54 = load %struct.Ray*, %struct.Ray** %7, align 8
  %55 = getelementptr inbounds %struct.Ray, %struct.Ray* %54, i32 0, i32 0
  %56 = load %struct.Vec*, %struct.Vec** %55, align 8
  %57 = load %struct.Ray*, %struct.Ray** %7, align 8
  %58 = getelementptr inbounds %struct.Ray, %struct.Ray* %57, i32 0, i32 1
  %59 = load %struct.Vec*, %struct.Vec** %58, align 8
  %60 = load double, double* %11, align 8
  %61 = call %struct.Vec* @Vec_mul(%struct.Vec* %59, double %60)
  %62 = call %struct.Vec* @Vec_add(%struct.Vec* %56, %struct.Vec* %61)
  %63 = load %struct.Isect*, %struct.Isect** %8, align 8
  %64 = getelementptr inbounds %struct.Isect, %struct.Isect* %63, i32 0, i32 1
  store %struct.Vec* %62, %struct.Vec** %64, align 8
  %65 = load %struct.Plane*, %struct.Plane** %5, align 8
  %66 = getelementptr inbounds %struct.Plane, %struct.Plane* %65, i32 0, i32 1
  %67 = load %struct.Vec*, %struct.Vec** %66, align 8
  %68 = load %struct.Isect*, %struct.Isect** %8, align 8
  %69 = getelementptr inbounds %struct.Isect, %struct.Isect* %68, i32 0, i32 2
  store %struct.Vec* %67, %struct.Vec** %69, align 8
  %70 = load %struct.Vec*, %struct.Vec** %6, align 8
  %71 = load %struct.Isect*, %struct.Isect** %8, align 8
  %72 = getelementptr inbounds %struct.Isect, %struct.Isect* %71, i32 0, i32 2
  %73 = load %struct.Vec*, %struct.Vec** %72, align 8
  %74 = call double @Vec_dot(%struct.Vec* %70, %struct.Vec* %73)
  %75 = call double @clamp(double %74, double 1.000000e-01, double 1.000000e+00)
  store double %75, double* %12, align 8
  %76 = load %struct.Isect*, %struct.Isect** %8, align 8
  %77 = getelementptr inbounds %struct.Isect, %struct.Isect* %76, i32 0, i32 1
  %78 = load %struct.Vec*, %struct.Vec** %77, align 8
  %79 = getelementptr inbounds %struct.Vec, %struct.Vec* %78, i32 0, i32 0
  %80 = load double, double* %79, align 8
  %81 = load %struct.Isect*, %struct.Isect** %8, align 8
  %82 = getelementptr inbounds %struct.Isect, %struct.Isect* %81, i32 0, i32 1
  %83 = load %struct.Vec*, %struct.Vec** %82, align 8
  %84 = getelementptr inbounds %struct.Vec, %struct.Vec* %83, i32 0, i32 0
  %85 = load double, double* %84, align 8
  %86 = fdiv double %85, 2.000000e+00
  %87 = call double @llvm.floor.f64(double %86)
  %88 = fmul double 2.000000e+00, %87
  %89 = fsub double %80, %88
  store double %89, double* %13, align 8
  %90 = load %struct.Isect*, %struct.Isect** %8, align 8
  %91 = getelementptr inbounds %struct.Isect, %struct.Isect* %90, i32 0, i32 1
  %92 = load %struct.Vec*, %struct.Vec** %91, align 8
  %93 = getelementptr inbounds %struct.Vec, %struct.Vec* %92, i32 0, i32 2
  %94 = load double, double* %93, align 8
  %95 = load %struct.Isect*, %struct.Isect** %8, align 8
  %96 = getelementptr inbounds %struct.Isect, %struct.Isect* %95, i32 0, i32 1
  %97 = load %struct.Vec*, %struct.Vec** %96, align 8
  %98 = getelementptr inbounds %struct.Vec, %struct.Vec* %97, i32 0, i32 2
  %99 = load double, double* %98, align 8
  %100 = fdiv double %99, 2.000000e+00
  %101 = call double @llvm.floor.f64(double %100)
  %102 = fmul double 2.000000e+00, %101
  %103 = fsub double %94, %102
  store double %103, double* %14, align 8
  %104 = load double, double* %13, align 8
  %105 = fcmp olt double 1.000000e+00, %104
  br i1 %105, label %106, label %109

; <label>:106:                                    ; preds = %53
  %107 = load double, double* %14, align 8
  %108 = fcmp olt double 1.000000e+00, %107
  br i1 %108, label %115, label %109

; <label>:109:                                    ; preds = %106, %53
  %110 = load double, double* %13, align 8
  %111 = fcmp olt double %110, 1.000000e+00
  br i1 %111, label %112, label %118

; <label>:112:                                    ; preds = %109
  %113 = load double, double* %14, align 8
  %114 = fcmp olt double %113, 1.000000e+00
  br i1 %114, label %115, label %118

; <label>:115:                                    ; preds = %112, %106
  %116 = load double, double* %12, align 8
  %117 = fmul double %116, 5.000000e-01
  store double %117, double* %15, align 8
  br label %120

; <label>:118:                                    ; preds = %112, %109
  %119 = load double, double* %12, align 8
  store double %119, double* %15, align 8
  br label %120

; <label>:120:                                    ; preds = %118, %115
  %121 = load %struct.Isect*, %struct.Isect** %8, align 8
  %122 = getelementptr inbounds %struct.Isect, %struct.Isect* %121, i32 0, i32 1
  %123 = load %struct.Vec*, %struct.Vec** %122, align 8
  %124 = getelementptr inbounds %struct.Vec, %struct.Vec* %123, i32 0, i32 2
  %125 = load double, double* %124, align 8
  %126 = call double @llvm.fabs.f64(double %125)
  store double %126, double* %16, align 8
  %127 = load double, double* %16, align 8
  %128 = fcmp olt double %127, 2.500000e+01
  br i1 %128, label %129, label %133

; <label>:129:                                    ; preds = %120
  %130 = load double, double* %16, align 8
  %131 = fmul double 4.000000e-02, %130
  %132 = fsub double 1.000000e+00, %131
  store double %132, double* %17, align 8
  br label %134

; <label>:133:                                    ; preds = %120
  store double 0.000000e+00, double* %17, align 8
  br label %134

; <label>:134:                                    ; preds = %133, %129
  %135 = load %struct.Plane*, %struct.Plane** %5, align 8
  %136 = getelementptr inbounds %struct.Plane, %struct.Plane* %135, i32 0, i32 2
  %137 = load %struct.Vec*, %struct.Vec** %136, align 8
  %138 = load double, double* %15, align 8
  %139 = load double, double* %17, align 8
  %140 = fmul double %138, %139
  %141 = call %struct.Vec* @Vec_mul(%struct.Vec* %137, double %140)
  %142 = load %struct.Isect*, %struct.Isect** %8, align 8
  %143 = getelementptr inbounds %struct.Isect, %struct.Isect* %142, i32 0, i32 3
  store %struct.Vec* %141, %struct.Vec** %143, align 8
  %144 = load double, double* %11, align 8
  %145 = load %struct.Isect*, %struct.Isect** %8, align 8
  %146 = getelementptr inbounds %struct.Isect, %struct.Isect* %145, i32 0, i32 4
  store double %144, double* %146, align 8
  %147 = load %struct.Isect*, %struct.Isect** %8, align 8
  %148 = getelementptr inbounds %struct.Isect, %struct.Isect* %147, i32 0, i32 0
  %149 = load i32, i32* %148, align 8
  %150 = add nsw i32 %149, 1
  %151 = load %struct.Isect*, %struct.Isect** %8, align 8
  %152 = getelementptr inbounds %struct.Isect, %struct.Isect* %151, i32 0, i32 0
  store i32 %150, i32* %152, align 8
  %153 = load %struct.Ray*, %struct.Ray** %7, align 8
  %154 = getelementptr inbounds %struct.Ray, %struct.Ray* %153, i32 0, i32 1
  %155 = load %struct.Vec*, %struct.Vec** %154, align 8
  %156 = load %struct.Isect*, %struct.Isect** %8, align 8
  %157 = getelementptr inbounds %struct.Isect, %struct.Isect* %156, i32 0, i32 5
  store %struct.Vec* %155, %struct.Vec** %157, align 8
  br label %158

; <label>:158:                                    ; preds = %134, %47, %4
  ret i32 0
}

; Function Attrs: nounwind readnone speculatable
declare double @llvm.floor.f64(double) #3

; Function Attrs: nounwind readnone speculatable
declare double @llvm.fabs.f64(double) #3

; Function Attrs: noinline nounwind optnone uwtable
define i32 @Env_intersect(%struct.Env*, %struct.Ray*, %struct.Isect*) #0 {
  %4 = alloca %struct.Env*, align 8
  %5 = alloca %struct.Ray*, align 8
  %6 = alloca %struct.Isect*, align 8
  store %struct.Env* %0, %struct.Env** %4, align 8
  store %struct.Ray* %1, %struct.Ray** %5, align 8
  store %struct.Isect* %2, %struct.Isect** %6, align 8
  %7 = load %struct.Env*, %struct.Env** %4, align 8
  %8 = getelementptr inbounds %struct.Env, %struct.Env* %7, i32 0, i32 1
  %9 = load %struct.Sphere*, %struct.Sphere** %8, align 8
  %10 = load %struct.Env*, %struct.Env** %4, align 8
  %11 = getelementptr inbounds %struct.Env, %struct.Env* %10, i32 0, i32 0
  %12 = load %struct.Vec*, %struct.Vec** %11, align 8
  %13 = load %struct.Ray*, %struct.Ray** %5, align 8
  %14 = load %struct.Isect*, %struct.Isect** %6, align 8
  %15 = call i32 @Sphere_intersect(%struct.Sphere* %9, %struct.Vec* %12, %struct.Ray* %13, %struct.Isect* %14)
  %16 = load %struct.Env*, %struct.Env** %4, align 8
  %17 = getelementptr inbounds %struct.Env, %struct.Env* %16, i32 0, i32 2
  %18 = load %struct.Sphere*, %struct.Sphere** %17, align 8
  %19 = load %struct.Env*, %struct.Env** %4, align 8
  %20 = getelementptr inbounds %struct.Env, %struct.Env* %19, i32 0, i32 0
  %21 = load %struct.Vec*, %struct.Vec** %20, align 8
  %22 = load %struct.Ray*, %struct.Ray** %5, align 8
  %23 = load %struct.Isect*, %struct.Isect** %6, align 8
  %24 = call i32 @Sphere_intersect(%struct.Sphere* %18, %struct.Vec* %21, %struct.Ray* %22, %struct.Isect* %23)
  %25 = load %struct.Env*, %struct.Env** %4, align 8
  %26 = getelementptr inbounds %struct.Env, %struct.Env* %25, i32 0, i32 3
  %27 = load %struct.Sphere*, %struct.Sphere** %26, align 8
  %28 = load %struct.Env*, %struct.Env** %4, align 8
  %29 = getelementptr inbounds %struct.Env, %struct.Env* %28, i32 0, i32 0
  %30 = load %struct.Vec*, %struct.Vec** %29, align 8
  %31 = load %struct.Ray*, %struct.Ray** %5, align 8
  %32 = load %struct.Isect*, %struct.Isect** %6, align 8
  %33 = call i32 @Sphere_intersect(%struct.Sphere* %27, %struct.Vec* %30, %struct.Ray* %31, %struct.Isect* %32)
  %34 = load %struct.Env*, %struct.Env** %4, align 8
  %35 = getelementptr inbounds %struct.Env, %struct.Env* %34, i32 0, i32 4
  %36 = load %struct.Plane*, %struct.Plane** %35, align 8
  %37 = load %struct.Env*, %struct.Env** %4, align 8
  %38 = getelementptr inbounds %struct.Env, %struct.Env* %37, i32 0, i32 0
  %39 = load %struct.Vec*, %struct.Vec** %38, align 8
  %40 = load %struct.Ray*, %struct.Ray** %5, align 8
  %41 = load %struct.Isect*, %struct.Isect** %6, align 8
  %42 = call i32 @Plane_intersect(%struct.Plane* %36, %struct.Vec* %39, %struct.Ray* %40, %struct.Isect* %41)
  ret i32 0
}

; Function Attrs: noinline nounwind optnone uwtable
define %struct.Env* @Env_new() #0 {
  %1 = alloca %struct.Env*, align 8
  %2 = call i8* @malloc(i64 40)
  %3 = bitcast i8* %2 to %struct.Env*
  store %struct.Env* %3, %struct.Env** %1, align 8
  %4 = call %struct.Vec* @Vec_new(double 5.770000e-01, double 5.770000e-01, double 5.770000e-01)
  %5 = load %struct.Env*, %struct.Env** %1, align 8
  %6 = getelementptr inbounds %struct.Env, %struct.Env* %5, i32 0, i32 0
  store %struct.Vec* %4, %struct.Vec** %6, align 8
  %7 = call double @sin(double 0.000000e+00) #4
  %8 = call %struct.Vec* @Vec_new(double 0.000000e+00, double -5.000000e-01, double %7)
  %9 = call %struct.Vec* @Vec_new(double 1.000000e+00, double 0.000000e+00, double 0.000000e+00)
  %10 = call %struct.Sphere* @Sphere_new(double 5.000000e-01, %struct.Vec* %8, %struct.Vec* %9)
  %11 = load %struct.Env*, %struct.Env** %1, align 8
  %12 = getelementptr inbounds %struct.Env, %struct.Env* %11, i32 0, i32 1
  store %struct.Sphere* %10, %struct.Sphere** %12, align 8
  %13 = call double @cos(double 6.660000e+00) #4
  %14 = call %struct.Vec* @Vec_new(double 2.000000e+00, double 0.000000e+00, double %13)
  %15 = call %struct.Vec* @Vec_new(double 0.000000e+00, double 1.000000e+00, double 0.000000e+00)
  %16 = call %struct.Sphere* @Sphere_new(double 1.000000e+00, %struct.Vec* %14, %struct.Vec* %15)
  %17 = load %struct.Env*, %struct.Env** %1, align 8
  %18 = getelementptr inbounds %struct.Env, %struct.Env* %17, i32 0, i32 2
  store %struct.Sphere* %16, %struct.Sphere** %18, align 8
  %19 = call double @cos(double 3.330000e+00) #4
  %20 = call %struct.Vec* @Vec_new(double -2.000000e+00, double 5.000000e-01, double %19)
  %21 = call %struct.Vec* @Vec_new(double 0.000000e+00, double 0.000000e+00, double 1.000000e+00)
  %22 = call %struct.Sphere* @Sphere_new(double 1.500000e+00, %struct.Vec* %20, %struct.Vec* %21)
  %23 = load %struct.Env*, %struct.Env** %1, align 8
  %24 = getelementptr inbounds %struct.Env, %struct.Env* %23, i32 0, i32 3
  store %struct.Sphere* %22, %struct.Sphere** %24, align 8
  %25 = call %struct.Vec* @Vec_new(double 0.000000e+00, double -1.000000e+00, double 0.000000e+00)
  %26 = call %struct.Vec* @Vec_new(double 0.000000e+00, double 1.000000e+00, double 0.000000e+00)
  %27 = call %struct.Vec* @Vec_new(double 1.000000e+00, double 1.000000e+00, double 1.000000e+00)
  %28 = call %struct.Plane* @Plane_new(%struct.Vec* %25, %struct.Vec* %26, %struct.Vec* %27)
  %29 = load %struct.Env*, %struct.Env** %1, align 8
  %30 = getelementptr inbounds %struct.Env, %struct.Env* %29, i32 0, i32 4
  store %struct.Plane* %28, %struct.Plane** %30, align 8
  %31 = load %struct.Env*, %struct.Env** %1, align 8
  ret %struct.Env* %31
}

; Function Attrs: nounwind
declare double @sin(double) #2

; Function Attrs: nounwind
declare double @cos(double) #2

; Function Attrs: noinline nounwind optnone uwtable
define i32 @color_of(double) #0 {
  %2 = alloca i32, align 4
  %3 = alloca double, align 8
  %4 = alloca i32, align 4
  store double %0, double* %3, align 8
  %5 = load double, double* %3, align 8
  %6 = call double @clamp(double %5, double 0.000000e+00, double 1.000000e+00)
  %7 = fmul double 2.560000e+02, %6
  %8 = fptosi double %7 to i32
  store i32 %8, i32* %4, align 4
  %9 = load i32, i32* %4, align 4
  %10 = icmp eq i32 %9, 256
  br i1 %10, label %11, label %12

; <label>:11:                                     ; preds = %1
  store i32 255, i32* %2, align 4
  br label %14

; <label>:12:                                     ; preds = %1
  %13 = load i32, i32* %4, align 4
  store i32 %13, i32* %2, align 4
  br label %14

; <label>:14:                                     ; preds = %12, %11
  %15 = load i32, i32* %2, align 4
  ret i32 %15
}

; Function Attrs: noinline nounwind optnone uwtable
define void @print_col(%struct.Vec*) #0 {
  %2 = alloca %struct.Vec*, align 8
  store %struct.Vec* %0, %struct.Vec** %2, align 8
  %3 = load %struct.Vec*, %struct.Vec** %2, align 8
  %4 = getelementptr inbounds %struct.Vec, %struct.Vec* %3, i32 0, i32 0
  %5 = load double, double* %4, align 8
  %6 = call i32 @color_of(double %5)
  %7 = load %struct.Vec*, %struct.Vec** %2, align 8
  %8 = getelementptr inbounds %struct.Vec, %struct.Vec* %7, i32 0, i32 1
  %9 = load double, double* %8, align 8
  %10 = call i32 @color_of(double %9)
  %11 = load %struct.Vec*, %struct.Vec** %2, align 8
  %12 = getelementptr inbounds %struct.Vec, %struct.Vec* %11, i32 0, i32 2
  %13 = load double, double* %12, align 8
  %14 = call i32 @color_of(double %13)
  %15 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([10 x i8], [10 x i8]* @.str, i32 0, i32 0), i32 %6, i32 %10, i32 %14)
  ret void
}

declare i32 @printf(i8*, ...) #1

; Function Attrs: noinline nounwind optnone uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca %struct.Env*, align 8
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca double, align 8
  %6 = alloca double, align 8
  %7 = alloca %struct.Ray*, align 8
  %8 = alloca %struct.Isect*, align 8
  %9 = alloca %struct.Vec*, align 8
  %10 = alloca %struct.Vec*, align 8
  %11 = alloca i32, align 4
  %12 = alloca %struct.Ray*, align 8
  store i32 0, i32* %1, align 4
  %13 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([14 x i8], [14 x i8]* @.str.1, i32 0, i32 0), i32 200, i32 200)
  %14 = call %struct.Env* @Env_new()
  store %struct.Env* %14, %struct.Env** %2, align 8
  store i32 0, i32* %3, align 4
  br label %15

; <label>:15:                                     ; preds = %127, %0
  %16 = load i32, i32* %3, align 4
  %17 = icmp slt i32 %16, 300
  br i1 %17, label %18, label %130

; <label>:18:                                     ; preds = %15
  store i32 0, i32* %4, align 4
  br label %19

; <label>:19:                                     ; preds = %124, %18
  %20 = load i32, i32* %4, align 4
  %21 = icmp slt i32 %20, 300
  br i1 %21, label %22, label %127

; <label>:22:                                     ; preds = %19
  %23 = load i32, i32* %4, align 4
  %24 = sitofp i32 %23 to double
  %25 = fdiv double %24, 1.500000e+02
  %26 = fsub double %25, 1.000000e+00
  store double %26, double* %5, align 8
  %27 = load i32, i32* %3, align 4
  %28 = sub nsw i32 300, %27
  %29 = sitofp i32 %28 to double
  %30 = fdiv double %29, 1.500000e+02
  %31 = fsub double %30, 1.000000e+00
  store double %31, double* %6, align 8
  %32 = call %struct.Vec* @Vec_new(double 0.000000e+00, double 2.000000e+00, double 6.000000e+00)
  %33 = load double, double* %5, align 8
  %34 = load double, double* %6, align 8
  %35 = call %struct.Vec* @Vec_new(double %33, double %34, double -1.000000e+00)
  %36 = call %struct.Vec* @Vec_normalize(%struct.Vec* %35)
  %37 = call %struct.Ray* @Ray_new(%struct.Vec* %32, %struct.Vec* %36)
  store %struct.Ray* %37, %struct.Ray** %7, align 8
  %38 = call %struct.Vec* @Vec_new(double 0.000000e+00, double 0.000000e+00, double 0.000000e+00)
  %39 = call %struct.Vec* @Vec_new(double 0.000000e+00, double 0.000000e+00, double 0.000000e+00)
  %40 = call %struct.Vec* @Vec_new(double 0.000000e+00, double 0.000000e+00, double 0.000000e+00)
  %41 = call %struct.Vec* @Vec_new(double 0.000000e+00, double 0.000000e+00, double 0.000000e+00)
  %42 = call %struct.Isect* @Isect_new(i32 0, %struct.Vec* %38, %struct.Vec* %39, %struct.Vec* %40, double 1.000000e+07, %struct.Vec* %41)
  store %struct.Isect* %42, %struct.Isect** %8, align 8
  %43 = load %struct.Env*, %struct.Env** %2, align 8
  %44 = load %struct.Ray*, %struct.Ray** %7, align 8
  %45 = load %struct.Isect*, %struct.Isect** %8, align 8
  %46 = call i32 @Env_intersect(%struct.Env* %43, %struct.Ray* %44, %struct.Isect* %45)
  %47 = load %struct.Isect*, %struct.Isect** %8, align 8
  %48 = getelementptr inbounds %struct.Isect, %struct.Isect* %47, i32 0, i32 0
  %49 = load i32, i32* %48, align 8
  %50 = icmp slt i32 0, %49
  br i1 %50, label %51, label %107

; <label>:51:                                     ; preds = %22
  %52 = load %struct.Isect*, %struct.Isect** %8, align 8
  %53 = getelementptr inbounds %struct.Isect, %struct.Isect* %52, i32 0, i32 3
  %54 = load %struct.Vec*, %struct.Vec** %53, align 8
  store %struct.Vec* %54, %struct.Vec** %9, align 8
  %55 = call %struct.Vec* @Vec_new(double 1.000000e+00, double 1.000000e+00, double 1.000000e+00)
  %56 = load %struct.Isect*, %struct.Isect** %8, align 8
  %57 = getelementptr inbounds %struct.Isect, %struct.Isect* %56, i32 0, i32 3
  %58 = load %struct.Vec*, %struct.Vec** %57, align 8
  %59 = call %struct.Vec* @Vec_multi(%struct.Vec* %55, %struct.Vec* %58)
  store %struct.Vec* %59, %struct.Vec** %10, align 8
  store i32 1, i32* %11, align 4
  br label %60

; <label>:60:                                     ; preds = %102, %51
  %61 = load i32, i32* %11, align 4
  %62 = icmp slt i32 %61, 4
  br i1 %62, label %63, label %105

; <label>:63:                                     ; preds = %60
  %64 = load %struct.Isect*, %struct.Isect** %8, align 8
  %65 = getelementptr inbounds %struct.Isect, %struct.Isect* %64, i32 0, i32 1
  %66 = load %struct.Vec*, %struct.Vec** %65, align 8
  %67 = load %struct.Isect*, %struct.Isect** %8, align 8
  %68 = getelementptr inbounds %struct.Isect, %struct.Isect* %67, i32 0, i32 2
  %69 = load %struct.Vec*, %struct.Vec** %68, align 8
  %70 = call %struct.Vec* @Vec_mul(%struct.Vec* %69, double 1.000000e-04)
  %71 = call %struct.Vec* @Vec_add(%struct.Vec* %66, %struct.Vec* %70)
  %72 = load %struct.Isect*, %struct.Isect** %8, align 8
  %73 = getelementptr inbounds %struct.Isect, %struct.Isect* %72, i32 0, i32 5
  %74 = load %struct.Vec*, %struct.Vec** %73, align 8
  %75 = load %struct.Isect*, %struct.Isect** %8, align 8
  %76 = getelementptr inbounds %struct.Isect, %struct.Isect* %75, i32 0, i32 2
  %77 = load %struct.Vec*, %struct.Vec** %76, align 8
  %78 = call %struct.Vec* @Vec_reflect(%struct.Vec* %74, %struct.Vec* %77)
  %79 = call %struct.Ray* @Ray_new(%struct.Vec* %71, %struct.Vec* %78)
  store %struct.Ray* %79, %struct.Ray** %12, align 8
  %80 = load %struct.Env*, %struct.Env** %2, align 8
  %81 = load %struct.Ray*, %struct.Ray** %12, align 8
  %82 = load %struct.Isect*, %struct.Isect** %8, align 8
  %83 = call i32 @Env_intersect(%struct.Env* %80, %struct.Ray* %81, %struct.Isect* %82)
  %84 = load i32, i32* %11, align 4
  %85 = load %struct.Isect*, %struct.Isect** %8, align 8
  %86 = getelementptr inbounds %struct.Isect, %struct.Isect* %85, i32 0, i32 0
  %87 = load i32, i32* %86, align 8
  %88 = icmp slt i32 %84, %87
  br i1 %88, label %89, label %102

; <label>:89:                                     ; preds = %63
  %90 = load %struct.Vec*, %struct.Vec** %9, align 8
  %91 = load %struct.Vec*, %struct.Vec** %10, align 8
  %92 = load %struct.Isect*, %struct.Isect** %8, align 8
  %93 = getelementptr inbounds %struct.Isect, %struct.Isect* %92, i32 0, i32 3
  %94 = load %struct.Vec*, %struct.Vec** %93, align 8
  %95 = call %struct.Vec* @Vec_multi(%struct.Vec* %91, %struct.Vec* %94)
  %96 = call %struct.Vec* @Vec_add(%struct.Vec* %90, %struct.Vec* %95)
  store %struct.Vec* %96, %struct.Vec** %9, align 8
  %97 = load %struct.Vec*, %struct.Vec** %10, align 8
  %98 = load %struct.Isect*, %struct.Isect** %8, align 8
  %99 = getelementptr inbounds %struct.Isect, %struct.Isect* %98, i32 0, i32 3
  %100 = load %struct.Vec*, %struct.Vec** %99, align 8
  %101 = call %struct.Vec* @Vec_multi(%struct.Vec* %97, %struct.Vec* %100)
  store %struct.Vec* %101, %struct.Vec** %10, align 8
  br label %102

; <label>:102:                                    ; preds = %89, %63
  %103 = load i32, i32* %11, align 4
  %104 = add nsw i32 %103, 1
  store i32 %104, i32* %11, align 4
  br label %60

; <label>:105:                                    ; preds = %60
  %106 = load %struct.Vec*, %struct.Vec** %9, align 8
  call void @print_col(%struct.Vec* %106)
  br label %124

; <label>:107:                                    ; preds = %22
  %108 = load %struct.Ray*, %struct.Ray** %7, align 8
  %109 = getelementptr inbounds %struct.Ray, %struct.Ray* %108, i32 0, i32 1
  %110 = load %struct.Vec*, %struct.Vec** %109, align 8
  %111 = getelementptr inbounds %struct.Vec, %struct.Vec* %110, i32 0, i32 1
  %112 = load double, double* %111, align 8
  %113 = load %struct.Ray*, %struct.Ray** %7, align 8
  %114 = getelementptr inbounds %struct.Ray, %struct.Ray* %113, i32 0, i32 1
  %115 = load %struct.Vec*, %struct.Vec** %114, align 8
  %116 = getelementptr inbounds %struct.Vec, %struct.Vec* %115, i32 0, i32 1
  %117 = load double, double* %116, align 8
  %118 = load %struct.Ray*, %struct.Ray** %7, align 8
  %119 = getelementptr inbounds %struct.Ray, %struct.Ray* %118, i32 0, i32 1
  %120 = load %struct.Vec*, %struct.Vec** %119, align 8
  %121 = getelementptr inbounds %struct.Vec, %struct.Vec* %120, i32 0, i32 1
  %122 = load double, double* %121, align 8
  %123 = call %struct.Vec* @Vec_new(double %112, double %117, double %122)
  call void @print_col(%struct.Vec* %123)
  br label %124

; <label>:124:                                    ; preds = %107, %105
  %125 = load i32, i32* %4, align 4
  %126 = add nsw i32 %125, 1
  store i32 %126, i32* %4, align 4
  br label %19

; <label>:127:                                    ; preds = %19
  %128 = load i32, i32* %3, align 4
  %129 = add nsw i32 %128, 1
  store i32 %129, i32* %3, align 4
  br label %15

; <label>:130:                                    ; preds = %15
  %131 = load i32, i32* %1, align 4
  ret i32 %131
}

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind readnone speculatable }
attributes #4 = { nounwind }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 6.0.0-1ubuntu2 (tags/RELEASE_600/final)"}
