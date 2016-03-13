; ModuleID = 'library.ll'

define double @"unary.-"(double %x) {
entry:
  %0 = fsub double 0.0, %x
  ret double %0
}

define double @"binary.+"(double %x, double %y) {
entry:
  %0 = fadd double %x, %y
  ret double %0
}

define double @"binary.-"(double %x, double %y) {
entry:
  %0 = fsub double %x, %y
  ret double %0
}

define double @"binary.*"(double %x, double %y) {
entry:
  %0 = fmul double %x, %y
  ret double %0
}

define double @"binary./"(double %x, double %y) {
entry:
  %0 = fdiv double %x, %y
  ret double %0
}
