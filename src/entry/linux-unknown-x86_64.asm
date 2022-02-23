bits 64

section .text
  global _start
  extern main

_start:
  xor rax, rax  ; clear rax
  call main     ; call main
  mov rdi, rax  ; save return value (if main returns (), rax will stay 0)
  mov rax, 60   ; 60 = exit
  syscall       ; exit