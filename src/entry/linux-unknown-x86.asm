bits 32

section .text
  global _start
  extern main

_start:
  xor eax, eax  ; clear eax
  call main     ; call main
  mov edi, eax  ; save return value (if main returns (), eax will stay 0)
  mov eax, 60   ; 60 = exit
  syscall       ; exit