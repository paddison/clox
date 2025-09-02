cmake_minimum_required(VERSION 3.10)

project(clox)

set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED True)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_GENERATOR Ninja)

add_executable(clox main.c chunk.c debug.c lines.c memory.c value.c myalloc.c vm.c scanner.c compiler.c object.c)
