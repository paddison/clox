cmake_minimum_required(VERSION 3.10)

project(clox)

set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED True)

add_executable(clox main.c chunk.c debug.c lines.c memory.c value.c)
