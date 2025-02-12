cmake_minimum_required(VERSION 3.10)
project(x3d2 LANGUAGES Fortran CXX C)
enable_testing()

# option to enable ADIOS2 - default is OFF
option(ENABLE_ADIOS2 "Enable ADIOS2" OFF)

set(WITH_2DECOMPFFT ON CACHE BOOL
  "Enable Poisson based FFT solver on the OpenMP backend.")

# add directory containing custom find module
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/adios2")

if(ENABLE_ADIOS2)
  cmake_policy(SET CMP0074 NEW)
  find_package(ADIOS2 QUIET)
  if(NOT ADIOS2_FOUND)
    message(FATAL_ERROR "ADIOS2 not found!")
  endif()
else()
  message(STATUS "ADIOS2 is disabled")
endif()

add_subdirectory(src)
add_subdirectory(tests)