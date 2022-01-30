cmake_minimum_required(VERSION 3.15)
project(compiler426)

set(CMAKE_CXX_STANDARD 20)

set(ANTLR4_TAG e4c1a74)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)
# required if linking to static library
add_definitions(-DANTLR4CPP_STATIC)
# add external build for antlrcpp
include(ExternalAntlr4Cpp)
# add antrl4cpp artifacts to project environment
include_directories(${ANTLR4_INCLUDE_DIRS})

# set variable pointing to the antlr tool that supports C++
# this is not required if the jar file can be found under PATH environment
set(ANTLR_EXECUTABLE ${PROJECT_SOURCE_DIR}/third_party/antlr-4.9.3-complete.jar)
# add macros to generate ANTLR Cpp code from grammar
find_package(ANTLR REQUIRED)


# Call macro to add lexer and grammar to your build dependencies.
antlr_target(SampleGrammar grammar426.g4 LEXER PARSER VISITOR)
# include generated files in project environment
include_directories(${ANTLR_SampleGrammar_OUTPUT_DIR})

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# Set your project compile flags.
# E.g. if using the C++ header files
# you will need to enable C++11 support
# for your compiler.

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# Find the libraries that correspond to the LLVM components
# that we wish to use
llvm_map_components_to_libnames(llvm_libs support core irreader)


# add generated grammar to demo binary target
add_executable(compiler426 src/main.cpp
        ${ANTLR_SampleGrammar_CXX_OUTPUTS})

target_link_libraries(compiler426 ${llvm_libs} antlr4_static)

