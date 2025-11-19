cmake_policy(SET CMP0048 NEW)
cmake_minimum_required(VERSION 3.9)

# Set policies to avoid warnings and compatibility issues
if(POLICY CMP0135)
    cmake_policy(SET CMP0135 NEW)
endif()

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

set(PROJECT_NAME "sclang-format")

project(${PROJECT_NAME} VERSION 0.0.1 LANGUAGES C CXX)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED)

if(EXISTS "${CMAKE_SOURCE_DIR}/.git")
    execute_process(
        COMMAND git rev-parse --abbrev-ref HEAD
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        OUTPUT_VARIABLE GIT_BRANCH
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    execute_process(
        COMMAND git log -1 --format=%h
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        OUTPUT_VARIABLE GIT_COMMIT_HASH
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
else()
    set(GIT_BRANCH "not_a_git_checkout")
    set(GIT_COMMIT_HASH "na")
endif()

message(STATUS "${PROJECT_NAME} v${PROJECT_VERSION}, ${GIT_BRANCH}@${GIT_COMMIT_HASH}")

# Use system ANTLR4 instead of building from source (Windows only)
if(WIN32)
    find_package(PkgConfig QUIET)
    if(PkgConfig_FOUND)
        pkg_check_modules(ANTLR4 antlr4-runtime)
        if(ANTLR4_FOUND)
            set(USE_SYSTEM_ANTLR4 ON CACHE BOOL "Use system ANTLR4" FORCE)
        endif()
    endif()
endif()

# Disable ANTLR4 C++ tests to avoid googletest dependency
set(ANTLR_BUILD_CPP_TESTS OFF CACHE BOOL "Build C++ tests" FORCE)
set(ENV{ANTLR_BUILD_CPP_TESTS} "OFF")

add_subdirectory(external/sparkler)
add_subdirectory(src)

