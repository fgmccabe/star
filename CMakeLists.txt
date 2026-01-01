cmake_minimum_required(VERSION 3.20)
project(Star)

include(CPack)
include(InstallRequiredSystemLibraries)

set(PROJECT_VERSION_MAJOR 1)
set(PROJECT_VERSION_MINOR 0)

set(version "${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}")

add_subdirectory("src")
add_subdirectory("Docs")

set(CPACK_PACKAGE_NAME "star")
set(CPACK_PACKAGE_VENDOR "Francis G. McCabe")
set(CPACK_PACKAGE_VERSION_MAJOR ${PROJECT_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${PROJECT_VERSION_MINOR})

add_custom_target(dist
  COMMAND cpack --C Release)


