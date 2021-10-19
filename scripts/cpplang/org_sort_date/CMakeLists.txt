cmake_minimum_required(VERSION 3.15)
project(org_sort_date)

set(CMAKE_CXX_STANDARD 20)

add_executable(org_sort_date main.cpp)

##
include(FindPkgConfig)

set(Boost_USE_STATIC_LIBS OFF)
set(Boost_USE_MULTITHREADED ON)
set(Boost_USE_STATIC_RUNTIME OFF)

find_package(Boost REQUIRED COMPONENTS filesystem regex)
include_directories(${Boost_INCLUDE_DIRS})
target_link_libraries(org_sort_date ${Boost_LIBRARIES})
##
