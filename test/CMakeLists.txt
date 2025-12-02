message(STATUS "Building tests")

# Automatically find all test files matching test_*.f90
file(GLOB TEST_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/test_*.f90")

# Build each test
foreach(test_file ${TEST_SOURCES})
  # Extract the filename without extension
  get_filename_component(test_name ${test_file} NAME_WE)

  message(STATUS "Found test: ${test_name}")

  add_executable(${test_name} ${test_file})

  # Specify the include directories
  target_include_directories(${test_name} PRIVATE "${CMAKE_BUILD_PREFIX}" "${CMAKE_BUILD_PREFIX}/mod")

  target_link_libraries(${test_name} PRIVATE coreutils)

  add_test(NAME ${test_name} COMMAND ${test_name})
endforeach()
