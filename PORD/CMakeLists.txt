# PORD - Parallel Ordering Library

file(GLOB PORD_SOURCES "lib/*.c")

add_library(pord ${PORD_SOURCES})

target_include_directories(pord PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include/mumps/pord>
)

install(TARGETS pord
    EXPORT MUMPSTargets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
)

install(DIRECTORY include/ DESTINATION include/mumps/pord)
