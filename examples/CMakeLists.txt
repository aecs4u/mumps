# MUMPS Examples and Benchmarks

# ============================================================================
# Simple Test Programs
# ============================================================================

foreach(PREC ${PRECISIONS})
    # Simple test
    add_executable(${PREC}simpletest ${PREC}simpletest.F)
    target_link_libraries(${PREC}simpletest ${PREC}mumps mumps_common)

    # C example
    if(PREC STREQUAL "d")
        add_executable(c_example c_example.c)
        target_link_libraries(c_example dmumps mumps_common)
    endif()
endforeach()

# ============================================================================
# Benchmark Programs
# ============================================================================

if(BUILD_BENCHMARKS)
    foreach(PREC ${PRECISIONS})
        # MUMPS benchmark (sparse)
        add_executable(${PREC}mumps_bench ${PREC}mumps_bench.c)
        target_link_libraries(${PREC}mumps_bench ${PREC}mumps mumps_common)

        # GEMM benchmark (dense)
        add_executable(${PREC}gemm_bench ${PREC}gemm_bench.F90)
        target_link_libraries(${PREC}gemm_bench ${BLAS_LIBRARIES} ${LAPACK_LIBRARIES})
    endforeach()
endif()

# ============================================================================
# Installation
# ============================================================================

foreach(PREC ${PRECISIONS})
    install(TARGETS ${PREC}simpletest
        RUNTIME DESTINATION bin
    )

    if(BUILD_BENCHMARKS)
        install(TARGETS ${PREC}mumps_bench ${PREC}gemm_bench
            RUNTIME DESTINATION bin
        )
    endif()
endforeach()

if(TARGET c_example)
    install(TARGETS c_example RUNTIME DESTINATION bin)
endif()
