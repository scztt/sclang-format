add_executable(sclang-format sclang-format.cpp)
if(WIN32)
    set_property(TARGET sclang-format PROPERTY
        MSVC_RUNTIME_LIBRARY "MultiThreadedDLL"
    )
endif()

target_link_libraries(sclang-format sprklr)
