cmake_minimum_required(VERSION 3.9)
project (RandomTimeTransformaion VERSION 1.0)
configure_file(RandomTimeTransformaionConfig.h.in RandomTimeTransformaionConfig.h)
add_executable(RandomTimeTransformaion RandomTimeTransformaion.cpp)
