# Try to find AIO
# Once done, this will define
#
# AIO_FOUND - system has AIO
# AIO_INCLUDE_DIR - the AIO include directories
# AIO_LIBRARIES - link these to use AIO

if(AIO_INCLUDE_DIR AND AIO_LIBRARIES)
    set(AIO_FIND_QUIETLY TRUE)
endif(AIO_INCLUDE_DIR AND AIO_LIBRARIES)

# include dir
find_path(AIO_INCLUDE_DIR aio.h)
message("AIO_INCLUDE = ${AIO_INCLUDE_DIR}")

# finally the library itself
if(APPLE)
  set(RTLIB c)
endif(APPLE)
find_library(LIBAIO_LIBRARIES NAMES aio rt ${RTLIB} c)

message("AIO_LIBRARIES = ${AIO_LIBRARIES}")

# handle the QUIETLY and REQUIRED arguments and set AIO_FOUND to TRUE if
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(AIO DEFAULT_MSG AIO_LIBRARIES AIO_INCLUDE_DIR)

mark_as_advanced(AIO_LIBRARIES AIO_INCLUDE_DIR)
