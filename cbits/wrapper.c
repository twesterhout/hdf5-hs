#include "wrapper.h"
#include <pthread.h>

herr_t wrapper_h5o_get_type(hid_t object, H5O_type_t *out) {
#if H5_VERS_MAJOR == 1 &&                                                      \
    (H5_VERS_MINOR < 10 || (H5_VERS_MINOR == 10 && H5_VERS_RELEASE <= 3))
  H5O_info_t info;
  herr_t status = H5Oget_info(object, &info);
#elif H5_VERS_MAJOR == 1 && H5_VERS_MINOR == 10 && H5_VERS_RELEASE > 3
  H5O_info_t info;
  herr_t status = H5Oget_info2(object, &info, H5O_INFO_BASIC);
#else
  H5O_info2_t info;
  herr_t status = H5Oget_info3(object, &info, H5O_INFO_BASIC);
#endif

  if (status >= 0) {
    *out = info.type;
  }
  return status;
}

static pthread_once_t globals_is_initialized = PTHREAD_ONCE_INIT;
static wrapper_constants globals;
void initialize_globals(void) {
  globals.c_H5P_DEFAULT = H5P_DEFAULT;
  globals.c_H5T_VARIABLE = H5T_VARIABLE;
  globals.c_H5S_ALL = H5S_ALL;
  globals.c_H5F_ACC_RDONLY = H5F_ACC_RDONLY;
  globals.c_H5F_ACC_TRUNC = H5F_ACC_TRUNC;
  globals.c_H5F_ACC_RDWR = H5F_ACC_RDWR;
  globals.c_H5F_ACC_EXCL = H5F_ACC_EXCL;
}

wrapper_constants const *wrapper_get_constants(void) {
  (void)pthread_once(&globals_is_initialized, initialize_globals);
  return &globals;
}
