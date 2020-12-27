#include "wrapper.h"

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
