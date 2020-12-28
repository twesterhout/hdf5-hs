#include <hdf5.h>

herr_t wrapper_h5o_get_type(hid_t object, H5O_type_t *out);

typedef struct wrapper_constants {
  hid_t c_H5P_DEFAULT;
  size_t c_H5T_VARIABLE;
  hid_t c_H5S_ALL;
  unsigned c_H5F_ACC_RDONLY;
  unsigned c_H5F_ACC_TRUNC;
  unsigned c_H5F_ACC_RDWR;
  unsigned c_H5F_ACC_EXCL;
} wrapper_constants;
wrapper_constants const *wrapper_get_constants(void);
