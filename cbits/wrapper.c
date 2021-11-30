#include <hdf5.h>
#include <stdlib.h>

__attribute__((constructor, visibility("default"))) void hdf5_hs_init(void) {
  fprintf(stderr, "Running init...\n");
  herr_t const status = H5Eset_auto(H5E_DEFAULT, NULL, NULL);
  if (status < 0) {
    fprintf(stderr,
            "Fatal error: could not initialize hdf5-hs package. H5Eset_auto "
            "failed with error code %i. Aborting...\n",
            status);
    abort();
  }
}
