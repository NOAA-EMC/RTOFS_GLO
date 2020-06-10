#ifndef _CDF_H
#define _CDF_H

void cdfDebug(int debug);

const char *cdfLibraryVersion(void);
const char *hdfLibraryVersion(void);

int  cdfOpen(const char *filename, const char *mode);
int  cdfOpen64(const char *filename, const char *mode);
int  cdf4Open(const char *filename, const char *mode);
void cdfClose(int fileID);

#endif  /* _CDF_H */
