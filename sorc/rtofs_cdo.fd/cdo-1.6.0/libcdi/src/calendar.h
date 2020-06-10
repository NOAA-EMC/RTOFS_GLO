#ifndef _CALENDAR_H
#define _CALENDAR_H

void encode_caldaysec(int calendar, int year, int month, int day, int hour, int minute, int second,
		      int *julday, int *secofday);
void decode_caldaysec(int calendar, int julday, int secofday, 
		      int *year, int *month, int *day, int *hour, int *minute, int *second);

#endif  /* _CALENDAR_H */
