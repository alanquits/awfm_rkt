#ifndef AWFM_C_H
#define AWFM_C_H

#include <math.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif
    double dfac_(int*);
    double dbesk0_(double*);
    double de1_(double*);
#ifdef __cplusplus
}
#endif

double Factorial(int n);
double K0(double x);
double W(double u);

// // Averages a timeseries
// // returns size of input array.
// int timeseries_average(double *ts_in, double *vs_in, int size_in,
//                         double *t_out, double *v_out);
//
// // ts_in, vs_in: original times and values
// // size_in: size of ts_in and ts_out
// // ts_out, vs_out: reduced data
// // returns: size of reduced data
// int timeseries_average_by_sign(double *ts_in, double *vs_in, int size_in,
//                     double *ts_out, double *vs_out);
//
// bool float_compare(double v1, double v2, double tolerance);
//
// // Returns 1, 0, or -1 based on sign of input value
// int sign(double v);

#endif // AWFM_C_H
