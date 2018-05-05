#include "awfm.h"

double Factorial(int n)
{
  return dfac_(&n);
}

double K0(double x)
{
    return dbesk0_(&x);
}

double W(double u)
{
  return (u > 100) ? 0 : de1_(&u);
}

// bool float_compare(double v1, double v2, double tolerance) {
//     if (fabs(v1 - v2) < tolerance) {
//         return true;
//     } else {
//         return false;
//     }
// }
//
// int sign(double v) {
//     if (float_compare(v, 0, 1e-6)) {
//         return 0;
//     } else if (v < 0) {
//         return -1;
//     } else { // v > 0
//         return 1;
//     }
// }
//
// int timeseries_average(double *ts_in, double *vs_in, int size_in,
//                         double *t_out, double *v_out) {
//
//     if (size_in == 0) {
//         return 0;
//     }
//
//     double sum = 0;
//     for (int i = 0; i < size_in; i++) {
//         sum += vs_in[i];
//     }
//
//     *t_out = ts_in[0];
//     *v_out = sum / size_in;
//
//     return 1;
// }
//
// int timeseries_average_by_sign(double *ts_in, double *vs_in, int size_in,
//                     double *ts_out, double *vs_out) {
//
//     if (size_in <= 1) {
//         *ts_out = *ts_in;
//         *vs_out = *vs_in;
//         return size_in;
//     }
//
//     int current_sign = sign(vs_in[0]);
//
// }
