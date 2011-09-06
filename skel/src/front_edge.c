#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <R_ext/Applic.h>

#include "sexp_macros.h"

#ifndef MIN
#define MIN(A, B) ((A < B) ? A : B)
#endif

SEXP do_which_points_on_edge(SEXP s_front) {
    size_t point, other_point, objective;
    SEXP s_result;
    UNPACK_REAL_MATRIX(s_front, front, number_of_objectives, number_of_points);
    
    PROTECT(s_result = allocVector(LGLSXP, number_of_points));
    int *result = LOGICAL(s_result);
    
    /* Temprary storage for two reference points. Automatically freed
     * on return. 
     */
    double *base_reference_point = (double *)R_alloc(number_of_objectives, 
                                                     sizeof(double));
    double *current_reference_point = (double *)R_alloc(number_of_objectives, 
                                                        sizeof(double));
    
    /* Find reference point: */
    for (objective = 0; objective < number_of_objectives; ++objective) {
        base_reference_point[objective] = R_NegInf;
        for (point = 0; point < number_of_points; ++point) {
            /* Move up/right/back by 1 to make sure reference point is
             * really dominated:
             */
            const double value = front[point * number_of_objectives + objective] + 1;
            if (value > base_reference_point[objective]) {
                base_reference_point[objective] = value;
            }
        }
    }

    /* Iterate over all points and... */
    for (point = 0; point < number_of_points; ++point) {
        double *point_values = front + point * number_of_objectives;

        /* All points start out as points on the edge: */
        result[point] = FALSE;

        /* Initialize reference point: */
        for (objective = 0; objective < number_of_objectives; ++objective)
            current_reference_point[objective] = base_reference_point[objective] ;

        /* Shrink bounding box w.r.t reference point: */
        for (other_point = 0; other_point < number_of_points; ++other_point) {
            double *other_point_values = front + other_point * number_of_objectives;
            size_t number_of_dominated_objectives = 0;
            size_t last_dominated_objective;
            for (objective = 0; objective < number_of_objectives; ++objective) {
                const double this_value = point_values[objective];
                const double other_value = other_point_values[objective];
                if (this_value < other_value) {
                    ++number_of_dominated_objectives;
                    last_dominated_objective = objective;
                    if (number_of_dominated_objectives > 1)
                        break;
                }
            }
            if (number_of_dominated_objectives == 1) {
                current_reference_point[last_dominated_objective] = 
                    MIN(current_reference_point[last_dominated_objective],
                        other_point_values[last_dominated_objective]);
            }
        }
        
        /* If bounding box still depends on reference point, then the
         * point is on the edge of the front: 
         */
        for (objective = 0; objective < number_of_objectives; ++objective) {
            if (current_reference_point[objective] == base_reference_point[objective]) {
                result[point] = TRUE;
                break;
            }
        }
    }
    UNPROTECT(1); /* s_result */
    return s_result;
}
