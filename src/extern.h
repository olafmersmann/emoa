	
#ifndef EMOA_EXTERN_H
#define EMOA_EXTERN_H
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>

SEXP do_crowding_distance(SEXP s_front);
SEXP do_dominance_matrix(SEXP s_points);
SEXP do_dominated_hypervolume(SEXP s_data, SEXP s_ref);
SEXP do_eps_ind(SEXP s_data, SEXP s_ref);
SEXP do_hv_contrib(SEXP s_data, SEXP s_ref);
SEXP do_is_dominated(SEXP s_points);
SEXP do_pm(SEXP s_x, SEXP s_lb, SEXP s_ub, SEXP s_eta, SEXP s_p);
SEXP do_r_ind(SEXP s_data, SEXP s_ideal, SEXP s_nadir, SEXP s_lambda, SEXP s_method);
SEXP do_sbx(SEXP s_parents, SEXP s_lb, SEXP s_ub, SEXP s_eta, SEXP s_p);
SEXP do_sympart(SEXP s_x);
SEXP do_unary_r2_ind(SEXP s_data, SEXP s_weights, SEXP s_ideal);
SEXP do_which_points_on_edge(SEXP s_front);
SEXP nondominated_order(SEXP s_points, SEXP s_tosort);
#endif
