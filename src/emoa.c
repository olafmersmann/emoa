#include "extern.h"

static const R_CallMethodDef callMethods[] = {
	{"do_crowding_distance", (DL_FUNC) &do_crowding_distance, 1},
	{"do_dominance_matrix", (DL_FUNC) &do_dominance_matrix, 1},
	{"do_dominated_hypervolume", (DL_FUNC) &do_dominated_hypervolume, 2},
	{"do_eps_ind", (DL_FUNC) &do_eps_ind, 2},
	{"do_hv_contrib", (DL_FUNC) &do_hv_contrib, 2},
	{"do_is_dominated", (DL_FUNC) &do_is_dominated, 1},
	{"do_pm", (DL_FUNC) &do_pm, 5},
	{"do_r_ind", (DL_FUNC) &do_r_ind, 5},
	{"do_sbx", (DL_FUNC) &do_sbx, 5},
	{"do_sympart", (DL_FUNC) &do_sympart, 1},
	{"do_unary_r2_ind", (DL_FUNC) &do_unary_r2_ind, 3},
	{"do_which_points_on_edge", (DL_FUNC) &do_which_points_on_edge, 1},
	{"nondominated_order", (DL_FUNC) &nondominated_order, 2}
};

void R_init_mco(DllInfo *info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, TRUE);
}
