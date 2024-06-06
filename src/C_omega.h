
#ifndef C_OMEGA_H
#define C_OMEGA_H

#ifdef __cplusplus
extern "C" {
#endif

/* This is a copy of 'coef_t'.  Can't use the original because it's in
 * a C++ header file. */
typedef long long coefficient_t;

/* This is a copy of struct Variable_Info.  Can't use the original because
 * it's in a C++ header file. */
typedef struct Variable_Info_struct {
  struct Var_Decl *var;
  coefficient_t    coef;
} Variable_Info_struct;

struct Relation *hsw_new_relation(int n_input, int n_output);
struct Relation *hsw_new_set(int n);
void hsw_free_relation(struct Relation *rel);
char *hsw_relation_show(struct Relation *rel);
int hsw_num_input_vars(struct Relation *rel);
int hsw_num_output_vars(struct Relation *rel);
int hsw_num_set_vars(struct Relation *rel);
struct Var_Decl *hsw_input_var(struct Relation *rel, int n);
struct Var_Decl *hsw_output_var(struct Relation *rel, int n);
struct Var_Decl *hsw_set_var(struct Relation *rel, int n);
int hsw_is_lower_bound_satisfiable(struct Relation *rel);
int hsw_is_upper_bound_satisfiable(struct Relation *rel);
int hsw_is_obvious_tautology(struct Relation *rel);
int hsw_is_definite_tautology(struct Relation *rel);
int hsw_is_exact(struct Relation *rel);
int hsw_is_inexact(struct Relation *rel);
int hsw_is_unknown(struct Relation *rel);
struct Relation *hsw_upper_bound(struct Relation *);
struct Relation *hsw_lower_bound(struct Relation *);
int hsw_equal(struct Relation *, struct Relation *);
struct Relation *hsw_union(struct Relation *, struct Relation *);
struct Relation *hsw_intersection(struct Relation *, struct Relation *);
struct Relation *hsw_composition(struct Relation *, struct Relation *);
struct Relation *hsw_restrict_domain(struct Relation *, struct Relation *);
struct Relation *hsw_restrict_range(struct Relation *, struct Relation *);
struct Relation *hsw_difference(struct Relation *, struct Relation *);
struct Relation *hsw_cross_product(struct Relation *, struct Relation *);
struct Relation *hsw_gist(struct Relation *, struct Relation *, int);
struct Relation *hsw_transitive_closure(struct Relation *);
struct Relation *hsw_domain(struct Relation *);
struct Relation *hsw_range(struct Relation *);
struct Relation *hsw_inverse(struct Relation *);
struct Relation *hsw_complement(struct Relation *);
struct Relation *hsw_deltas(struct Relation *);
struct Relation *hsw_approximate(struct Relation *);

struct F_And *hsw_relation_add_and(struct Relation *rel);
struct Formula *hsw_relation_add_or(struct Relation *rel);
struct Formula *hsw_relation_add_not(struct Relation *rel);
struct F_Declaration *hsw_relation_add_forall(struct Relation *rel);
struct F_Declaration *hsw_relation_add_exists(struct Relation *rel);
void hsw_relation_finalize(struct Relation *rel);

struct F_And *hsw_formula_add_and(struct Formula *rel);
struct Formula *hsw_formula_add_or(struct Formula *rel);
struct Formula *hsw_formula_add_not(struct Formula *rel);
struct F_Declaration *hsw_formula_add_forall(struct Formula *rel);
struct F_Declaration *hsw_formula_add_exists(struct Formula *rel);
void hsw_formula_finalize(struct Formula *rel);

struct Var_Decl *hsw_declaration_declare(struct F_Declaration *rel);

struct F_And *hsw_formula_to_and(struct Formula *rel);

void hsw_add_constraint(struct F_And *formula,
		    int is_eq,
		    int num_vars,
		    int *coefficients,
		    struct Var_Decl **vars,
		    int constant);

struct DNF_Iterator *hsw_query_dnf(struct Relation *rel);
struct Conjunct *hsw_dnf_iterator_next(struct DNF_Iterator *iter);
void hsw_dnf_iterator_free(struct DNF_Iterator *iter);

struct Tuple_Iter *hsw_get_conjunct_variables(struct Conjunct *conj);
void *hsw_tuple_iterator_next(struct Tuple_Iter *iter);
void hsw_tuple_iterator_free(struct Tuple_Iter *iter);

struct EQ_Iterator *hsw_get_eqs(struct Conjunct *conj);
struct EQ_Handle *hsw_eqs_next(struct EQ_Iterator *g);
void hsw_eqs_free(struct EQ_Iterator *g);
void hsw_eq_handle_free(struct EQ_Handle *hdl);

struct GEQ_Iterator *hsw_get_geqs(struct Conjunct *conj);
struct GEQ_Handle *hsw_geqs_next(struct GEQ_Iterator *g);
void hsw_geqs_free(struct GEQ_Iterator *g);
void hsw_geq_handle_free(struct GEQ_Handle *hdl);

struct Constraint_Handle_;	/* Use a different name to get rid of C++ warning */
coefficient_t hsw_constraint_get_const(struct Constraint_Handle_ *hdl);
struct Constr_Vars_Iter *hsw_constraint_get_coefficients(struct Constraint_Handle_ *hdl);
int hsw_constr_vars_next(Variable_Info_struct *out, struct Constr_Vars_Iter *iter);
void hsw_constr_vars_free(struct Constr_Vars_Iter *iter);



void hsw_debug_print_eq(struct EQ_Handle *hdl);
void hsw_debug_print_geq(struct GEQ_Handle *hdl);


#ifdef __cplusplus
}
#endif

#endif
