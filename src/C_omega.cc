
#include <omega.h>
#include <string.h>

#include "C_omega.h"

extern "C"
Relation *hsw_new_relation(int n_input, int n_output)
{
  return new Relation(n_input, n_output);
}

extern "C"
Relation *hsw_new_set(int n)
{
  return new Relation(n);
}

extern "C"
void hsw_free_relation(Relation *rel)
{
  delete rel;
}

extern "C"
char *hsw_relation_show(Relation *rel)
{
  return strdup((const char *)rel->print_with_subs_to_string());
}

extern "C"
int hsw_num_input_vars(Relation *rel)
{
  return rel->n_inp();
}

extern "C"
int hsw_num_output_vars(Relation *rel)
{
  return rel->n_out();
}

extern "C"
int hsw_num_set_vars(Relation *rel)
{
  return rel->n_set();
}

extern "C"
Var_Decl *hsw_input_var(Relation *rel, int n)
{
  return rel->input_var(n);
}

extern "C"
Var_Decl *hsw_output_var(Relation *rel, int n)
{
  return rel->output_var(n);
}
extern "C"
Var_Decl *hsw_set_var(Relation *rel, int n)
{
  return rel->set_var(n);
}

extern "C"
int hsw_is_lower_bound_satisfiable(Relation *rel)
{
  return rel->is_lower_bound_satisfiable();
}

extern "C"
int hsw_is_upper_bound_satisfiable(Relation *rel)
{
  return rel->is_upper_bound_satisfiable();
}

extern "C"
int hsw_is_obvious_tautology(Relation *rel)
{
  return rel->is_obvious_tautology();
}
extern "C"
int hsw_is_definite_tautology(Relation *rel)
{
  return rel->is_tautology();
}

extern "C"
int hsw_is_exact(Relation *rel)
{
  return rel->is_exact();
}

extern "C"
int hsw_is_inexact(Relation *rel)
{
  return rel->is_inexact();
}

extern "C"
int hsw_is_unknown(Relation *rel)
{
  return rel->is_unknown();
}

extern "C"
Relation *hsw_upper_bound(Relation *rel)
{
  return new Relation(Upper_Bound(copy(*rel)));
}

extern "C"
Relation *hsw_lower_bound(Relation *rel)
{
  return new Relation(Lower_Bound(copy(*rel)));
}

extern "C"
int hsw_equal(Relation *r, Relation *s)
{
  /*   r == s
   * iff
   *    r `intersection` not s == False
   * && r `union` not s        == True
   */
  Relation com_s = Complement(copy(*s));

  /* If intersection is satisfiable, unequal */
  if (Intersection(copy(*r), copy(com_s)).is_upper_bound_satisfiable())
    return 0;

  /* If union is tautology, equal; else unequal */
  return Union(copy(*r), com_s).is_tautology();
}

extern "C"
Relation *hsw_union(Relation *r, Relation *s)
{
  return new Relation(Union(copy(*r), copy(*s)));
}

extern "C"
Relation *hsw_intersection(Relation *r, Relation *s)
{
  return new Relation(Intersection(copy(*r), copy(*s)));
}

extern "C"
Relation *hsw_composition(Relation *r, Relation *s)
{
  return new Relation(Composition(copy(*r), copy(*s)));
}

extern "C"
Relation *hsw_restrict_domain(Relation *r, Relation *s)
{
  return new Relation(Restrict_Domain(copy(*r), copy(*s)));
}

extern "C"
Relation *hsw_restrict_range(Relation *r, Relation *s)
{
  return new Relation(Restrict_Range(copy(*r), copy(*s)));
}

extern "C"
Relation *hsw_difference(Relation *r, Relation *s)
{
  return new Relation(Difference(copy(*r), copy(*s)));
}

extern "C"
Relation *hsw_cross_product(Relation *r, Relation *s)
{
  return new Relation(Cross_Product(copy(*r), copy(*s)));
}

extern "C"
Relation *hsw_gist(Relation *r, Relation *s, int effort)
{
  return new Relation(Gist(copy(*r), copy(*s), effort));
}

extern "C"
Relation *hsw_transitive_closure(Relation *rel)
{
  return new Relation(TransitiveClosure(copy(*rel)));
}

extern "C"
Relation *hsw_domain(Relation *rel)
{
  return new Relation(Domain(copy(*rel)));
}

extern "C"
Relation *hsw_range(Relation *rel)
{
  return new Relation(Range(copy(*rel)));
}

extern "C"
Relation *hsw_inverse(Relation *rel)
{
  return new Relation(Inverse(copy(*rel)));
}

extern "C"
Relation *hsw_complement(Relation *rel)
{
  return new Relation(Complement(copy(*rel)));
}

extern "C"
Relation *hsw_deltas(Relation *rel)
{
  return new Relation(Deltas(copy(*rel)));
}

extern "C"
Relation *hsw_approximate(Relation *rel)
{
  return new Relation(Approximate(copy(*rel)));
}

extern "C"
F_And *hsw_relation_add_and(Relation *rel)
{
  return rel->add_and();
}

extern "C"
Formula *hsw_relation_add_or(Relation *rel)
{
  return rel->add_or();
}

extern "C"
Formula *hsw_relation_add_not(Relation *rel)
{
  return rel->add_not();
}

extern "C"
F_Declaration *hsw_relation_add_forall(Relation *rel)
{
  return rel->add_forall();
}

extern "C"
F_Declaration *hsw_relation_add_exists(Relation *rel)
{
  return rel->add_exists();
}

extern "C"
void hsw_relation_finalize(Relation *rel)
{
  rel->finalize();
}

extern "C"
Var_Decl *hsw_declaration_declare(F_Declaration *rel)
{
  return rel->declare();
}

extern "C"
F_And *hsw_formula_to_and(Formula *rel)
{
  F_And *and_formula = dynamic_cast<F_And *>(rel);

  /* If the parameter is already an 'and', return it */
  if (and_formula) return and_formula;

  /* Otherwise add an 'and' */
  return rel->add_and();
}

extern "C"
F_And *hsw_formula_add_and(Formula *rel)
{
  return rel->add_and();
}

extern "C"
Formula *hsw_formula_add_or(Formula *rel)
{
  return rel->add_or();
}

extern "C"
Formula *hsw_formula_add_not(Formula *rel)
{
  return rel->add_not();
}

extern "C"
F_Declaration *hsw_formula_add_forall(Formula *rel)
{
  return rel->add_forall();
}

extern "C"
F_Declaration *hsw_formula_add_exists(Formula *rel)
{
  return rel->add_exists();
}

extern "C"
void hsw_formula_finalize(Formula *rel)
{
  rel->finalize();
}

/* hsw_add_constraint creates an equality or inequality constraint,
 * fills in the coefficients for each variable, and fills in the
 * constant term. */
extern "C"
void hsw_add_constraint(F_And *formula,
		    int is_eq,
		    int num_vars,
		    int *coefficients,
		    Var_Decl **vars,
		    int constant)
{
  Constraint_Handle *hdl = is_eq
    ? (Constraint_Handle *)new EQ_Handle(formula->add_EQ())
    : (Constraint_Handle *)new GEQ_Handle(formula->add_GEQ());

  /* Update each coefficient in the array */
  for (; num_vars; num_vars--)
    {
      int index = num_vars - 1;
      hdl->update_coef(vars[index], coefficients[index]);
    }

  /* Update the constant part of the constraint */
  hdl->update_const(constant);

  hdl->finalize();
  free(hdl);
}

/* These are all for inspecting a DNF formula */

extern "C"
DNF_Iterator *hsw_query_dnf(Relation *rel)
{
  return new DNF_Iterator(rel->query_DNF());
}

extern "C"
Conjunct *hsw_dnf_iterator_next(DNF_Iterator *iter)
{
  if (!iter->live()) return NULL;

  Conjunct *c = **iter;
  ++*iter;
  return c;
}

extern "C"
void hsw_dnf_iterator_free(DNF_Iterator *iter)
{
  delete iter;
}

/* Use to iterate over the tuple of the variables that are used in the
 * conjunct.  The variables obtained should not be freed. */
extern "C"
struct Tuple_Iter *hsw_get_conjunct_variables(Conjunct *conj)
{
  Tuple_Iterator<void *> *ti =
    reinterpret_cast<Tuple_Iterator<void *> *>
    (new Tuple_Iterator<Variable_ID>(*conj->variables()));
  return (struct Tuple_Iter *)ti;
}

extern "C"
void *
hsw_tuple_iterator_next(struct Tuple_Iter *iter)
{
  Tuple_Iterator<void *> *ti = (Tuple_Iterator<void *> *)iter;

  if (!ti->live()) return NULL;	// Exhausted?

  void *ret = (void *)**ti;
  ++*ti;
  return ret;
}

extern "C"
void
hsw_tuple_iterator_free(struct Tuple_Iter *iter)
{
  delete (Tuple_Iterator<void *> *)iter;
}

/* Use to iterate over the EQ constraints in a conjunct.  The constraints
 * obtained should be freed once you're done with them. */
extern "C"
struct EQ_Iterator *
hsw_get_eqs(Conjunct *conj)
{
  return new EQ_Iterator(conj->EQs());
}

extern "C"
struct EQ_Handle *
hsw_eqs_next(struct EQ_Iterator *g)
{
  if (!g->live()) return NULL;	// Exhausted?

  EQ_Handle *hdl = new EQ_Handle(**g);
  ++*g;
  return hdl;
}

extern "C"
void
hsw_eqs_free(struct EQ_Iterator *g)
{
  delete g;
}

extern "C"
void
hsw_eq_handle_free(struct EQ_Handle *hdl)
{
  delete hdl;
}

/* Use to iterate over the GEQ constraints in a conjunct.  Works like
 * hsw_get_eqs. */
extern "C"
struct GEQ_Iterator *hsw_get_geqs(Conjunct *conj)
{
  return new GEQ_Iterator(conj->GEQs());
}

extern "C"
struct GEQ_Handle *
hsw_geqs_next(struct GEQ_Iterator *g)
{
  if (!g->live()) return NULL;	// Exhausted?

  GEQ_Handle *hdl = new GEQ_Handle(**g);
  ++*g;
  return hdl;
}

extern "C"
void
hsw_geqs_free(struct GEQ_Iterator *g)
{
  delete g;
}

extern "C"
void
hsw_geq_handle_free(struct GEQ_Handle *hdl)
{
  delete hdl;
}

extern "C"
coefficient_t
hsw_constraint_get_const(struct Constraint_Handle_ *hdl)
{
  return ((struct Constraint_Handle *)hdl)->get_const();
}

extern "C"
Constr_Vars_Iter *
hsw_constraint_get_coefficients(struct Constraint_Handle_ *hdl)
{
  return new Constr_Vars_Iter(*(Constraint_Handle *)hdl);  
}

extern "C"
int
hsw_constr_vars_next(Variable_Info_struct *out, Constr_Vars_Iter *iter)
{
  if (!iter->live()) return 0;

  Variable_Info info(**iter);
  ++*iter;

  out->var = info.var;
  out->coef = info.coef;

  return 1;
}

extern "C"
void
hsw_constr_vars_free(Constr_Vars_Iter *iter)
{
  delete iter;
}

/* For debugging */

extern "C"
void
hsw_debug_print_eq(struct EQ_Handle *hdl)
{
  String s(hdl->print_to_string());
  puts(s);
}

extern "C"
void
hsw_debug_print_geq(struct GEQ_Handle *hdl)
{
  String s(hdl->print_to_string());
  puts(s);
}

#if 0 /* Not used? */

/* Find an array element equal to v.  Return the element index,
 * or -1 if no element matches. */
static int
find_variable_index(Var_Decl *v, int num_vars, Var_Decl **vars)
{
  int n;
  for (n = 0; n < num_vars; n++) {
    if (v == vars[n]) return n;
  }
  return -1;
}
#endif
