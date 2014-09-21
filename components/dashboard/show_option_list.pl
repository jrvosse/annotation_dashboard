:- module(an_dashboard_components_show_option_list,
	  [ show_option_list//1
	  ]).

:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(components(label)).

show_option_list([]) --> !.
show_option_list([Prop|Tail]) -->
	{ (   is_dict(Prop)
	  ->  V = Prop.'@value',
	      Key = Prop.label
	  ;   (   Prop = K-V
	      ;	    Prop =.. [K,V]),
		    (   rdf_current_predicate(K)
		    ->  Key = \rdf_link(K)
		    ;   Key = K
		    )
	  ),
	  (   rdf_is_resource(V)
	  ->  (   rdf_subject(V)
	      ->  Value = \rdf_link(V)
	      ;	   (  rdf_global_id(_:Value, V)
		   ->  true
		   ;   Value = V
		   )
	      )
	  ;   (   rdf_is_literal(V)
	      ->  literal_text(V, Value)
	      ;   V = Value
	      )
	  )
	},
	html(tr([td(Key), td(Value)])),
	show_option_list(Tail).

show_option_list(Dict) -->
	{ is_dict(Dict),
	  dict_pairs(Dict, _Tag, Pairs)
	},
	show_option_list(Pairs).

