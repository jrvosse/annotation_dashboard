:- module(an_dashboard_components_pagination,
	  [ pagination//1
	  ]).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(http/html_write)).

pagination(Options) -->
	{ option(limit(Limit), Options),
	  option(offset(Offset), Options),
	  option(total(Total), Options),
	  Max is floor(Total/Limit),
	  numlist(0,Max, List1),
	  maplist(multiply(Limit), List1, List)
	},
	html(ul([class(pagination)],[\pitem(Offset, List)])).

multiply(A,B,P) :- P is A * B.
pitem(_, []) --> !.
pitem(Offset, [H1|T]) -->
	{ ( T = [H2|_], between(H1, H2, Offset)
	  ->  Class = [active]
	  ;   Class = [])
	},
	html(li([class(Class)],a([href('#')],H1))),
	pitem(Offset,T).
