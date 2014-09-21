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
	  option(task(Task), Options),
	  Max is floor(Total/Limit),
	  numlist(0,Max, List1),
	  maplist(multiply(Limit), List1, List)
	},
	html(ul([class(pagination)],[\pitem(Limit, Offset,Task, List)])).

multiply(A,B,P) :- P is A * B.
pitem(_,_,_, []) --> !.
pitem(Limit, Offset, Task, [H1|T]) -->
	{ ( T = [H2|_], H1 =< Offset, Offset < H2
	  ->  Class = [active]
	  ;   Class = [])
	},
	html(li([class(Class)],
		a([class(pagination), task(Task),
		   offset(H1), limit(Limit)
		  ],
		  H1))),
	pitem(Limit, Offset, Task, T).
