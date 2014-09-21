:- module(an_dashboard_components_show_users,
	  [ show_users//2
	  ]).

:- use_module(library(option)).
:- use_module(library(sgml)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).

show_users([], _) --> !.
show_users([U|T], Rank) -->
	{ Next is Rank + 1
	},
	show_user(U, Rank),
	show_users(T, Next).

show_user(U, Rank) -->
	{ option(id(Uid), U),
	  option(done(Done), U),
	  iri_xml_namespace(Uid, _, ScreenName),
	  http_link_to_id(http_dashboard_user, [user(Uid)], UserLink)
	},
	html(tr([td([class(rank)],[Rank]),
		 td(a([href(UserLink)],[ScreenName])),
		 td([class='an_nr_of_annotations'],Done)])).
