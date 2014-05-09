:- module(an_dashboard, []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(oa_schema)).

:- use_module(user(user_db)).
:- use_module(cliopatria(hooks)).

:- http_handler(cliopatria(annotate/dashboard/home), http_dashboard_home, []).
:- http_handler(cliopatria(annotate/dashboard/user), http_dashboard_user, []).

:- setting(annotation:dashboard_admin_only, boolean, true,
	   'Dashboard only for users with admin rights').

cliopatria:menu_item(100=annotation/http_dashboard_home, 'dashboard').

:- html_resource(bootstrap,
	      [ virtual(true),
		requires(
		    [ '//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css'
		    ])
	      ]).

:- multifile
	show_user_annotations//3.

http_dashboard_user(Request) :-
	(setting(annotation:dashboard_admin_only, true)
	-> authorized(admin(dashboard)); true),
	http_parameters(Request, [user(User, [])]),
	user_page(User, []).

http_dashboard_home(_Request) :-
	(setting(annotation:dashboard_admin_only, true)
	-> authorized(admin(dashboard)); true),
	dashboard_page([]).


user_page(User, _Options) :-
	findall(Prop, user_property(User, Prop), Props),
	reply_html_page(
	    [title(User),
	      meta([name(viewport),
		    content('width=device-width, initial-scale=1')],
		   []),
	     \html_requires(
		   [bootstrap,
		    css('dashboard.css')
		   ])
	    ],
	    [ \top_navbar,
	      div([class('container-fluid')],
		  [ div([class(row)],
			[ div([class('col-sm-3 col-md-2 sidebar')],
			      [nav_to_be_done]),
			  div([class('col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main')],
			      [ h1([class('page-header')], ['Dashboard']),
				h2([class('sub-header')],
				   ['User information']),
				div([class('table-responsive')],
				    [table([class('table table-striped')],
					   [ \show_user_props(Props)
					   ])
				    ]),
				h2([class('sub-header')],
				   ['Annotations made so far']),
				to_be_done
			      ])
			])
		  ])
	    ]).


dashboard_page(_Options) :-
	find_users(Users),
	length(Users, NrOfUsers),
	reply_html_page(
	    [ title('Annotation dashboard'),
	      meta([name(viewport),
		    content('width=device-width, initial-scale=1')],
		   []),
	      \html_requires(
		   [bootstrap,
		    css('dashboard.css')
		   ])
	    ],
	    [ \top_navbar,
	      div([class('container-fluid')],
		  [ div([class(row)],
			[ div([class('col-sm-3 col-md-2 sidebar')],
			      [nav_to_be_done]),
			  div([class('col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main')],
			      [ h1([class('page-header')], ['Dashboard']),
				h2([class('sub-header')],
				   ['Total number of users so far: ', NrOfUsers]),
				div([class('table-responsive')],
				    [table([class('table table-striped')],
					   [ thead([
						 tr([th('User id'), th('Number of annotations')])
					     ]),
					     tbody([
						 \show_users(Users)
					     ])
					   ])
				    ])
			      ])
			])
		  ])
	    ]).

top_navbar -->
	html(
	    div([class('navbar navbar-inverse navbar-fixed-top'),
		 role('navigation')],
		[nav_to_be_done])
	).

show_users([]) --> !.
show_users([U|T]) -->
	show_user(U),
	show_users(T).

show_user(U) -->
	{
	 option(id(Uid), U),
	 option(done(Done), U),
	 http_link_to_id(http_dashboard_user, [user(Uid)], UserLink)
	},
	html(tr([td(a([href(UserLink)],['~p'-Uid])),
		 td([class='an_nr_of_annotations'],Done)])).


find_users(Users) :-
	findall(User, participant(User), Users0),
	sort(Users0, Users).

participant(User) :-
	current_user(Uid),
	user_property(Uid, url(URL)),
	find_annotations(URL, Annotations),
	length(Annotations, Done),
	User= [
	       id(Uid), url(URL), done(Done)
	      ].


find_annotations(User, Annotations) :-
	findall(A, annotation_by_user(User, A), Anns0),
	maplist(ann_time, Anns0, APairs),
	group_pairs_by_key(APairs, AGrouped),
	keysort(AGrouped, AGroupedS),
	pairs_values(AGroupedS, Values),
	append(Values, Annotations).

ann_time(Ann, Time-Ann) :-
	rdf(Ann, oa:annotated, TimeLit),
	literal_text(TimeLit, Time).

annotation_by_user(User, Annotation) :-
	rdf(Annotation, oa:annotator, User).


show_user_props([]) --> !.

show_user_props([connection(_,_)|Tail]) -->
	show_user_props(Tail).
show_user_props([allow(_)|Tail]) -->
	show_user_props(Tail).
show_user_props([password(_)|Tail]) -->
	show_user_props(Tail).

show_user_props([Prop|Tail]) --> {
    Prop =.. [K,V]
},
	html(tr([td(K), td(V)])),
	show_user_props(Tail).

show_annotations(User, A, D) --> show_user_annotations(User, A, D),!.
show_annotations(_, [], []) -->
	html(div([class('warning no_annotations')],['no annotations yet'])).
