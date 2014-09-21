:- module(an_dashboard_user_page, []).

% from SWI-Prolog libraries:
:- use_module(library(apply)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).

% from ClioPatria:
:- use_module(cliopatria(hooks)).
:- use_module(user(user_db)).

% from other cpacks:
:- use_module(library(oa_schema)).
:- use_module(library(oa_annotation)).
:- use_module(api(media_caching)).       % needed for http api handlers

% from this pack:
:- use_module(api(dashboard_api)).
:- use_module(library(dashboard_util)).
:- use_module(components(dashboard/top_nav_bar)).
:- use_module(components(dashboard/show_objects)).
:- use_module(components(dashboard/show_option_list)).

:- http_handler(cliopatria(annotate/dashboard/user), http_dashboard_user, []).

http_dashboard_user(Request) :-
	(setting(annotation:dashboard_admin_only, true)
	-> authorized(admin(dashboard)); true),
	http_parameters(Request, [user(User, [])]),
	user_page(User, []).

user_page(User, Options0) :-
	findall(Prop, user_property(User, Prop), Props),
	find_annotations_by_user(User, Annotations),
	partition(is_tag, Annotations, Tags, Judgements),
	maplist(rdf_get_annotation_target, Tags, Targets),
	sort(Targets, Objects),
	Options = [annotations(Annotations),
		   judgements(Judgements),
		   lazy(true),
		   user(User),
		   showTag(mine),
		   image_link_predicate(http_mediumscale) |
		   Options0
		  ],
	reply_html_page(
	    [title(User),
	     meta([name(viewport),
		    content('width=device-width, initial-scale=1')],
		   []),
	     \html_requires(dashboard)
	    ],
	    [ \top_navbar,
	      div([class('container-fluid')],
		  [ div([class(row)],
			[ div([class('col-sm-12 main')
			      ],
			      [ h1([class('page-header')], ['Dashboard']),
				h2([class('sub-header')],
				   ['User information']),
				div([class('table-responsive')],
				    [table([class('table table-striped')],
					   [ \show_option_list(Props)
					   ])
				    ]),
				h2([class('sub-header')],
				   ['Annotations made so far']),
				\show_objects(Objects, Options)
			      ])
			])
		  ])
	    ]).
