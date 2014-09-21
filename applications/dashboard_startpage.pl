:- module(an_dashboard_startpage, []).

% from SWI-Prolog libraries:
:- use_module(library(settings)).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).

% from ClioPatria:
:- use_module(cliopatria(hooks)).
:- use_module(user(user_db)).

% from other cpacks:
:- use_module(library(yui3_beta)). % needed for html resource declarations
:- use_module(library(oa_schema)).
:- use_module(api(media_caching)).       % needed for http api handlers

% from this pack:
:- use_module(api(dashboard_api)).
:- use_module(library(dashboard_util)).
:- use_module(components(dashboard/show_annotations)).
:- use_module(components(dashboard/show_tasks)).
:- use_module(components(dashboard/show_users)).
:- use_module(components(dashboard/top_nav_bar)).

:- http_handler(cliopatria(annotate/dashboard/home), http_dashboard_home, []).

:- setting(annotation:dashboard_admin_only, boolean, true,
	   'Dashboard only for users with admin rights').

cliopatria:menu_item(100=annotation/http_dashboard_home, 'dashboard').

:- html_resource(dashboard,
		 [ virtual(true),
		   ordered(true),
		   requires([bootstrap,
			     css('deniche-dashboard.css'),
			     css('deniche-tags.css'),
			     yui3('yui/yui-min.js'),
			     js('dashboard.js')
			    ])
		 ]).
:- html_resource(task_stats,
		 [ virtual(true),
		   requires([d3js,
			    css('task_stats.css')])
		 ]).
:- html_resource(d3js,
		 [ virtual(true),
		   requires(['http://d3js.org/d3.v3.min.js'])
		 ]).

:- html_resource(bootstrap,
	      [ virtual(true),
		requires(
		    [ '//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css'
		    ])
	      ]).


http_dashboard_home(_Request) :-
	(setting(annotation:dashboard_admin_only, true)
	-> authorized(admin(dashboard)); true),
	dashboard_page([]).

dashboard_page(_Options) :-
	find_tasks(Tasks),
	find_workers(Users),
	find_annotations_without_task(TaskLess),
	length(TaskLess, NrTaskless),
	length(Users, NrOfUsers),
	reply_html_page(
	    [ title('Annotation dashboard'),
	      meta([name(viewport),
		    content('width=device-width, initial-scale=1')],
		   []),
	      \html_requires(dashboard)
	    ],
	    [ \top_navbar,
	      div([class('container-fluid')],
		  [ div([class(row)],
			[ div([class('col-sm-12 main')],
			      [ h1([class('page-header')], ['Dashboard']),
				\show_tasks(Tasks),
				h2([class('sub-header')],
				   ['Total annotations not associated with a defined task: ', NrTaskless]),
				div([class('table-responsive')],
				    [table([class('table table-striped')],
					   [\show_annotations(TaskLess, [])
					   ])
				    ]),
				h2([id(leaderboard), class('sub-header')],
				   ['Leaderboard: #', NrOfUsers, ' annotators']),
				div([class('table-responsive')],
				    [table([class('table table-striped')],
					   [ thead([
						 tr([th('User rank'),
						     th('Name'),
						     th('Number of annotations')])
					     ]),
					     tbody([
						 \show_users(Users, 1)
					     ])
					   ])
				    ])
			      ])
			])
		  ])
	    ]).
