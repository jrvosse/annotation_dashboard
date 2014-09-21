:- module(an_dashboard_comp_top_nav_bar,
	  [ top_navbar//0
	  ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

top_navbar -->
	{ http_link_to_id(http_annotation, [], AnnotateLink)
	},
	html(
	    div([class('navbar navbar-inverse navbar-fixed-top'),
		 role('navigation')],
		[div([class('container-fluid')],
		     [div([class('navbar-header')],
			  [button([type('button'), class('navbar-toggle'),
				   'data-toggle'(collapse), 'data-target'('.navbar-collapse')],
				   [ span([class('sr-only')], 'Toggle navigation'),
				     span([class('icon-bar')],[]),
				     span([class('icon-bar')],[]),
				     span([class('icon-bar')],[])
				   ]),
			   a([class('navbar-brand'),
			      href('http://sealincmedia.wordpress.com/tag/accurator/')
			     ], ['Accurator for ',
				 span([class(role)],['Curator'])])
			  ]),
		      div(class('navbar-collapse collapse'),
			  [ ul(class('nav navbar-nav navbar-right'),
				[ li([a([href('home')],['Dashboard'])]),
				  li([a([href(AnnotateLink)],['Annotate'])]),
				  li([a([href('../../admin')],['Admin'])]),
				  li([a([href('../../user/logout')],['Logout'])])

				])
			  ])
		     ])
		])
	).
