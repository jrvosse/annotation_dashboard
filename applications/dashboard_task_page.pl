:- module(an_dashboard_taskpage, []).

% from SWI-Prolog libraries:
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(pairs)).
:- use_module(library(settings)).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).

% from ClioPatria:
:- use_module(library(semweb/rdf_label)).
:- use_module(cliopatria(hooks)).
:- use_module(user(user_db)).

% from other cpacks:
:- use_module(library(yui3_beta)). % needed for html resource declarations
:- use_module(library(oa_schema)).
:- use_module(library(oa_annotation)).
:- use_module(library(ac_list_util)).
:- use_module(api(media_caching)).       % needed for http api handlers
:- use_module(applications(annotation)).

% from this pack:
:- use_module(api(dashboard_api)).
:- use_module(library(dashboard_util)).
% :- use_module(library(region_merge)).
:- use_module(components(dashboard/task_stats)).
:- use_module(components(dashboard/show_objects)).
:- use_module(components(dashboard/top_nav_bar)).
:- use_module(components(dashboard/pagination)).

:- http_handler(cliopatria(annotate/dashboard/task), http_dashboard_task, []).

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


http_dashboard_task(Request) :-
	(setting(annotation:dashboard_admin_only, true)
	-> authorized(admin(dashboard)); true),
	http_parameters(Request,
			[task(Task, []),
			 limit(Limit, [nonneg, default(5)]),
			 offset(Offset, [nonneg, default(0)])
			]),

	task_page(Task, [showTag(always), limit(Limit), offset(Offset)]).

task_page(Task, Options0) :-
	option(limit(Limit), Options0, 5),
	option(offset(Offset), Options0, 0),
	rdf_display_label(Task, Label),
	rdf_has(Task, ann_ui:taskUI, UI),
	get_metafields(UI, [], MetadataFields),
	get_anfields(UI, [], [], AnnotationFields),
	% AnnotationFields = [ 'http://eculture.cs.vu.nl/sealinc/ns/demo/ui/CommonBirdNameAnnotation'],
	find_annotations_by_task(Task, Annotations),
	partition(is_tag, Annotations, Tags, Judgements),
	maplist(rdf_get_annotation_target, Tags, RawTargets),
	sort(RawTargets, AllTargets), % de-dup
	TargetsWithImages = AllTargets,
	% include(target_has_image, AllTargets, TargetsWithImages),
	% list_offset(['http://purl.org/collections/nl/rma/collection/r-108494'|AllTargets], Offset, OffTargets),
	list_offset(TargetsWithImages, Offset, OffTargets),
	list_limit(OffTargets, Limit, Objects, _Rest),
	length(TargetsWithImages, Total),
	maplist(count_annotations, Objects, CountPairs),
	sort(CountPairs, SortedPairs0),
	reverse(SortedPairs0, ReversePairs),
	pairs_values(ReversePairs, SortedObjects),
	Options = [annotations(Annotations),
		   judgements(Judgements),
		   annotation_fields(AnnotationFields),
		   metadata_fields(MetadataFields),
		   ui(UI),
		   task(Task),
		   lazy(true),
		   limit(Limit),
		   offset(Offset),
		   total(Total),
		   image_link_predicate(http_mediumscale) |
		   Options0
		  ],
	reply_html_page(
	    [ title(Label),
	      meta([name(viewport),
		   content('width=device-width, initial-scale=1')]),
	      \html_requires(dashboard)
	    ],
	    [ \top_navbar,
	      div([class('container-fluid')],
		  [
		    div([class(row)],
			[ div([class('col-sm-12 main')],
			      [ h1([class('page-header')], ['Dashboard']),
				h2([class('sub-header')],
				   [Label]),
				div([class(row)], \task_stats(Task)),
				h3([class('sub-header')],
				   ['Task objects']),
				\show_objects(SortedObjects, Options),
				\pagination(Options)
			      ])
			])
		  ])
	    ]).
