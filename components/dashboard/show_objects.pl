:- module(an_dashboard_components_show_objects,
	  [ show_objects//2
	  ]).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(oa_annotation)).
:- use_module(applications(annotation)).
:- use_module(components(dashboard/show_annotations)).
:- use_module(components(dashboard/show_option_list)).
:- use_module(library(dashboard_util)).

show_objects(Targets,Options) -->
	{ true,
	  option(task(Task), Options),
	  rdf_has(Task, ann_ui:taskUI, UI),
	  get_metafields(UI, [], MetadataFields),
	  get_anfields(UI, [], [], AnnotationFields),
	  NewOptions = [
	      ui(UI),
	      metadata_fields(MetadataFields),
	      annotation_fields(AnnotationFields) |
	      Options
	  ]
	},
	html(
	    [div([class(row)],
		 [div([class('col-xs-6')],
		      [\annotation_page_body([targets(Targets)|NewOptions])
		      ])
		 ])
	    ]).

match_target(T,A) :-
	rdf_get_annotation_target(A,T).

show_object_old(O, Options) -->
	{ option(annotations(A), Options, []),
	  include(match_target(O), A, Annotations),
	  (   ( option(metadata_fields(_), Options),
		option(ui(_), Options),
		option(annotation_fields(_), Options))
	  ->  NewOptions = Options
	  ;   ( option(task(Task), Options)
	      ->  true
	      ;	  member(First, A),
		  guess_task(First, Task-First)
	      ),
	      rdf_has(Task, ann_ui:taskUI, UI),
	      get_metafields(UI, [], MetadataFields),
	      get_anfields(UI, [], [], AnnotationFields),
	      NewOptions = [
		  ui(UI),
		  metadata_fields(MetadataFields),
		  annotation_fields(AnnotationFields) |
		  Options
	      ]
	  )
	},
	html(
	    [div([class(row)],
		 [div([class('col-xs-6')],
		      [\annotation_page_body([target(O)|NewOptions])
		      ]),
		  div([class('col-xs-6 table-responsive')],
		      [ table([class('table table-striped')], [
				  \show_annotations(Annotations, NewOptions)
			      ])
		      ])
		 ])
	    ]).
