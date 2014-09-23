:- module(an_dashboard_components_show_objects,
	  [ show_objects//2
	  ]).

:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(http/html_write)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(oa_annotation)).
:- use_module(applications(annotation)).
:- use_module(components(dashboard/show_annotations)).

show_objects(Targets,Options) -->
	{ get_annotion_options(Options, NewOptions)
	},
	html(
	    [div([class(row)],
		 [div([],
		      [\annotation_page_body([targets(Targets),
					      media_class('col-xs-6')|NewOptions])
		      ])
		 ])
	    ]).

get_annotion_options(Options, NewOptions) :-
	option(task(Task), Options), !,
	rdf_has(Task, ann_ui:taskUI, UI),
	get_metafields(UI, [], MetadataFields),
	get_anfields(UI, [], [], AnnotationFields),
	ExtraOptions = [
	    ui(UI),
	    metadata_fields(MetadataFields),
	    annotation_fields(AnnotationFields)
	],
	merge_options(ExtraOptions, Options, NewOptions).

get_annotion_options(Options, NewOptions) :-
	merge_options([annotation_fields(dynamic),
		       lazy(true)
		      ],
		      Options, NewOptions).


match_target(T,A) :-
	rdf_get_annotation_target(A,T).

image_annotation:html_application_target_info(Options) -->
	{ get_annotion_options(Options, NewOptions),
	  option(target(Target), Options),
	  option(annotations(A), Options, []),
	  include(match_target(Target), A, Annotations)
	},
	html(
	    [div([class(dashboard)],
		  [ div([class('col-xs-6 table-responsive')],
		      [ table([class('table table-striped')], [
				  \show_annotations(Annotations, NewOptions)
			      ])
		      ])
		 ])
	    ]).
