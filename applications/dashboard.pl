:- module(an_dashboard, []).

% from SWI-Prolog libraries:
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).

% from ClioPatria:
:- use_module(library(semweb/rdf_label)).
:- use_module(library(count)).
:- use_module(cliopatria(hooks)).
:- use_module(components(label)).
:- use_module(user(user_db)).

% from other cpacks:
:- use_module(library(yui3_beta)). % needed for html resource declarations
:- use_module(library(oa_schema)).
:- use_module(library(oa_annotation)).
:- use_module(api(media_caching)).       % needed for http api handlers
:- use_module(applications(annotation)).

:- http_handler(cliopatria(annotate/dashboard/home), http_dashboard_home, []).
:- http_handler(cliopatria(annotate/dashboard/user), http_dashboard_user, []).
:- http_handler(cliopatria(annotate/dashboard/task), http_dashboard_task, []).

:- setting(annotation:dashboard_admin_only, boolean, true,
	   'Dashboard only for users with admin rights').

cliopatria:menu_item(100=annotation/http_dashboard_home, 'dashboard').

:- html_resource(dashboard,
		 [ virtual(true),
		   ordered(true),
		   requires([bootstrap,
			     css('dashboard.css'),
			     css('deniche-tags.css'),
			     yui3('yui/yui-min.js'),
			     js('dashboard.js')
			    ])
		 ]).

:- html_resource(bootstrap,
	      [ virtual(true),
		requires(
		    [ '//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css'
		    ])
	      ]).
:- rdf_meta
	task_property_blacklist(t).

task_property_blacklist(
    [rdf:type,
     dcterms:title
    ]).

http_dashboard_user(Request) :-
	(setting(annotation:dashboard_admin_only, true)
	-> authorized(admin(dashboard)); true),
	http_parameters(Request, [user(User, [])]),
	user_page(User, []).

http_dashboard_home(_Request) :-
	(setting(annotation:dashboard_admin_only, true)
	-> authorized(admin(dashboard)); true),
	dashboard_page([]).

http_dashboard_task(Request) :-
	(setting(annotation:dashboard_admin_only, true)
	-> authorized(admin(dashboard)); true),
	http_parameters(Request, [task(Task, [])]),
	task_page(Task, []).

is_tag(A) :-
	rdf_has(A, oa:motivatedBy, oa:tagging).

is_tag(A) :-
	rdfs_individual_of(A, ann_ui:tag).

task_page(Task, _Options) :-
	rdf_display_label(Task, Label),
	find_annotations_by_task(Task, Annotations),
	partition(is_tag, Annotations, Tags, Judgements),
	maplist(rdf_get_annotation_target, Tags, Targets),
	sort(Targets, Objects),

	rdf_has(Task, ann_ui:taskUI, UI),
	get_metafields(UI, [], MetadataFields),
	get_anfields(UI, [], [], AnnotationFields),
	Options = [annotations(Annotations),
		   judgements(Judgements),
		   annotation_fields(AnnotationFields),
		   metadata_fields(MetadataFields),
		   ui(UI),
		   lazy(true)
		  ],
	reply_html_page(
	    [ title(Label),
	      meta([name(viewport),
		   content('width=device-width, initial-scale=1')]),
	      \html_requires(dashboard)
	    ],
	    [ \top_navbar,
	      div([class('container-fluid')],
		  [ div([class(row)],
			[ div([class('col-sm-12 main')],
			      [ h1([class('page-header')], ['Dashboard']),
				h2([class('sub-header')],
				   [Label]),
				h3([class('sub-header')],
				   ['Task objects']),
				%\show_objects(['http://purl.org/collections/nl/rma/collection/r-115055',
				%	       'http://purl.org/collections/nl/rma/collection/r-108494',
				%	       'http://purl.org/collections/nl/rma/collection/r-141319'],
				%	      Options)
				\show_objects(Objects, Options)
			      ])
			])
		  ])
	    ]).

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
			[ div([class('col-sm-12 main')
			      ],
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
	      \html_requires(
		   [bootstrap,
		    css('dashboard.css')
		   ])
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
						 tr([th('User id'), th('Number of annotations')])
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

top_navbar -->
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
			  ])
		     ])
		])
	).



show_objects([],_) --> !.
show_objects([H|T],Options) -->
	show_object(H,Options),
	show_objects(T,Options).


match_target(T,A) :-
	rdf_get_annotation_target(A,T).

show_object(O, Options) -->
	{ option(annotations(A), Options, []),
	  include(match_target(O), A, Annotations)
	},
	html(
	    [div([class(row)],
		 [div([class('col-xs-6')],
		      [\annotation_page_body([target(O)|Options])
		      ]),
		  div([class('col-xs-6 table-responsive')],
		      [ table([class('table table-striped')], [
				  \show_annotations(Annotations, Options)
			      ])
		      ])
		 ])
	    ]).

show_tasks([]) --> !.
show_tasks([H|T]) -->
	show_task(H),
	show_tasks(T).

show_task(Task) -->
	{ rdf_display_label(Task,Title),
	  http_link_to_id(http_dashboard_task, [task(Task)], TaskLink),
	  find_task_properties(Task, Props)
	},
	html([h3([class('sub-header')],
		 [a([href(TaskLink)],Title)]),
	      div([class('table-responsive')],
		  [table([class('table table-striped')],
			 [ thead([
			       tr([th('Property'), th('Value')])
			   ]),
			   tbody([
			       \show_user_props(Props)
			   ])
			 ])
		  ])
	     ]).

ann_sort_key(Annotation, Key-Annotation) :-
	rdf_has(Annotation, ann_ui:annotationField, Field),
	rdf_display_label(Field, FieldLabel),
	rdf_display_label(Annotation,BodyLabel),
	Key=key(FieldLabel, BodyLabel).

show_annotations(List, Options) -->
	{ maplist(ann_sort_key, List, KeyList),
	  keysort(KeyList, KeySorted),
	  pairs_values(KeySorted, Sorted)
	},
	show_annotations_(Sorted, Options).

show_annotations_([], _) --> !.
show_annotations_([H|T], Options) -->
	html(tr([],
		\show_annotation_summery(H, Options))),
	show_annotations_(T, Options).

is_specific_target_annotation(A) :-
	is_specific_target_annotation(A,_).

is_specific_target_annotation(A, T) :-
	rdf_has(A, oa:hasTarget, T),
	rdfs_individual_of(T, oa:'SpecificResource').

find_task_properties(Task, Props) :-
	find_annotations_by_task(Task, Annotations),
	include(is_specific_target_annotation, Annotations, SpecAnns),
	length(Annotations, NrAnnotations),
	length(SpecAnns, NrSpecAnns),
	NrObjAnnotations is NrAnnotations - NrSpecAnns,

	task_property_blacklist(PropertyBlackList),
	findall(Prop,
		(   rdf_has(Task, P, O),
		    \+ member(P, PropertyBlackList),
		    Prop =.. [P,O]
		),
		RDFProps),
	CountProps = [
	    annotations(literal(NrAnnotations)),
	    spec_annotations(literal(NrSpecAnns)),
	    obj_annotations(literal(NrObjAnnotations)),
	    targets_total('unknown'),
	    targets_complete('unknown'),
	    targets_worked_on('unknown'),
	    targets_untouched('unknown')
	],
	append([CountProps, RDFProps], Props).


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

find_tasks(Tasks) :-
	findall(Status-Task, task(Task, Status), Tasks0),
	keysort(Tasks0, TasksSorted),
	pairs_values(TasksSorted, Tasks).

find_workers(Workers) :-
	answer_set(W, rdf_has(_, oa:annotatedBy, W), Workers0),
	maplist(participant, Workers0, Workers1),
	sort(Workers1, Workers2),
	reverse(Workers2, Workers).


task(Task, Status) :-
	rdfs_individual_of(Task, ann_ui:'AnnotationTask'),
	rdf_has(Task, ann_ui:taskStatus, Status).

participant(Url, User) :-
	find_annotations_by_user(Url, Annotations),
	length(Annotations, Done),
	User= user{id:Url, done:Done}.

find_annotations_by_user(User, Annotations) :-
	findall(A, annotation_by_user(User, A), Anns0),
	maplist(ann_time, Anns0, APairs),
	group_pairs_by_key(APairs, AGrouped),
	keysort(AGrouped, AGroupedS),
	pairs_values(AGroupedS, Values),
	append(Values, Annotations).

find_annotations_by_task(Task, Annotations) :-
	rdf_has(Task, ann_ui:taskUI, UI),
	rdf_has(UI, ann_ui:fields, FieldList),
	findall(
	    A,
	    (	rdfs_member(Field, FieldList),
		annotation_by_field(Field, A)
	    ),
	    Annotations).

find_annotations_without_task(Annotations) :-
	findall(A, is_annotation(A), All),
	maplist(guess_task, All, AllT),
	group_pairs_by_key(AllT, Grouped),
	(   member(undefined-Annotations, Grouped)
	->  true
	;   Annotations = []
	),
	!.

is_annotation(A) :-
	rdfs_individual_of(A, oa:'Annotation').

guess_task(Ann,Task-Ann) :-
	rdf_has(Ann, ann_ui:annotationField, Field),
	rdf_has(UI, ann_ui:fields, FieldList),
	rdfs_member(Field, FieldList),
	rdf_has(Task, ann_ui:taskUI, UI),
	!.
guess_task(Ann, undefined-Ann).

ann_time(Ann, Time-Ann) :-
	rdf_has(Ann, oa:annotatedAt, TimeLit),
	literal_text(TimeLit, Time).

annotation_by_user(User, Annotation) :-
	rdf_has(Annotation, oa:annotatedBy, User).
annotation_by_field(Field, Annotation) :-
	rdf_has(Annotation, ann_ui:annotationField, Field).

property_key_label(targets_untouched, '# objects without any annotations').
property_key_label(targets_total,     '# objects to be annotated').
property_key_label(targets_complete,  '# objects completed').
property_key_label(targets_worked_on, '# objects with some annotations').
property_key_label(annotations,       '# total annotations for this task').
property_key_label(obj_annotations,   '# annotations on the object').
property_key_label(spec_annotations,  '# annotations on a image region').

show_user_props([]) --> !.

show_user_props([connection(_,_)|Tail]) -->
	show_user_props(Tail).
show_user_props([allow(_)|Tail]) -->
	show_user_props(Tail).
show_user_props([password(_)|Tail]) -->
	show_user_props(Tail).

show_user_props([Prop|Tail]) -->
	{ Prop =.. [K,V],
	  (   rdf_current_predicate(K)
	  ->  Key = \rdf_link(K)
	  ;   property_key_label(K, Key)
	  ->  true
	  ;   Key = K
	  ),
	  (   rdf_is_literal(V)
	  ->  Value = V
	  ;   rdf_subject(V)
	  ->  Value = \rdf_link(V)
	  ;   rdf_global_id(_:Value, V)
	  ->  true
	  ;   Value = V
	  )
	},
	html(tr([td(Key), td(Value)])),
	show_user_props(Tail).


current_judgment(Type, A, Jlist, J, checked) :-
	member(J, Jlist),
	rdf_has(J, oa:hasTarget, A),
	rdf_has(J, dc:title, literal(Type)),
	!.

current_judgment(_Type, A, Jlist, J, unchecked) :-
	member(J, Jlist),
	rdf_has(J, oa:hasTarget, A),!.

current_judgment(_,_,_, null, unchecked).


button_image(agree,    '../../icons/thumbUp.png').
button_image(disagree, '../../icons/thumbDown.png').

button_class(Type, Checked, Class) :-
	atomic_list_concat(
	    ['inline judgeButton ',
	     Type,
	     'Button ',
	     Checked
	    ],
	    Class).

judge_button(Type, Annotation, Field, Judgements) -->
	{  current_judgment(Type, Annotation, Judgements, J, Checked),
	   button_image(Type, ImageSrc),
	   button_class(Type, Checked, ButtonClass)
	},
	html([span([class(ButtonClass),
		    field(Field),
		    judgement(J),
		    annotation(Annotation)],
		   [img([src(ImageSrc)])
		   ])
	     ]).
is_judgement_of(A, J) :-
	rdf_has(J, oa:hasTarget, A).

show_annotation_summery(A, Options) -->
	{ rdf_has(A, oa:annotatedBy, User),
	  (   rdf_has(A, ann_ui:annotationField, Field)
	  ->  true
	  ;   Field = undefined
	  ),
	  option(judgements(J), Options, []),
	  include(is_judgement_of(A), J, Js)
	},
	html([
	    td([class(judgebuttoncell)],
	       [ \judge_button(agree, A, Field, Js),
		 \judge_button(disagree, A, Field, Js)
	       ]),
	    td(\rdf_link(Field, [resource_format(label)])),
	    td(\rdf_link(A,     [resource_format(label)])),
	    td(\rdf_link(User,  [resource_format(label)]))
	]).
