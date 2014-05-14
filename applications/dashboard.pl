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
:- use_module(cliopatria(hooks)).
:- use_module(components(label)).
:- use_module(user(user_db)).

% from other cpacks:
:- use_module(library(yui3_beta)). % needed for html resource declarations
:- use_module(library(oa_schema)).
:- use_module(library(oa_annotation)).
:- use_module(api(media_caching)).       % needed for http api handlers
:- use_module(applications(annotation)). % needed for http api handlers

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

task_page(Task, _Options) :-
	rdf_display_label(Task, Label),
	find_annotations_by_task(Task, Annotations),
	partition(is_tag, Annotations, Tags, Judgements),
	maplist(rdf_get_annotation_target, Tags, Targets),
	sort(Targets, Objects),
	rdf_has(Task, ann_ui:taskUI, UI),
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
				\show_objects(Objects,
					      [annotations(Annotations),
					       judgements(Judgements),
					       ui(UI)
					      ])
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
	find_users(Users),
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
			     ], 'Accurator')
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
	  option(ui(UI), Options, undefined),
	  object_image(O, Image),
	  http_link_to_id(http_fit_thumbnail, [uri(Image)], Thumbnail),
	  (   UI \= undefined
	  ->  http_link_to_id(http_annotation, [target(O), ui(UI)], ImageLink)
	  ;   http_link_to_id(http_original, [uri(Image)], ImageLink)
	  ),
	  include(match_target(O), A, Annotations)
	},
	html(
	    [div([class(row)],
		 [div([class('col-xs-6')],
		      [a([href(ImageLink)],
			 [img([class('dashboard object image'), src(Thumbnail)
			      ])
			 ]),
		       div([class('resource title')], \rdf_link(O, [resource_format(label),max_length(50)]))
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

find_tasks(Tasks) :-
	findall(Status-Task, task(Task, Status), Tasks0),
	keysort(Tasks0, TasksSorted),
	pairs_values(TasksSorted, Tasks).

find_users(Users) :-
	findall(User, participant(User), Users0),
	sort(Users0, Users).

task(Task, Status) :-
	rdfs_individual_of(Task, ann_ui:'AnnotationTask'),
	rdf_has(Task, ann_ui:taskStatus, Status).

participant(User) :-
	current_user(Uid),
	user_property(Uid, url(URL)),
	find_annotations_by_user(URL, Annotations),
	length(Annotations, Done),
	User= [
	       id(Uid), url(URL), done(Done)
	      ].


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
	rdf(Ann, oa:annotatedAt, TimeLit),
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
	html([span([class(ButtonClass), field(Field), judgement(J), annotation(Annotation)],
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
	    td([style('')],
	       [ \judge_button(agree, A, Field, Js),
		 \judge_button(disagree, A, Field, Js)
	       ]),
	    td(\rdf_link(Field, [resource_format(label)])),
	    td(\rdf_link(A,     [resource_format(label)])),
	    td(\rdf_link(User,  [resource_format(label)]))
	]).
