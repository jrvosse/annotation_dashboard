:- module(dashboard_util,
	  [ is_tag/1,
	    guess_task/2,
	    count_annotations/2,
	    blacklisted_annotation/1,
	    find_annotations_by_user/2,
	    find_annotations_by_task/2,
	    find_annotations_without_task/1,

	    find_tasks/1,
	    find_task_properties/4,

	    find_workers/1
	  ]).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(pairs)).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(count)).

:- use_module(library(oa_annotation)).
:- use_module(applications(annotation)).

:- rdf_meta
	task_property_blacklist(t).

task_property_blacklist(
    [rdf:xtype,
     dcterms:xtitle,
     'xhttp://semanticweb.cs.vu.nl/annotate/ui/taskUI'
    ]).

blacklisted_annotation(A) :-
	rdf_has(A, oa:annotatedBy, 'http://sealinc.ops.few.vu.nl/accurator/user/jasper')
	;
	rdf_has(A, oa:annotedBy, 'http://localhost:3020/user/admin')
	;
	rdf_has(A, dcterms:title, literal(test)).

is_tag(A) :-
	rdf_has(A, oa:motivatedBy, oa:tagging).

is_tag(A) :-
	rdfs_individual_of(A, ann_ui:tag).

is_annotation(A) :-
	rdfs_individual_of(A, oa:'Annotation').

annotation_by_user(User, Annotation) :-
	rdf_has(Annotation, oa:annotatedBy, User).
annotation_by_field(Field, Annotation) :-
	rdf_has(Annotation, ann_ui:annotationField, Field).

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
		annotation_by_field(Field, A),
		\+ blacklisted_annotation(A)
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

is_specific_target_annotation(A) :-
	is_specific_target_annotation(A,_).

is_specific_target_annotation(A, T) :-
	rdf_has(A, oa:hasTarget, T),
	rdfs_individual_of(T, oa:'SpecificResource').

find_task_properties(Task, Props, Representative, Options) :-
	option(filter(Filter), Options, ground),
	find_annotations_by_task(Task, Annotations),
	maplist(rdf_get_annotation_target, Annotations, WorkedOn0),
	sort(WorkedOn0, WorkedOn),
	include(is_specific_target_annotation, Annotations, SpecAnns),
	length(Annotations, NrAnnotations),
	length(SpecAnns, NrSpecAnns),
	length(WorkedOn, NrWorkedOn),
	NrObjAnnotations is NrAnnotations - NrSpecAnns,
	NrTotalTargets = 200,  % Fixme
	NrCompleteTargets = 0, % Fixme
	NrUntouched is NrTotalTargets - NrWorkedOn,
	task_property_blacklist(PropertyBlackList),
	representative(WorkedOn, Representative),
	findall(Prop,
		(   rdf_has(Task, P, O),
		    \+ member(P, PropertyBlackList),
		    F =.. [Filter, O],
		    F,
		    Prop =.. [P,O]
		),
		RDFProps),
	CountProps = [ annotations(NrAnnotations),
		       spec_annotations(NrSpecAnns),
		       obj_annotations(NrObjAnnotations),
		       targets_total(NrTotalTargets),
		       targets_complete(NrCompleteTargets),
		       targets_worked_on(NrWorkedOn),
		       targets_untouched(NrUntouched)
		     ],
	append([CountProps, RDFProps], Opts0),
	maplist(pmap, Opts0, Props).

count_annotations(Target, Count-Target) :-
	findall(A,
		(   rdf_get_annotation_target(A, Target),
		    \+ blacklisted_annotation(A)
		),
		As),

	length(As, Count).

representative(Candidates, Representative) :-
	maplist(count_annotations, Candidates, Counts),
	sort(Counts, Sorted),
	pairs_values(Sorted, Values),
	reverse(Values, Ordered),
	find_first_with_image(Ordered, Representative).
representative(_, no_representative_found).

find_first_with_image([H|_], H) :-
	\+ no_object_image(H), !.
find_first_with_image([_|T], First) :-
	find_first_with_image(T, First).


property_key_label(500, targets_untouched, 'works without tags').
property_key_label(200, targets_total,     'works targeted').
property_key_label(250, targets_complete,  'works completed').
property_key_label(300, targets_worked_on, 'works with some tags').
property_key_label(100, annotations,       'total annotations').
property_key_label(105, obj_annotations,   'annotations on object').
property_key_label(104, spec_annotations,  'annotations on region').
property_key_label(999, Key, Label) :- Label = \rdf_link(Key).

pmap(In, Out) :-
	In =.. [Key, Value],
	property_key_label(Order, Key, Label),
	Out = task_stats{order:Order,label:Label, '@value':Value, key:Key}.

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
