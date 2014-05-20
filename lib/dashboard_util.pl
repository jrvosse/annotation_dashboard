:- module(dashboard_util,
	  [ is_tag/1,
	    find_annotations_by_user/2,
	    find_annotations_by_task/2,
	    find_annotations_without_task/1,

	    find_tasks/1,
	    find_task_properties/2,

	    find_workers/1
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(count)).

:- rdf_meta
	task_property_blacklist(t).

task_property_blacklist(
    [rdf:type,
     dcterms:title
    ]).

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
	CountProps = task_stats{
			 annotations: NrAnnotations,
			 spec_annotations: NrSpecAnns,
			 obj_annotations: NrObjAnnotations,
			 targets_total:'unknown',
			 targets_complete:'unknown',
			 targets_worked_on:'unknown',
			 targets_untouched:'unknown'
		     },
	put_dict(RDFProps, CountProps, Props).

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
