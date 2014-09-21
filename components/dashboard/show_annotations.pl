:- module(an_dashboard_components_show_annotations,
	  [ show_annotations//2
	  ]).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(pairs)).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(http/html_write)).

:- use_module(components(label)).

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

ann_sort_key(Annotation, Key-Annotation) :-
	rdf_has(Annotation, ann_ui:annotationField, Field),
	rdf_display_label(Field, FieldLabel),
	rdf_display_label(Annotation,BodyLabel),
	Key=key(FieldLabel, BodyLabel).

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
	       div([class('btn-group'), 'data-toggle'(buttons)],
		   [ \judge_button(agree, A, Field, Js),
		     \judge_button(disagree, A, Field, Js)
		   ])
	      ),
	    td(\rdf_link(Field, [resource_format(label)])),
	    td(\rdf_link(A,     [resource_format(label)])),
	    td(\rdf_link(User,  [resource_format(label)]))
	]).

is_judgement_of(A, J) :-
	rdf_has(J, oa:hasTarget, A).


judge_button(Type, Annotation, Field, Judgements) -->
	{  current_judgment(Type, Annotation, Judgements, J, Checked),
	   checked_active(Checked, Active)
	},
	html([
	    label([class([Active, btn,'btn-primary'])],
		  [ input([class([judgebutton, Type, Checked
				 ]),
			   field(Field),
			   judgement(J),
			   annotation(Annotation),
			   type(radio),
			   title(Type), 'data-toggle'(tooltip)
			  ],[]),
		    \button_glyph(Type)
		    ])
	]).

current_judgment(Type, A, Jlist, J, checked) :-
	member(J, Jlist),
	rdf_has(J, oa:hasTarget, A),
	rdf_has(J, dc:title, literal(Type)),
	!.

current_judgment(_Type, A, Jlist, J, unchecked) :-
	member(J, Jlist),
	rdf_has(J, oa:hasTarget, A),!.

current_judgment(_,_,_, null, unchecked).

button_glyph(agree) -->
	html(span([class([glyphicon,'glyphicon-thumbs-up'])],[' agree'])).
button_glyph(disagree) -->
	html(span([class([glyphicon,'glyphicon-thumbs-down'])],[' disagree'])).

checked_active(checked, active).
checked_active(_, '').
