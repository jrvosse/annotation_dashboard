:- module(an_dashboard, []).

% from SWI-Prolog libraries:
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(pairs)).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/js_write)).

% from ClioPatria:
:- use_module(library(semweb/rdf_label)).
:- use_module(cliopatria(hooks)).
:- use_module(components(label)).
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
:- use_module(library(region_merge)).

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

http_dashboard_task(Request) :-
	(setting(annotation:dashboard_admin_only, true)
	-> authorized(admin(dashboard)); true),
	http_parameters(Request, [task(Task, [])]),
	task_page(Task, [showTag(always)]).

http_dashboard_user(Request) :-
	(setting(annotation:dashboard_admin_only, true)
	-> authorized(admin(dashboard)); true),
	http_parameters(Request, [user(User, [])]),
	user_page(User, []).

task_page(Task, Options0) :-
	rdf_display_label(Task, Label),
	rdf_has(Task, ann_ui:taskUI, UI),
	get_metafields(UI, [], MetadataFields),
	get_anfields(UI, [], [], AnnotationFields),

	find_annotations_by_task(Task, Annotations),
	partition(is_tag, Annotations, Tags, Judgements),
	maplist(rdf_get_annotation_target, Tags, Targets),
	list_limit(Targets, 1, TargetsLimited, _Rest),
	sort(TargetsLimited, Objects),
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
				\show_objects(SortedObjects, Options)
			      ])
			])
		  ])
	    ]).

user_page(User, Options0) :-
	findall(Prop, user_property(User, Prop), Props),
	find_annotations_by_user(User, Annotations),
	partition(is_tag, Annotations, Tags, Judgements),
	maplist(rdf_get_annotation_target, Tags, Targets),
	sort(Targets, Objects),
	Options = [annotations(Annotations),
		   judgements(Judgements),
		   lazy(true),
		   user(User),
		   showTag(mine),
		   image_link_predicate(http_mediumscale) |
		   Options0
		  ],
	reply_html_page(
	    [title(User),
	     meta([name(viewport),
		    content('width=device-width, initial-scale=1')],
		   []),
	     \html_requires(dashboard)
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
					   [ \show_option_list(Props)
					   ])
				    ]),
				h2([class('sub-header')],
				   ['Annotations made so far']),
				\show_objects(Objects, Options)
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



show_objects([],_) --> !.
show_objects([H|T],Options) -->
	show_object(H,Options),
	show_objects(T,Options).


match_target(T,A) :-
	rdf_get_annotation_target(A,T).

show_object(O, Options) -->
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

task_compare(Order, Task1, Task2) :-
	compare(Order, Task1.order, Task2.order).

show_tasks([]) --> !.
show_tasks([H|T]) -->
	show_task(H),
	show_tasks(T).

show_task(Task) -->
	{ rdf_display_label(Task,Title),
	  http_link_to_id(http_dashboard_task, [task(Task)], TaskLink),
	  find_task_properties(Task, Props0, Representative,[]),
	  predsort(task_compare, Props0, Props),
	  object_image(Representative, Image),
	  http_link_to_id(http_medium_fit, [uri(Image)], ImageHref)

	},
	html([div(class(row),
		  [ h3([class('sub-header')],
		       [a([href(TaskLink)],Title)]),
		    div(class('col-sm-5'),
			[
			    img([src(ImageHref), alt('Example image for this task'),
				 class('img-responsive')],[])
			]),
		    div(class('col-sm-7'),
			[
			  div([class('table-responsive')],
			      [table([class('table table-striped')],
				     [ % thead([tr([th('Property'), th('Value')])]),
				       tbody([\show_option_list(Props)])
				     ])
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

show_option_list([]) --> !.
show_option_list([Prop|Tail]) -->
	{ (   is_dict(Prop)
	  ->  V = Prop.'@value',
	      Key = Prop.label
	  ;   (   Prop = K-V
	      ;	    Prop =.. [K,V]),
		    (   rdf_current_predicate(K)
		    ->  Key = \rdf_link(K)
		    ;   Key = K
		    )
	  ),
	  (   rdf_is_resource(V)
	  ->  (   rdf_subject(V)
	      ->  Value = \rdf_link(V)
	      ;	   (  rdf_global_id(_:Value, V)
		   ->  true
		   ;   Value = V
		   )
	      )
	  ;   (   rdf_is_literal(V)
	      ->  literal_text(V, Value)
	      ;   V = Value
	      )
	  )
	},
	html(tr([td(Key), td(Value)])),
	show_option_list(Tail).

show_option_list(Dict) -->
	{ is_dict(Dict),
	  dict_pairs(Dict, _Tag, Pairs)
	},
	show_option_list(Pairs).

current_judgment(Type, A, Jlist, J, checked) :-
	member(J, Jlist),
	rdf_has(J, oa:hasTarget, A),
	rdf_has(J, dc:title, literal(Type)),
	!.

current_judgment(_Type, A, Jlist, J, unchecked) :-
	member(J, Jlist),
	rdf_has(J, oa:hasTarget, A),!.

current_judgment(_,_,_, null, unchecked).


%button_image(agree,    '../../icons/thumbUp.png').
%button_image(disagree, '../../icons/thumbDown.png').

button_glyph(agree) -->
	html(span([class([glyphicon,'glyphicon-thumbs-up'])],[' agree'])).
button_glyph(disagree) -->
	html(span([class([glyphicon,'glyphicon-thumbs-down'])],[' disagree'])).

checked_active(checked, active).
checked_active(_, '').

button_class(Type, Checked, Class) :-
	checked_active(Checked, Active),
	atomic_list_concat(
	    ['inline judgeButton ',
	     Type,
	     'Button ',
	     Checked , ' ',
	     Active
	    ],
	    Class).

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
			  ],
			  [
			      \button_glyph(Type)
			  ])
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
	       div([class('btn-group'), 'data-toggle'(buttons)],
		   [ \judge_button(agree, A, Field, Js),
		     \judge_button(disagree, A, Field, Js)
		   ])
	      ),
	    td(\rdf_link(Field, [resource_format(label)])),
	    td(\rdf_link(A,     [resource_format(label)])),
	    td(\rdf_link(User,  [resource_format(label)]))
	]).


task_stats(Task) -->
	{ http_link_to_id(http_api_dashboard_task,
			  [task(Task), filter(number)], DataSource),
	  gensym(chart, Class),
	  atom_concat('svg.', Class, Selector)
	},
	html([
	    \html_requires(task_stats),
	    svg([class(Class)],[]),
	    \js_script({|javascript(DataSource, Selector)||
			var cwidth = document.body.clientWidth;
			var margin = {top: 20, right: 40, bottom: 50, left: 50},
			width =  0.45*cwidth - margin.left - margin.right,
			height = 200 - margin.top - margin.bottom;


			var x = d3.scale.ordinal()
			.rangeRoundBands([5, width], .1);

			var y = d3.scale.linear()
			.range([height, 0]);

			var xAxis = d3.svg.axis()
			.scale(x)
			.tickSize(10,0)
			.orient("bottom");

			var yAxis = d3.svg.axis()
			.scale(y)
			.orient("left")
			.ticks(10, "");

			var svg = d3.select(Selector)
			.attr("width", width + margin.left + margin.right)
			.attr("height", height + margin.top + margin.bottom)
			.append("g")
			.attr("transform", "translate(" + margin.left + "," + margin.top + ")");

			d3.json(DataSource, function(error, data) {
						data.sort(compare_value);

						x.domain(data.map(function(d) { return d.label; }));
						y.domain([0, d3.max(data, function(d) { return d['@value']; })]);

						var bar = svg.selectAll("g")
						.data(data)
						.enter()
						.append("g");

						bar.append("rect")
						.attr("class", function(d) { return "bar " + d.key; })
						.attr("x", function(d) { return x(d.label); })
						.attr("width", x.rangeBand())
						.attr("y", function(d) { return y(d['@value']); })
						.attr("height", function(d) { return height - y(d['@value']); });

						bar.append("text")
						.style("text-anchor", "middle")
						.attr("class", "count")
						.attr("x", function(d) { return x(d.label) + 0.5 * x.rangeBand(); })
						.attr("y", function(d) { return Math.min(y(0), y(d['@value'])); })
						.attr("dy", "-1ex")
						.text(function(d) { return d['@value']; });

						svg.append("g")
						.attr("class", "x axis")
						.attr("transform", "translate(0," + height + ")")
						.call(xAxis).selectAll("text")
						.style("text-anchor", "middle")
						.attr("dx", '.1em')
						.attr("dy", function(d,i) { return 0 + 14*(i%3); });

						d3.selectAll("g.x.axis g.tick line")
						.style("stroke-opacity", 0.2)
						.attr("y2", function(d,i){ return 15 + 14*(i%3); });

						svg.append("g")
						.attr("class", "y axis")
						.call(yAxis)
						.append("text")
						.attr("transform", "rotate(-90)")
						.attr("y", 6)
						.attr("dy", "0.7ex")
						.style("text-anchor", "end")
						.text("Count");
					    });

			function compare_value(a,b) {
				     if (a.value < b.value) return 1;
				     if (a.value > b.value) return -1;
				     return 0;
				 }


		       |})
	]).
