:- module(an_dashboard_components_show_tasks,
	  [ show_tasks//1
	  ]).

:- use_module(library(sort)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(dashboard_util)).
:- use_module(applications(annotation)).
:- use_module(show_option_list).


show_tasks([]) --> !.
show_tasks([H|T]) -->
	show_task(H),
	show_tasks(T).

task_compare(Order, Task1, Task2) :-
	compare(Order, Task1.order, Task2.order).

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
