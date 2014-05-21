:- module(dashboard_api,
	  [
	  ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).

:- use_module(library(dashboard_util)).

:- http_handler(cliopatria(api/annotate/dashboard/task), http_api_dashboard_task, []).

http_api_dashboard_task(Request) :-
	http_parameters(Request,
			[task(Task, [uri, description('URI of the task')]),
			 filter(Filter, [default(ground),
					 oneof([ground, number])
					])
			]),
	find_task_properties(Task, Props, [filter(Filter)]),
	reply_json(Props).
