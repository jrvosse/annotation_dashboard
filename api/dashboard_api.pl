:- module(dashboard_api,
	  [
	  ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).

:- use_module(library(dashboard_util)).

:- http_handler(cliopatria(api/annotate/dashboard/task), http_api_dashboard_task, []).

http_api_dashboard_task(Request) :-
	http_parameters(Request, [task(Task, [])]),
	find_task_properties(Task, Props),
	reply_json(Props).
