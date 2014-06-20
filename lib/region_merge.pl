:- module(ad_region_merge,
	  [ region_merge/3
	  ]).

:- use_module(library(oa_annotation)).
:- use_module(library(settings)).

:- setting(minimum_overlap, float, 0.3, 'minimum overlapping ratio').

region_merge(Target, Merged, Options) :-
	findall(A, rdf_get_annotation_by_tfa(Target, _,_,_, A), As),
	reset_gensym(sid),
	region_merge(As, m{none:[]}, Merged, Options),
	dict_pairs(Merged, m, Pairs),
	length(Pairs, Lout),
	length(As, Lin),
	debug(merge, '~n# merged regions: ~d~n# input regions: ~d', [Lout,Lin]).


region_merge([], Result, Result, _Options):- !.
region_merge([H|T], Accum, Result, Options) :-
	dict_create(D, an, H),
	overlapping_annotation(D, Accum, NewAccum),
	!,
	region_merge(T, NewAccum, Result, Options).
region_merge([H|T], Accum, Result, Options) :-
	Accum.put(none, [H|Accum.none]),
	region_merge(T, [Accum], Result, Options).

overlapping_annotation(A, Accum, New) :-
        T = A.hasTarget,
	(   is_list(T)
	->  member(Target, T),
	    S = Target.get('hasSelector')
	;   S = T.get('hasSelector')
	),
	!,
	None = best{overlap:0, key:none},
	best_overlapping_selector(S, None, Accum, Best),
	(   insufficent_overlap(S, Best)
	->  gensym(sid, Tid),
	    debug(merge, 'Creating new ~w for ~w', [Tid, S.value]),
	    New = Accum.put(Tid,_{children:[A], hasSelector:S})
	;   merge_selectors(A, S, Best, Accum, New)
	).
overlapping_annotation(_A, Accum, Accum).


insufficent_overlap(S, Best) :-
	Best.overlap == 0, !,
	debug(merge, 'no overlap for ~w', [S.value]).

insufficent_overlap(S, B) :-
	AreaS is S.w * S.h,
	AreaB is B.hasSelector.w * B.hasSelector.h,
	RatioS is B.overlap/AreaS,
	RatioB is B.overlap/AreaB,
	debug(merge, 'overlap ~3f:~3f ~3f (~3f/~3f)',
	      [B.overlap, RatioS, RatioB, AreaS, AreaB]),

	setting(minimum_overlap, Min),
	(   RatioS < Min
	;   RatioB < Min
	).



merge_selectors(A, S, B, Accum, New) :-
	debug(merge, 'Merging: ~w into ~w, overlap ~3f',
	      [S.value, B.key, B.overlap]),
	AccumChildren = Accum.get(B.key).children,
	X is min(S.x, B.hasSelector.x), W is max(S.w, B.hasSelector.w),
	Y is min(S.y, B.hasSelector.y) ,H is max(S.h, B.hasSelector.h),
	NewSelector = selector{x:X, y:Y, w:W, h:H},
	New = Accum
          .put(B.key/children, [A|AccumChildren])
	  .put(B.key/hasSelector, NewSelector).

best_overlapping_selector(S, BestSoFar, AccumDict, Best) :-
	dict_pairs(AccumDict, m, AccumPairs),
	best_overlapping_selector_(S, BestSoFar, AccumPairs,Best).


best_overlapping_selector_(_,Best, [], Best):- !.
best_overlapping_selector_(S, BestSoFar, [H|T], Best) :-
	H=Key-Hdict,
	(   Key = none
	->  best_overlapping_selector_(S, BestSoFar, T, Best)
	;   HS=Hdict.hasSelector,
	    selector_overlap(S, HS, Overlap),
	    (	Overlap > BestSoFar.overlap
	    ->	NewBest = best{overlap:Overlap, key:Key, hasSelector:HS},
	        best_overlapping_selector_(S, NewBest, T, Best)
	    ;	best_overlapping_selector_(S, BestSoFar, T, Best)
	    )
	).

%     A.x1_______________AX2
%             B.x1_______________BX2
selector_overlap(A, B, Overlap) :-
	AX2 is A.x + A.w, AY2 is A.y + A.h,
	BX2 is B.x + B.w, BY2 is B.y + B.h,
	OX is min(AX2, BX2) - max(A.x, B.x),
	OY is min(AY2, BY2) - max(A.y, B.y),
	Overlap is OX * OY.





