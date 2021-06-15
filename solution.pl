% batuhan tongarlak
% 2019400270
% compiling: yes
% complete: yes


% include the knowledge base'
:- ['load.pro'].


%(List, Sum) This predicate is true when Sum is equal to the summation of
%all elements in the list.
sum_of_the_list([],0).
sum_of_the_list(List, Sum) :-
	[H|T] = List,
	sum_of_the_list(T,TailSum),
	Sum is H + TailSum.

%(List1, List2, Result) This predicate is true when Result[i] is equal to
%the multiplication of List1[i] and List2[i].
multiply_two_lists([],[],[]).
multiply_two_lists([Head1|Tail1],[Head2|Tail2],Result) :-
	multiply_two_lists(Tail1,Tail2,TailResult),
	HeadResult is Head1*Head2,
	Result = [HeadResult|TailResult].

%(Name1, Name2) This predicate is true when th Glanian named Name2's gender 
%fits the Glanian named Name1' expected gender preferences.
is_gender_expected(Name1, Name2) :-   %it means, Name1 expects gender of Name2
	expects(Name1,OnesExpectedGenders,_),
	glanian(Name2,Gender,_),
	member(Gender,OnesExpectedGenders).


%(Result, List1, List2) This predicate is true when Result[i] is equal to
%this : List1[i]-List2[i]
create_dashed_list([], [], []).
create_dashed_list([Head|Tail], [HeadFirst|TailFirst], [HeadSecond|TailSecond]) :-
	Head = HeadFirst-HeadSecond,
    create_dashed_list(Tail, TailFirst, TailSecond).
%Actually, this predicate is Unnecessary and creates redundancy 
%(it is the same as create_dashed_list predicate), but the sake of
%clearity, I gave different names for creating and dividing dashed lists.
divide_dashed_list([], [], []).
divide_dashed_list([Head|Tail], [HeadFirst|TailFirst], [HeadSecond|TailSecond]) :-
    HeadFirst-HeadSecond = Head,
    divide_dashed_list(Tail, TailFirst, TailSecond).

%(Name, TargetList, DistanceList) This predicate is true when DistanceList[i]
%is equal to the glanian distance between Name and Targetlist[i].
find_distances_of_a_list(Name,[],[]).
find_distances_of_a_list(Name, TargetList,DistanceList):-
	[HT|TT] = TargetList,
	[HD|TD] = DistanceList,
	glanian_distance(Name,HT,HD),
	find_distances_of_a_list(Name,TT,TD).

%------WEIGHTED DISTANCES OF A LIST-----

%(Name, TargetList,DistanceList) This predicate is true when DistanceList[i]
%is equal to the weighted glanian distance between Name and Targetlist[i].
find_weighted_distances_of_a_list(Name,[],[]).
find_weighted_distances_of_a_list(Name, TargetList,DistanceList):-
	[HT|TT] = TargetList,
	[HD|TD] = DistanceList,
	weighted_glanian_distance(Name,HT,HD),
	find_weighted_distances_of_a_list(Name,TT,TD).

%--------OLD RELATION--------

%(Name,TargetList,FilteredList,Distances,FilteredDistances), This predicate is true when,
%The FilteredList and FİlteredDistances are consisted regarding to that rule:
%If there is an old relationship between Name and TargetList[i], do not include 
%the corresponding TargetName and Distance values into the new Lists.
find_and_delete_old_relationships(Name,[],[],[],[]).
find_and_delete_old_relationships(Name,TargetList,FilteredList,Distances,NewD):-
	[HT|TT] = TargetList,
	[HF|TF] = FilteredList,
	[HD|TD] = Distances,
	[HND|TND] = NewD,
 	not(old_relation([Name,HT]);old_relation([HT,Name])), % old relationshipi yoksa devam
	HF = HT,
	HND = HD,
	find_and_delete_old_relationships(Name,TT,TF,TD,TND).


find_and_delete_old_relationships(Name,TargetList,FilteredList,Distances,NewD):-
	[HT|TT] = TargetList,
	[HD|TD] = Distances,
	find_and_delete_old_relationships(Name,TT,FilteredList,TD,NewD).

%-----DELETE ACCORDING TO THE FEATURES---------	

%(Name,TargetList,Distances,NewTargets,NewDistances) This predicate is true when
%there is no problem with respect to the features and limit constraints in the last 2 Lists. 
%Glanian named Name has some limits, so there will be only suitable Glanian Targets
%in the NewTargets and their corresponding Distances in the NewDistances 
delete_ac_to_features(Name,[],[],[],[]).
delete_ac_to_features(Name,TargetList,Distances,NewTargets,NewDistances):-
	[HT|TT]=TargetList,
	[HNT|TNT] = NewTargets,
	[HD|TD] = Distances,
	[HND|TND] = NewDistances,
	glanian(HT,_,Features),
	dislikes(Name,_,_,Limits),
	is_it_proper_wrt_features(Limits,Features),
	HNT=HT,
	HND=HD,
	delete_ac_to_features(Name,TT,TD,TNT,TND).

delete_ac_to_features(Name,TargetList,Distances,NewTargets,NewDistances):-
	[HT|TT]=TargetList,
	[HD|TD]=Distances,
	delete_ac_to_features(Name,TT,TD,NewTargets,NewDistances).

%(Limits,Features) This predicate is true when Features[i] is in between the corresponding
%Limits[i] for all i's.
is_it_proper_wrt_features([],[]).
is_it_proper_wrt_features(Limits,Features):-
	[HF|TF] = Features,
	[HL|TL] = Limits,
	((HL = []);
	([Min|Rest] = HL,
	Min < HF,
	[Max|_]=Rest,
	Max > HF)),
	is_it_proper_wrt_features(TL,TF).

%---------DELETE ACCORDING TO THE INTERSECTION -------- belki burda sorun vardır

%(Name,TargetList,NewTargetList,Distances,NewDistances) This predicate is true when
%NewTargetList and NewDistances contains no elements that disobeys this rule:
%The intersection between Name’s DislikedActivities
%and Target[i]’s LikedActivities should not be more than two.
delete_ac_to_intersection(Name,[],[],[],[]).
delete_ac_to_intersection(Name,TargetList,NewTargetList,Distances,NewDistances):-
	[HT|TT] = TargetList,
	[HNT|TNT] = NewTargetList,
	[HD|TD] = Distances,
	[HND|TND] = NewDistances,
	dislikes(Name,DislikedActs,_,_),
	likes(HT,LikedActivities,_),
	intersection(DislikedActs, LikedActivities, IntersectActs),
	length(IntersectActs,Length),
	Length =< 2,
	HNT=HT,
	HND=HD,
	delete_ac_to_intersection(Name,TT,TNT,TD,TND).

delete_ac_to_intersection(Name,TargetList,NewTargetList,Distances,NewDistances):-
	[HT|TT] = TargetList,
	[HD|TD] = Distances,
	delete_ac_to_intersection(Name,TT,NewTargetList,TD,NewDistances).
	
%-----FIND PAIRS------
%Cities and activities are constructed with the idea of 'Pairs', From now, 
%They will be founded as pairs.

%(Name,SortedPairedfalan) is true when SortedPairs in this form:
%(Activity that Name can do)-(City that this activity can be done) with following rule:
%(**)Name should either like City[i] or be a habitant of City[i] (i.e. predicate 3.3), 
%or there should be an Activity[i] in City[i] that is also in LikedActivities of Name.
find_pairs(Name,SortedPairedfalan):-
	find_possible_cities(Name,LikedCities),
	pair_generator_city(LikedCities,CityOutC,ActOutC),
	create_dashed_list(PairedListC,ActOutC,CityOutC),
	likes(Name,LikedActivities,_),
	pair_generator_acts(LikedActivities,CityOutA,ActOutA),
	create_dashed_list(PairedListA,ActOutA,CityOutA),
	union(PairedListA, PairedListC, PairedPossibleCityandActs),
	keysort(PairedPossibleCityandActs, SortedPairedfalan).

%This is a sub predicate that used in the find_pairs,
%CityOut2 and ActOut2 is constructed from only the first part of the
%main restriction (**). (before OR) 
pair_generator_city([],[],[]).
pair_generator_city(LikedCities,CityOut2,ActOut2):-	
	[HLC|TLC] = LikedCities,
	[HCO|TCO] = CityOut,
	[HAO|TAO] = ActOut,
	city(HLC,_,ActivityListofCity),
	sort(ActivityListofCity, SortedActivityListofCity),
	length(SortedActivityListofCity, Length),
	findall(HLC, between(1, Length, _), HCO),
	HAO = SortedActivityListofCity,
	pair_generator_city(TLC,TCO,TAO),
	flatten(CityOut,CityOut2),
	flatten(ActOut, ActOut2).

%This is a sub predicate that used in the find_pairs,
%CityOut2 and ActOut2 is constructed from only the second part of the
%main restriction (**). (after OR) 
pair_generator_acts([],[],[]).
pair_generator_acts(LikedActivities,CityOut2,ActOut2):-
	[HLA|TLA] = LikedActivities,
	[HCO|TCO] = CityOut,
	[HAO|TAO] = ActOut,
	findall(Cities, city(Cities,_,_), CityBag),
	%func that takes citybag
	for_city(CityBag,HLA,HCO,HAO),
	pair_generator_acts(TLA,TCO,TAO),
	flatten(CityOut,CityOut2),
	flatten(ActOut,ActOut2).


pair_generator_acts(LikedActivities,CityOut2,ActOut2):-
	[HLA|TLA] = LikedActivities,
	pair_generator_acts(TLA,CityOut2,ActOut2).

%CityBag,Activity,CityOut,ActOut), CityBag contains all Cities, 
%This predicate is true when CityOut consists of the same city and
%ActOut is consisted of the corresponding activities that can be done 
%in that city, With more than use of this predicate, Corresponding output
%(at pair_generator_acts) will be a list of lists, to solve that, I used flatten predicate.
for_city([],Activity,[],[]).
for_city(CityBag,Activity,CityOut,ActOut):-
	[HCO|TCO] = CityOut,
	[HAO|TAO] = ActOut,
	[HCB|TCB]=CityBag,
	city(HCB,_,ActivitiesonCity),
	member(Activity, ActivitiesonCity),
	HCO = HCB,
	HAO = Activity,
	for_city(TCB,Activity,TCO,TAO).

for_city(CityBag,Activity,CityOut,ActOut):-
	[HCO|TCO] = CityOut,
	[HAO|TAO] = ActOut,
	[HCB|TCB]=CityBag,
	for_city(TCB,Activity,TCO,TAO).


%--------ALL THE PAIRS ARE FOUND

%--------NOW FIX THEM

%This is the main predicate for weed out the pairs (activity-city)
%wrt given constraints:
%1- City[i] should not be in DislikedCities of Name.
%2- Activity[i] should not be in DislikedActivities of Name.
%3- City[i] should be in CityList where merge possible cities(Name, Target, CityList) is true
%(i.e. the city should be in the merged possible cities).
fixer(Name,Target,PairedList,FixedPairedList4):-
	dislikes(Name,_,DislikedCities,_),
	fix_for_disliked_cities(DislikedCities,PairedList,FixedPairedList1),
	merge_possible_cities(Name,Target,MergedCities),
	fix_for_merged_cities(MergedCities,FixedPairedList1,FixedPairedList2),
	last_fix_for_merged_cities(FixedPairedList1,FixedPairedList2,FixedPairedList3),
	dislikes(Name,DislikedActs,_,_),
	fix_for_disliked_activities(DislikedActs,FixedPairedList3,FixedPairedList4).


%This predicate is true when FixedPairedList (Filtered version of PairedList)
%doesn't contain the elements
%which has key: Key.
fix_key(Key, PairedList, FixedPairedList) :-
    delete(PairedList, Key-_, FixedPairedList),
	!.

%This predicate is true when FixedPairedList (Filtered version of PairedList)
%doesn't contain the elements
%which has value: Value.
fix_value(Value, PairedList, FixedPairedList) :-
	delete(PairedList,_-Value,FixedPairedList),
	!.

%Specific predicate for the case of Name's disliked cities, it is true when
%the FixedPairedList is the modified version of Paired List where 
%necessary deletions have occured.
fix_for_disliked_cities([],A,A).
fix_for_disliked_cities(DislikedCities,PairedList,FixedPairedList):-
	[HDC|TDC]=DislikedCities,
	fix_value(HDC,PairedList,MiddlePairedList),
	fix_for_disliked_cities(TDC,MiddlePairedList,FixedPairedList).

%Specific predicate for the case of merge possible cities(Name, Target, CityList)
%case, it is true when the FixedPairedList[i]'s city is not in the merged cities, 
%to achieve main goal, we have subtract it from the PairedList.
fix_for_merged_cities([],A,A).
fix_for_merged_cities(MergedCities,PairedList,FixedPairedList):-
	[HMC|TMC]=MergedCities,
	fix_value(HMC,PairedList,MiddlePairedList),
	fix_for_merged_cities(TMC,MiddlePairedList,FixedPairedList).

%This predicate provides the subtraction mentioned above.
last_fix_for_merged_cities(OldPairedList,MergeFixList,FixedPairedList):-
	subtract(OldPairedList,MergeFixList,FixedPairedList).

%Specific predicate for the case of Name's disliked activities, it is true when
%the FixedPairedList is the modified version of Paired List where 
%necessary deletions have occured.
fix_for_disliked_activities([],A,A).
fix_for_disliked_activities(DislikedActs,PairedList,FixedPairedList):-
	[HDA|TDA]=DislikedActs,
	fix_key(HDA,PairedList,MiddlePairedList),
	fix_for_disliked_activities(TDA,MiddlePairedList,FixedPairedList).


%-------MULTIPLICATION PART FOR TARGETS AND DISTANCES

%(TargetName,Distance,PairedList,TargetOut,DistanceOut) this predicate is true when
%TargetOut and DistanceOut is consists of the element TargetName and Distance value
%with length of PairedList.
multiply_instances(TargetName,Distance,PairedList,TargetOut,DistanceOut):-
	length(PairedList,Length),
	findall(TargetName, between(1, Length, _), TargetOut),
	findall(Distance, between(1, Length, _),DistanceOut).

%All of the sub predicates (finding pairs, fixing them and multiplying instances)
%above are used in here, it is true when TargetOut and DistanceOut is consists of 
%TargetNames and Distances with multiplicated by a proper value determined in inside 
%of the function.
full_of_it(Name,[],[],[],[],[]).
full_of_it(Name,TargetList,DistanceList,TargetOut,DistanceOut,FullPairs):-
	[HT|TT]=TargetList,
	[HD|TD]=DistanceList,
	[HTO|TTO]=TargetOut,
	[HDO|TDO]=DistanceOut,
	[HFP|TFP]=FullPairs,
	find_pairs(Name,Pairs),
	fixer(Name,HT,Pairs,FixedPairs),
	HFP = FixedPairs,
	multiply_instances(HT,HD,FixedPairs,HTO,HDO),
	full_of_it(Name,TT,TD,TTO,TDO,TFP).

%-----------MATCH PART-----
%I used similar predicates for 3.9 (match case)

%----------WEIGHTED DISTANCES AVERAGE FOR MATCH CASE

%this predicate is true when TargetOut and DistanceOut are calculated according to
%the formula mentioned in the descripytion at 3.9. 
find_avg_weighted_distance(Name,[],[],[],[]).
find_avg_weighted_distance(Name,TargetList,Distances,TargetOut,DistanceOut):-
	[HT|TT]=TargetList,
	[HD|TD]=Distances,
	[HTO|TTO]=TargetOut,
	[HDO|TDO]=DistanceOut,
	weighted_glanian_distance(HT, Name, DistanceT),
	HDO is (HD + DistanceT)/2,
	HTO = HT,
	find_avg_weighted_distance(Name,TT,TD,TTO,TDO).

find_avg_weighted_distance(Name,TargetList,Distances,TargetOut,DistanceOut):-
	[HT|TT]=TargetList,
	[HD|TD]=Distances,
	find_avg_weighted_distance(Name,TT,TD,TargetOut,DistanceOut).

%this predicate is true when SortedTargets and SortedDistances are the
%sorted versions of the TargetList and Distances respectively.
sort_avg_weight_list(TargetList,Distances,SortedTargets,SortedDistances):-
	create_dashed_list(PairedList,Distances,TargetList),
	keysort(PairedList, DashedTemp),
	divide_dashed_list(DashedTemp,SortedDistances,SortedTargets).

%----DELETION FOR TARGET'S EXPECTED GENDER(S) (MATCH CASE)

%this prediceate checks the expected gender conditions for both name and head of
%the target list
delete_ac_to_exp_gender_match(Name,[],[],[],[]).
delete_ac_to_exp_gender_match(Name,TargetList,NewTargetList,Distances,NewDistances):-
	[HT|TT] = TargetList,
	[HNT|TNT] = NewTargetList,
	[HD|TD] = Distances,
	[HND|TND] = NewDistances,
	is_gender_expected(HT,Name),
	HNT=HT,
	HND=HD,
	delete_ac_to_exp_gender_match(Name,TT,TNT,TD,TND).

delete_ac_to_exp_gender_match(Name,TargetList,NewTargetList,Distances,NewDistances):-
	[HT|TT] = TargetList,
	[HD|TD] = Distances,
	delete_ac_to_exp_gender_match(Name,TT,NewTargetList,TD,NewDistances).

%----DELETION FOR FEATURES FOR TARGET (MATCH CASE) --

%this prediceate checks the feature conditions for both name and head of
%the target list
delete_ac_to_features_match(Name,[],[],[],[]).
delete_ac_to_features_match(Name,TargetList,Distances,NewTargets,NewDistances):-
	[HT|TT]=TargetList,
	[HNT|TNT] = NewTargets,
	[HD|TD] = Distances,
	[HND|TND] = NewDistances,
	glanian(HT,_,Features1),
	dislikes(Name,_,_,Limits1),
	glanian(Name,_,Features2),
	dislikes(HT,_,_,Limits2),
	(is_it_proper_wrt_features_match(Limits1,Features1) , is_it_proper_wrt_features_match(Limits2,Features2)),
	HNT=HT,
	HND=HD,
	delete_ac_to_features_match(Name,TT,TD,TNT,TND).

delete_ac_to_features_match(Name,TargetList,Distances,NewTargets,NewDistances):-
	[HT|TT]=TargetList,
	[HD|TD]=Distances,
	delete_ac_to_features_match(Name,TT,TD,NewTargets,NewDistances).

is_it_proper_wrt_features_match([],[]).
is_it_proper_wrt_features_match(Limits,Features):-
	[HF|TF] = Features,
	[HL|TL] = Limits,
	((HL = []);
	([Min|Rest] = HL,
	Min < HF,
	[Max|_]=Rest,
	Max > HF)),
	is_it_proper_wrt_features_match(TL,TF).

%------DELETION FOR INTERSECTION TARGET (MATCH CASE)---

%this prediceate checks the intersection of liked and disliked activities
%conditions for both name and head of the target list
delete_ac_to_intersection_match(Name,[],[],[],[]).
delete_ac_to_intersection_match(Name,TargetList,NewTargetList,Distances,NewDistances):-
	[HT|TT] = TargetList,
	[HNT|TNT] = NewTargetList,
	[HD|TD] = Distances,
	[HND|TND] = NewDistances,
	((dislikes(Name,DislikedActs1,_,_),
	likes(HT,LikedActivities1,_),
	intersection(DislikedActs1, LikedActivities1, IntersectActs1),
	length(IntersectActs1,Length1),
	Length1 =< 2),
	(dislikes(HT,DislikedActs2,_,_),
	likes(Name,LikedActivities2,_),
	intersection(DislikedActs2, LikedActivities2, IntersectActs2),
	length(IntersectActs2,Length2),
	Length2 =< 2)),
	HNT=HT,
	HND=HD,
	delete_ac_to_intersection_match(Name,TT,TNT,TD,TND).

delete_ac_to_intersection_match(Name,TargetList,NewTargetList,Distances,NewDistances):-
	[HT|TT] = TargetList,
	[HD|TD] = Distances,
	delete_ac_to_intersection_match(Name,TT,NewTargetList,TD,NewDistances).

%------FULL OF IT FOR MATCHES-----
%same predicate used 3.8, only difference is now we think the targetlist[i]'s
full_of_it_for_match(Name,[],[],[],[],[]).
full_of_it_for_match(Name,TargetList,DistanceList,TargetOut,DistanceOut,FullPairs):-
	[HT|TT]=TargetList,
	[HD|TD]=DistanceList,
	[HTO|TTO]=TargetOut,
	[HDO|TDO]=DistanceOut,
	[HFP|TFP]=FullPairs,
	find_pairs(Name,Pairs1),
	find_pairs(HT,Pairs2),
	intersection(Pairs1, Pairs2, Pairs),
	fixer(Name,HT,Pairs,FixedPairs),
	fixer(HT,Name,FixedPairs,FixedPairs2),
	HFP = FixedPairs2,
	multiply_instances(HT,HD,FixedPairs2,HTO,HDO),
	full_of_it_for_match(Name,TT,TD,TTO,TDO,TFP).

%--------PREDICATES USED IN 3.1 AND 3.2----
%It is true when NewList is subtraction of expecteds and features 
%within the given rules (-1 case)
subtract_e_f([],[],[]).
subtract_e_f(Expecteds,Features,NewList):-
	[HE|TE]=Expecteds,
	[HF|TF]=Features,
	[HN|TN]=NewList,
	not(HE is -1),
	not(HF is -1),
	HN is (HE - HF)*(HE - HF),
	subtract_e_f(TE,TF,TN).

subtract_e_f(Expecteds,Features,NewList):-
	[HE|TE]=Expecteds,
	[HF|TF]=Features,
	subtract_e_f(TE,TF,NewList).

%It is true when NewList is a proper Lİst according to the expression at 3.2
subtract_e_f_weighted([],[],[],[]).
subtract_e_f_weighted(Expecteds,Features,Weights,NewList):-
	[HE|TE]=Expecteds,
	[HF|TF]=Features,
	[HW|TW]=Weights,
	[HN|TN]=NewList,
	not(HE is -1),
	not(HF is -1),
	not(HW is -1),
	HN is HW*(HE - HF)*(HE - HF),
	subtract_e_f_weighted(TE,TF,TW,TN).

subtract_e_f_weighted(Expecteds,Features,Weights,NewList):-
	[HE|TE]=Expecteds,
	[HF|TF]=Features,
	[HW|TW]=Weights,
	subtract_e_f_weighted(TE,TF,TW,NewList).


%-----------MAIN PREDICATES--------------

glanian_distance(Name1,Name2,Distance):-
	expects(Name1,_,Expecteds),
	glanian(Name2,_,Features),	
	subtract_e_f(Expecteds,Features,List1),
	sum_of_the_list(List1,Sum),
	Distance is sqrt(Sum).


weighted_glanian_distance(Name1, Name2, Distance) :-
	expects(Name1,_,Expecteds),
	glanian(Name2,_,Features),	
	weight(Name1,Weights),
	subtract_e_f_weighted(Expecteds,Features,Weights,List1),
	sum_of_the_list(List1,Sum),
	Distance is sqrt(Sum).


find_possible_cities(Name,CityList) :-
	city(CityName,HabitantList,_),
	member(Name,HabitantList),
	!,
	likes(Name,_,LikedCities),
	append([CityName],LikedCities,CityList2),
	list_to_set(CityList2,CityList).


merge_possible_cities(Name1, Name2, MergedCities) :-
	find_possible_cities(Name1,CityList1),
	find_possible_cities(Name2,CityList2),
	union(CityList1,CityList2,MergedCities2),
	list_to_set(MergedCities2,MergedCities).


find_mutual_activities(Name1, Name2, MutualActivities) :-
	likes(Name1,LikedActivities1,_),
	likes(Name2,LikedActivities2,_),
	intersection(LikedActivities1, LikedActivities2, MutualActivities).
	

find_possible_targets(Name,SortedDistances,SortedTargets) :-
	findall(Target,is_gender_expected(Name,Target),TargetList),
	delete(TargetList, Name, TargetList2),
	find_distances_of_a_list(Name,TargetList2,DistanceList),
	create_dashed_list(PairedList,DistanceList,TargetList2),
	keysort(PairedList, DashedTemp),
	divide_dashed_list(DashedTemp,SortedDistances,SortedTargets).



find_weighted_targets(Name, SortedDistances, SortedTargets) :-
	findall(Target,is_gender_expected(Name,Target),TargetList),
	delete(TargetList, Name, TargetList2),
	find_weighted_distances_of_a_list(Name,TargetList2,DistanceList),
	create_dashed_list(PairedList,DistanceList,TargetList2),
	keysort(PairedList, DashedTemp),
	divide_dashed_list(DashedTemp,SortedDistances,SortedTargets).



find_my_best_target(Name, Distances, Activities, Cities, Targets) :-
	find_weighted_targets(Name,D,T),
	find_and_delete_old_relationships(Name,T,T1,D,D1),
	delete_ac_to_features(Name,T1,D1,T2,D2),
	delete_ac_to_intersection(Name,T2,T2half,D2,D2half),
	full_of_it(Name,T2half,D2half,T3,D3,FullPairsFTemp),
	flatten(T3, Targets),
	flatten(D3, Distances),
	flatten(FullPairsFTemp,FullPairsF),
	divide_dashed_list(FullPairsF,Activities,Cities).


find_my_best_match(Name, Distances, Activities, Cities, Targets):-
	find_weighted_targets(Name,D,T),
	find_avg_weighted_distance(Name,T,D,TargetOut,DistanceOut),
	sort_avg_weight_list(TargetOut,DistanceOut,SortedTargets,SortedDistances),
	find_and_delete_old_relationships(Name,SortedTargets,T1,SortedDistances,D1),

	delete_ac_to_features_match(Name,T1,D1,T2,D2),
	delete_ac_to_exp_gender_match(Name,T2,T3,D2,D3),
	delete_ac_to_intersection_match(Name,T3,T4,D3,D4),
	full_of_it_for_match(Name,T4,D4,T5,D5,FullPairsTemp),	
	flatten(T5, Targets),
	flatten(D5, Distances),
	flatten(FullPairsTemp,FullPairsF),
	divide_dashed_list(FullPairsF,Activities,Cities).
