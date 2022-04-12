#!/usr/bin/env swipl
/*  Author:     Craig Opie
    Email:      opieca@hawaii.edu
    Assignment: No. 5
    References: https://www.swi-prolog.org/pldoc/doc_for?object=dif/2

    Test Cases: top([is, it, true, that, mark, hamill, acts, in, star, wars, iv]).
                top([who, acts, in, star, wars, iv]).
*/

/*
    should write out a sensible answer regardless if it passes or fails
    it really should write out the text of the answer, not just the symbol.
*/
top(Sentence) :- yesno(Query, Sentence, []), showresults(Query).
top(Sentence) :- who(Who, Sentence, []), write("The person you're looking for is "), write(Who).



/*
    writes out positive text if ARG1 is a list of true predicates, negative otherwise.
*/
showresults(Query) :- test(Query), write("Yes, that's true.").
showresults(_) :- test(Query), write("Sorry, that's false.").



/*
    takes a list of predicates and succeeds if all the predicates are true, otherwise fails.
*/
test([Query]) :- Query.
test([Query|Rest]) :- Query, test(Rest).


/*
    grammer section
*/
who(X) --> [who], verb_phrase(X^_^[Query]), {Query}.
yesno(Sem) --> [is, it, true, that], statement(_^_^Sem). 

statement(S) --> singlestatement(S).
statement(_^_^Sem) --> singlestatement(_^_^S1), [and], statement(_^_^S2), {append(S1,S2,Sem)}.

singlestatement(Subj^Obj^Sem) --> noun_phrase(Subj), verb_phrase(Subj^Obj^Sem).

noun_phrase(Sem) --> proper_noun(Sem).
verb_phrase(Subj^Obj^Sem) --> verb(Subj^Obj^Sem), noun_phrase(Obj).

proper_noun(liam_neeson) --> [liam, neeson].
proper_noun(ewan_mcgregor) --> [ewan, mcgregor].
proper_noun(natalie_portman) --> [natalie, portman].
proper_noun(jake_lloyd) --> [jake, lloyd].
proper_noun(ian_mcdiarmid) --> [ian, mcdiarmid].
proper_noun(pernilla_august) --> [pernilla, august].
proper_noun(mark_hamill) --> [mark, hamill].
proper_noun(harrison_ford) --> [harrison, ford].
proper_noun(carrie_fisher) --> [carrie, fisher].
proper_noun(alec_guinness) --> [alec, guinness].
proper_noun(peter_cushing) --> [peter, cushing].
proper_noun(anthony_daniels) --> [anthony, daniels].
proper_noun(kenny_baker) --> [kenny, baker].
proper_noun(peter_mayhew) --> [peter, mayhew].
proper_noun(david_prowse) --> [david, prowse].
proper_noun(phil_brown) --> [phil, brown].
proper_noun(shelagh_fraser) --> [shelagh, fraser].
proper_noun(jack_purvis) --> [jack, purvis].

proper_noun(george_lucas) --> [george, lucas].
proper_noun(j_j_abrams) --> [j, j, abrams].
proper_noun(rian_johnson) --> [rian, johnson].

proper_noun(star_wars1) --> [star, wars, i].
proper_noun(star_wars2) --> [star, wars, ii].
proper_noun(star_wars3) --> [star, wars, iii].
proper_noun(star_wars4) --> [star, wars, iv].
proper_noun(star_wars5) --> [star, wars, v].
proper_noun(star_wars6) --> [star, wars, vi].
proper_noun(star_wars7) --> [star, wars, vii].
proper_noun(star_wars8) --> [star, wars, viii].
proper_noun(star_wars9) --> [star, wars, ix].

verb(X^Y^[acts_in(X,Y)]) --> [acts, in].


/*
    database
*/
actor(liam_neeson).
actor(ewan_mcgregor).
actor(natalie_portman).
actor(jake_lloyd).
actor(ian_mcdiarmid).
actor(pernilla_august).

actor(mark_hamill).
actor(harrison_ford).
actor(carrie_fisher).
actor(alec_guinness).
actor(peter_cushing).
actor(anthony_daniels).
actor(kenny_baker).
actor(peter_mayhew).
actor(david_prowse).
actor(phil_brown).
actor(shelagh_fraser).
actor(jack_purvis).

director(george_lucas).
director(j_j_abrams).
director(rian_johnson).

writer(george_lucas).

acts_in(liam_neeson, star_wars1).
acts_in(ewan_mcgregor, star_wars1).
acts_in(ewan_mcgregor, star_wars2).
acts_in(ewan_mcgregor, star_wars3).
acts_in(natalie_portman, star_wars1).
acts_in(natalie_portman, star_wars2).
acts_in(natalie_portman, star_wars3).
acts_in(jake_lloyd, star_wars1).
acts_in(ian_mcdiarmid, star_wars1).
acts_in(pernilla_august, star_wars1).
acts_in(mark_hamill, star_wars4).
acts_in(mark_hamill, star_wars5).
acts_in(mark_hamill, star_wars6).
acts_in(harrison_ford, star_wars4).
acts_in(harrison_ford, star_wars5).
acts_in(harrison_ford, star_wars6).
acts_in(harrison_ford, star_wars7).
acts_in(carrie_fisher, star_wars4).
acts_in(carrie_fisher, star_wars5).
acts_in(carrie_fisher, star_wars6).
acts_in(carrie_fisher, star_wars7).
acts_in(alec_guinness, star_wars8).
acts_in(peter_cushing, star_wars4).
acts_in(anthony_daniels, star_wars1).
acts_in(anthony_daniels, star_wars4).
acts_in(kenny_baker, star_wars1).
acts_in(kenny_baker, star_wars4).
acts_in(peter_mayhew, star_wars4).
acts_in(david_prowse, star_wars4).
acts_in(phil_brown, star_wars4).
acts_in(shelagh_fraser, star_wars4).
acts_in(jack_purvis, star_wars4).

plays(liam_neeson, qui-gon_jinn).
plays(ewan_mcgregor, obi-wan_kenobi).
plays(natalie_portman, queen_amidala).
plays(natalie_portman, padme).
plays(jake_lloyd, anakin_skywalker).
plays(ian_mcdiarmid, senator_palpatine).
plays(pernilla_august, shmi_skywalker).
plays(mark_hamill, luke_skywalker).
plays(harrison_ford, han_solo).
plays(carrie_fisher, princess_leia).
plays(alec_guinness, obi-wan_kenobi).
plays(peter_cushing, grand_moff_tarkin).
plays(anthony_daniels, c3po).
plays(kenny_baker, r2d2).
plays(peter_mayhew, chewbacca).
plays(david_prowse, darth_vader).
plays(phil_brown, uncle_owen).
plays(shelagh_fraser, aunt_beru).
plays(jack_purvis, chief_jawa).

directs(star_wars1, george_lucas).
directs(star_wars2, george_lucas).
directs(star_wars3, george_lucas).
directs(star_wars4, george_lucas).
directs(star_wars5, george_lucas).
directs(star_wars6, george_lucas).
directs(star_wars7, j_j_abrams).
directs(star_wars8, rian_johnson).
directs(star_wars9, j_j_abrams).

writes(star_wars1, george_lucas).
writes(star_wars2, george_lucas).
writes(star_wars3, george_lucas).
writes(star_wars4, george_lucas).
writes(star_wars5, george_lucas).
writes(star_wars6, george_lucas).
writes(star_wars7, j_j_abrams).
writes(star_wars8, rian_johnson).
writes(star_wars9, j_j_abrams).

title(star_wars1, the_phantom_menace).
title(star_wars2, attack_of_the_clones).
title(star_wars3, revenge_of_the_sith).
title(star_wars4, a_new_hope).
title(star_wars5, the_empire_strikes_back).
title(star_wars6, return_of_the_jedi).
title(star_wars7, the_force_awakens).
title(star_wars8, the_last_jedi).
title(star_wars9, the_rise_of_skywalker).