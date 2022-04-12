#!/usr/bin/env swipl
/*  Author:     Craig Opie
    Email:      opieca@hawaii.edu
    Assignment: No. 5
    References: https://www.swi-prolog.org/pldoc/doc_for?object=dif/2

    Test Cases: top([is, it, true, that, mark, hamill, acts, in, star, wars, iv]).
                top([who, acts, in, star, wars, iv]).
                top([did, mark, hamill, play, luke, skywalker]).
                top([mark, hamill, plays, luke, skywalker, right]).
                top([did, mark, hamill, play, han, solo, in, star, wars, iii]).
                top([did, mark, hamill, direct, star, wars, i]).
                top([is, it, true, that, mark, hamill, is, an, actor]).
                top([is, it, true, that, han, solo, is, a, character, in, star, wars, iii]).
                top([is, it, true, that, princess, leia, is, an, actor, and, mark, hamill, is, a, director]).
                top([george, lucas, is, a, director, and, mark, hamill, is, an, actor, and, han, solo, is, a, character, right]).
                top([who, is, the, actor, for, han, solo]).
                top([what, is, the, title, of, star, wars, ii]).
                top([who, is, the, director, of, star, wars, ii]).
                top([who, is, the, character, of, mark, hamill]).
*/

/*
    should write out a sensible answer regardless if it passes or fails
    it really should write out the text of the answer, not just the symbol.
*/
%% Overall sentence structures
top(Sentence) :- yesno(Query, Sentence, []), showresults(Query).
top(Sentence) :- who(Who, Sentence, []), write("The person you're looking for is "), write(Who). 
top(Sentence) :- what(What, Sentence, []), write("You're looking for "), write(What).


/*
    writes out positive text if ARG1 is a list of true predicates, negative otherwise.
*/
%% Test if the value is true and write a statement as such, else write a statement that its not
showresults(Query) :- test(Query), write("Yes, that's true."), !.
showresults(_) :- write("Sorry, that's false."), !, fail.


/*
    takes a list of predicates and succeeds if all the predicates are true, otherwise fails.
*/
%% Perform logical tests on list
test([Query]) :- Query.
test([Query|Rest]) :- Query, test(Rest).


/*
    grammer section
*/
%% Who question breakdown and returns a list
who(X) --> [who, is, the], verb_phrase(X^_^[Query]), {Query}.
who(X) --> [who], verb_phrase(X^_^[Query]), {Query}.

%% What question breakdown and returns a list
what(X) --> [what, is, the], verb_phrase(X^_^[Query]), {Query}.
what(X) --> [what], verb_phrase(X^_^[Query]), {Query}.

%% True false statements that return true or false
yesno(Sem) --> [is, it, true, that], statement(_^_^Sem).
yesno(Sem) --> [is, the], statement(_^_^Sem).
yesno(Sem) --> [did], statement(_^_^Sem).
yesno(Sem) --> statement(_^_^Sem), [right].
yesno(Sem) --> statement(_^_^Sem).

%% Evaluates the statement based on structure
statement(S) --> singlestatement(S).
statement(_^_^Sem) --> singlestatement(_^_^S1), [and], statement(_^_^S2), {append(S1,S2,Sem)}.
statement(Subj^_^Sem) --> singlestatement(Subj^_^S1), prep_phrase(Subj^_^S2), {append(S1,S2,Sem)}.

%% Defines a single statement as consisting of a noun phrase followed by a verb phrase
singlestatement(Subj^Obj^Sem) --> noun_phrase(Subj), verb_phrase(Subj^Obj^Sem).

%% Defines a noun phrase consisting of a noun or proper noun
noun_phrase(Sem) --> noun(Sem).
noun_phrase(Sem) --> proper_noun(Sem).

%% Defines a verb phrase consisting of a verb followed by a noun phrase
verb_phrase(Subj^Obj^Sem) --> verb(Subj^Obj^Sem), noun_phrase(Obj).

%% Defines a prep phrase consisting of a verb followed by a noun phrase
prep_phrase(Subj^Obj^Sem) --> verb(Subj^Obj^Sem), noun_phrase(Obj).

%% Defines the proper nouns in the database
%  This section is defining actor names
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

%  This section is defining character names
proper_noun(qui-gon_jinn) --> [quigon, jinn].
proper_noun(obi-wan_kenobi) --> [obiwan, kenobi].
proper_noun(queen_amidala) --> [queen, amidala].
proper_noun(padme) --> [padme].
proper_noun(anakin_skywalker) --> [anakin, skywalker].
proper_noun(senator_palpatine) --> [senator, palpatine].
proper_noun(shmi_skywalker) --> [shmi, skywalker].
proper_noun(luke_skywalker) --> [luke, skywalker].
proper_noun(han_solo) --> [han, solo].
proper_noun(princess_leia) --> [princess, leia].
proper_noun(grand_moff_tarkin) --> [grand, moff, tarkin].
proper_noun(c3po) --> [c3po].
proper_noun(r2d2) --> [r2d2].
proper_noun(chewbacca) --> [chewbacca].
proper_noun(darth_vader) --> [darth, vader].
proper_noun(uncle_owen) --> [uncle, owen].
proper_noun(aunt_beru) --> [aunt, beru].
proper_noun(chief_jawa) --> [chief, jawa].

%  This section is defining director names
proper_noun(george_lucas) --> [george, lucas].
proper_noun(j_j_abrams) --> [j, j, abrams].
proper_noun(rian_johnson) --> [rian, johnson].

%  This section is defining movie title names
proper_noun(star_wars1) --> [star, wars, i].
proper_noun(star_wars2) --> [star, wars, ii].
proper_noun(star_wars3) --> [star, wars, iii].
proper_noun(star_wars4) --> [star, wars, iv].
proper_noun(star_wars5) --> [star, wars, v].
proper_noun(star_wars6) --> [star, wars, vi].
proper_noun(star_wars7) --> [star, wars, vii].
proper_noun(star_wars8) --> [star, wars, viii].
proper_noun(star_wars9) --> [star, wars, ix].

%  This section is defining the movie title names
proper_noun(the_phantom_menace) --> [the, phantom, menace].
proper_noun(attack_of_the_clones) --> [attack, of, the, clones].
proper_noun(revenge_of_the_sith) --> [revenge, of, the, sith].
proper_noun(a_new_hope) --> [a, new, hope].
proper_noun(the_empire_strikes_back) --> [the, empire, strikes, back].
proper_noun(return_of_the_jedi) --> [return, of, the, jedi].
proper_noun(the_force_awakens) --> [the, force, awakens].
proper_noun(the_last_jedi) --> [the, last, jedi].
proper_noun(the_rise_of_skywalker) --> [the, rise, of, skywalker].

%% Defines the nouns in the database
noun(actor) --> [actor].
noun(character) --> [character].
noun(director) --> [director].
noun(writer) --> [writer].

%% Defines the verbs in the database and assigns their function
verb(X^Y^[is_a(X,Y)]) --> [is, a].
verb(X^Y^[is_a(X,Y)]) --> [is, an].
verb(X^Y^[title(X,Y)]) --> [title, of].
verb(X^Y^[title(X,Y)]) --> [title].
verb(X^Y^[acts_in(X,Y)]) --> [acts, in].
verb(X^Y^[acts_in(X,Y)]) --> [act, in].
verb(X^Y^[acts_in(X,Y) ; char_in(X,Y)]) --> [in].
verb(X^Y^[plays(X,Y)]) --> [actor, for].
verb(X^Y^[played_by(X,Y)]) --> [character, of].
verb(X^Y^[plays(X,Y)]) --> [plays].
verb(X^Y^[plays(X,Y)]) --> [play].
verb(X^Y^[plays(X,Y)]) --> [played].
verb(X^Y^[wrote(X,Y)]) --> [wrote].
verb(X^Y^[wrote(X,Y)]) --> [writes].
verb(X^Y^[wrote(X,Y)]) --> [write].
verb(X^Y^[directed(X,Y)]) --> [director, of].
verb(X^Y^[directed(X,Y)]) --> [director].
verb(X^Y^[directed(X,Y)]) --> [directed].
verb(X^Y^[directed(X,Y)]) --> [directs].
verb(X^Y^[directed(X,Y)]) --> [direct].


/*
    database
*/
%% Actors
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

%% Directors
director(george_lucas).
director(j_j_abrams).
director(rian_johnson).

%% Writers
writer(george_lucas).

%% Roles
%  Actors
is_a(liam_neeson, actor).
is_a(ewan_mcgregor, actor).
is_a(natalie_portman, actor).
is_a(jake_lloyd, actor).
is_a(ian_mcdiarmid, actor).
is_a(pernilla_august, actor).
is_a(mark_hamill, actor).
is_a(harrison_ford, actor).
is_a(carrie_fisher, actor).
is_a(alec_guinness, actor).
is_a(peter_cushing, actor).
is_a(anthony_daniels, actor).
is_a(kenny_baker, actor).
is_a(peter_mayhew, actor).
is_a(david_prowse, actor).
is_a(phil_brown, actor).
is_a(shelagh_fraser, actor).
is_a(jack_purvis, actor).

%  Characters
is_a(qui-gon_jinn, character).
is_a(obi-wan_kenobi, character).
is_a(queen_amidala, character).
is_a(padme, character).
is_a(anakin_skywalker, character).
is_a(senator_palpatine, character).
is_a(shmi_skywalker, character).
is_a(luke_skywalker, character).
is_a(han_solo, character).
is_a(princess_leia, character).
is_a(grand_moff_tarkin, character).
is_a(c3po, character).
is_a(r2d2, character).
is_a(chewbacca, character).
is_a(darth_vader, character).
is_a(uncle_owen, character).
is_a(aunt_beru, character).
is_a(chief_jawa, character).

%  Directors
is_a(george_lucas, director).
is_a(j_j_abrams, director).
is_a(rian_johnson, director).

%  Writers
is_a(george_lucas, writer).

%% Assigns actors to movies
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

%% Assigns characters to movies
char_in(qui-gon_jinn, star_wars1).
char_in(obi-wan_kenobi, star_wars1).
char_in(obi-wan_kenobi, star_wars2).
char_in(obi-wan_kenobi, star_wars3).
char_in(queen_amidala, star_wars1).
char_in(queen_amidala, star_wars2).
char_in(queen_amidala, star_wars3).
char_in(padme, star_wars1).
char_in(padme, star_wars2).
char_in(padme, star_wars3).
char_in(anakin_skywalker, star_wars1).
char_in(senator_palpatine, star_wars1).
char_in(shmi_skywalker, star_wars1).
char_in(luke_skywalker, star_wars4).
char_in(luke_skywalker, star_wars5).
char_in(luke_skywalker, star_wars6).
char_in(han_solo, star_wars4).
char_in(han_solo, star_wars5).
char_in(han_solo, star_wars6).
char_in(han_solo, star_wars7).
char_in(princess_leia, star_wars4).
char_in(princess_leia, star_wars5).
char_in(princess_leia, star_wars6).
char_in(princess_leia, star_wars7).
char_in(obi-wan_kenobi, star_wars4).
char_in(grand_moff_tarkin, star_wars4).
char_in(c3po, star_wars1).
char_in(c3po, star_wars4).
char_in(r2d2, star_wars1).
char_in(r2d2, star_wars4).
char_in(chewbacca, star_wars4).
char_in(darth_vader, star_wars4).
char_in(uncle_owen, star_wars4).
char_in(aunt_beru, star_wars4).
char_in(chief_jawa, star_wars4).

%% Assigns actors to characters
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

%% Assigns characters to actors
played_by(qui-gon_jinn, liam_neeson).
played_by(obi-wan_kenobi, ewan_mcgregor).
played_by(queen_amidala, natalie_portman).
played_by(padme, natalie_portman).
played_by(anakin_skywalker, jake_lloyd).
played_by(senator_palpatine, ian_mcdiarmid).
played_by(shmi_skywalker, pernilla_august).
played_by(luke_skywalker, mark_hamill).
played_by(han_solo, harrison_ford).
played_by(princess_leia, carrie_fisher).
played_by(grand_moff_tarkin, peter_cushing).
played_by(c3po, anthony_daniels).
played_by(r2d2, kenny_baker).
played_by(chewbacca, peter_mayhew).
played_by(darth_vader, david_prowse).
played_by(uncle_owen, phil_brown).
played_by(aunt_beru, shelagh_fraser).
played_by(chief_jawa, jack_purvis).

%% Assings directors to films
directed(george_lucas, star_wars1).
directed(george_lucas, star_wars2).
directed(george_lucas, star_wars3).
directed(george_lucas, star_wars4).
directed(george_lucas, star_wars5).
directed(george_lucas, star_wars6).
directed(j_j_abrams, star_wars7).
directed(rian_johnson, star_wars8).
directed(j_j_abrams, star_wars9).

%% Assigns writers to films
wrote(george_lucas, star_wars1).
wrote(george_lucas, star_wars2).
wrote(george_lucas, star_wars3).
wrote(george_lucas, star_wars4).
wrote(george_lucas, star_wars5).
wrote(george_lucas, star_wars6).
wrote(j_j_abrams, star_wars7).
wrote(rian_johnson, star_wars8).
wrote(j_j_abrams, star_wars9).

%% Assigns titles to films
title(the_phantom_menace, star_wars1).
title(attack_of_the_clones, star_wars2).
title(revenge_of_the_sith, star_wars3).
title(a_new_hope, star_wars4).
title(the_empire_strikes_back, star_wars5).
title(return_of_the_jedi, star_wars6).
title(the_force_awakens, star_wars7).
title(the_last_jedi, star_wars8).
title(the_rise_of_skywalker, star_wars9).