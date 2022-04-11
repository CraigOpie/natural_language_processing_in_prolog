# natural_language_processing_in_prolog
Natural Language Processing in Prolog

##Instructions
###Natural Language Processing in Prolog

Rules: Cite any sources you use, and write your own comments and error messages. Do not use any built-in predicates (aside from arithmetic operators, write/1 and format/2).

See Ass5.pl (under "Resources"). This will give you a head start on this assignment. You’ll have to modify it a lot, though!

Your task is to write a DCG and a database (i.e. a set of predicates) to answer questions about the Star Wars movies. Use IMDB.com as your source. Your database should cover any three movies in the series, and include information about five actors/characters for each movie, as well as its director. For consistency (and for the geeks amongst us, including myself), "The Phantom Menace" is Episode 1, and "A New Hope" is Episode 4. 

The top level predicate (top/1) should take a wide range of questions (represented in list form) as its argument, and write out a good answer to the question. It should write out “I don’t get it” if the question is ungrammatical. If the question is grammatical but nonsensical (e.g. “Did Star Wars IV play Luke?”) it should simply give a negative response. If there are multiple answers to a question, forced backtracking (i.e. hitting “;”) should produce them.

You should handle agreement with features.

Grammatical queries:

“Did Mark Hamill play Luke Skywalker?”

“Mark Hamill plays Luke Skywalker, right?”

“Did Mark Hamill play Han Solo in Star Wars III?”

“Did Mark Hamill direct Star Wars I?”

“Is it true that Mark Hamill is an actor?”

"Is it true that Han Solo is a character in Star Wars III?"

“Is it true that Princess Leia is an actor and Mark Hamill is a director?”

“George Lucas is a director, Mark Hamill is an actor and Han Solo is a character, right?” [should be able to handle any number of subqueries]

“Who is the actor for Han Solo?”

“What is the title of Star Wars II?”

“Who is the director of Star Wars II?”

“Who is the character of Mark Hamill?”

Ungrammatical queries (examples):

* “Did Mark Hamill plays Luke Skywalker?”

* “Is it true that Mark Hamill is a actor?”

* “Mark Hamill play Chewbacca, right?”

* “George Lucas is an director, right?”

###Bonus (10 points)

Impress me – be creative! Allow a wider range of queries than specified above, without allowing ungrammatical queries. To get full points, provide a list of grammatical queries allowed by your expanded DCG, as well as a list of ungrammatical queries it should reject.

###What to submit

Turn in the following (all via Laulima):

All code, appropriately commented.

All transcripts, showing that the code performs as required on a representative set of inputs. 

Additional resources for assignment
No attachments yet