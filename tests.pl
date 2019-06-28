%OK
:- jestEFGrafem([node(v, [], [])]).
:- jestEFGrafem([node(v4164, [], [v4186]), node(v4186, [], [v4164])]).
:- jestEFGrafem([node(v696, [], []), node(v710, [], [])]).
:- jestEFGrafem([node(v696, [], [v696])]).
:- jestEFGrafem([node(v696, [v696], [])]).
:- jestEFGrafem([node(v696, [v696], [v696])]).
:- jestEFGrafem([node(v700, [], []), node(v714, [], []), node(v728, [], [])]).
:- jestEFGrafem([node(v700, [], []), node(v714, [], [v714])]).
:- jestEFGrafem([node(v700, [], []), node(v714, [v700], [])]).
:- jestEFGrafem([node(v700, [], []), node(v714, [v714], [])]).
:- jestEFGrafem([node(v700, [], []), node(v714, [v700], [v714])]).
:- jestEFGrafem([node(v700, [], []), node(v714, [v714], [v714])]).
:- jestEFGrafem([node(v700, [], [v700]), node(v734, [], [])]).
:- jestEFGrafem([node(v700, [], [v700, v700])]).
:- jestEFGrafem([node(v700, [v700], []), node(v734, [], [])]).
:- jestEFGrafem([node(v700, [v708], []), node(v708, [], [])]).
:- jestEFGrafem([node(v700, [v700, v700], [])]).
:- jestEFGrafem([node(v700, [v700], [v700]), node(v740, [], [])]).
:- jestEFGrafem([node(v700, [v708], [v700]), node(v708, [], [])]).
:- jestEFGrafem([node(v700, [v700], [v700, v700])]).
:- jestEFGrafem([node(v700, [v700, v700], [v700])]).
:- jestEFGrafem([node(v700, [v700, v700], [v700, v700])]).
:- jestEFGrafem([node(v704, [], []), node(v718, [], []), node(v732, [], []), node(v746, [], [])]).
:- jestEFGrafem([node(v704, [], []), node(v718, [], []), node(v732, [], [v732])]).
:- jestEFGrafem([node(v704, [], []), node(v718, [], []), node(v732, [v704], [])]).
:- jestEFGrafem([node(v704, [], []), node(v718, [], []), node(v732, [v718], [])]).
:- jestEFGrafem([node(v704, [], []), node(v718, [], []), node(v732, [v732], [])]).
:- jestEFGrafem([node(v704, [], []), node(v718, [], []), node(v732, [v704], [v732])]).
:- jestEFGrafem([node(v704, [], []), node(v718, [], []), node(v732, [v718], [v732])]).
:- jestEFGrafem([node(v704, [], []), node(v718, [], []), node(v732, [v732], [v732])]).
:- jestEFGrafem([node(v704, [], []), node(v718, [], [v718]), node(v752, [], [])]).
:- jestEFGrafem([node(v704, [], []), node(v718, [], [v718, v718])]).
:- jestEFGrafem([node(v704, [], []), node(v718, [v704], []), node(v752, [], [])]).
:- jestEFGrafem([node(v704, [], []), node(v718, [v718], []), node(v752, [], [])]).
:- jestEFGrafem([node(v704, [], []), node(v718, [v726], []), node(v726, [], [])]).
:- jestEFGrafem([node(v704, [], []), node(v718, [v704, v704], [])]).
:- jestEFGrafem([node(v704, [], []), node(v718, [v704, v718], [])]).
:- jestEFGrafem([node(v704, [], []), node(v718, [v718, v704], [])]).
:- jestEFGrafem([node(v704, [], []), node(v718, [v718, v718], [])]).
:- jestEFGrafem([node(v704, [], []), node(v718, [v704], [v718]), node(v758, [], [])]).
:- jestEFGrafem([node(v704, [], []), node(v718, [v718], [v718]), node(v758, [], [])]).
:- jestEFGrafem([node(v704, [], []), node(v718, [v726], [v718]), node(v726, [], [])]).
:- jestEFGrafem([node(v704, [], []), node(v718, [v704], [v718, v718])]).
:- jestEFGrafem([node(v704, [], []), node(v718, [v718], [v718, v718])]).
:- jestEFGrafem([node(v704, [], []), node(v718, [v704, v704], [v718])]).
:- jestEFGrafem([node(v704, [], []), node(v718, [v704, v718], [v718])]).
:- jestEFGrafem([node(v704, [], []), node(v718, [v718, v704], [v718])]).
:- jestEFGrafem([node(v704, [], []), node(v718, [v718, v718], [v718])]).
:- jestEFGrafem([node(v704, [], []), node(v718, [v704, v704], [v718, v718])]).
:- jestEFGrafem([node(v704, [], []), node(v718, [v704, v718], [v718, v718])]).
:- jestEFGrafem([node(v704, [], []), node(v718, [v718, v704], [v718, v718])]).
:- jestEFGrafem([node(v704, [], []), node(v718, [v718, v718], [v718, v718])]).


%NOK
:- \+ jestEFGrafem([node(v4164, [], [v4188]), node(v4186, [], []),b]).

:- \+ jestEFGrafem([node(v4164, [], [v4188]), node(v4186, [], [])]).
:- \+ jestEFGrafem([node(v4164, [v4188], []), node(v4186, [], [])]).
:- \+ jestEFGrafem([node(v4164, [], []), node(v4186, [v4188], [])]).
:- \+ jestEFGrafem([node(v4164, [], [v4188]), node(v4186, [], [])]).
:- \+ jestEFGrafem([node(v4164, [], [v4186]), node(v4186, [], [v4188])]).

:- \+ jestEFGrafem([node(v4168, [], [v4182]), node(v4182, [], []), node(v4214, [], [])]).
:- \+ jestEFGrafem([node(v4168, [], [v4182]), node(v4182, [v4168], []), node(v4214, [], [])]).
:- \+ jestEFGrafem([node(v4168, [], [v4214,v4182]), node(v4182, [v4168], [v4168]), node(v4214, [], [])]).

:- \+ jestEFGrafem([node(v4168, [], []), node(v4182, [], [v4168]), node(v4214, [], [])]).

:- \+ jestEFGrafem([node(v704, [], []), node(v718, [v718, v718], [v704, v718])]).

:- \+ jestEFGrafem([node(v704, [], []), node(v704, [], []),node(v718, [v718, v718], [v704, v718])]).
:- \+ jestEFGrafem([node(v704, [], []), node(v718, [v718, v718], [v704, v718]),node(v718, [v718, v718], [v704, v718])]).

%OK
:- jestDobrzeUlozony([node(v1926, [], []), node(v1940, [v1926, v1926],[])]).
:- jestDobrzeUlozony([node(v988, [v1002], []), node(v1002, [v1016], []), node(v1016, [v1002,v1030], []), node(v1030, [v1044], []), node(v1044, [], [])]).
:- jestDobrzeUlozony([node(v988, [v1002], []), node(v1002, [v1016,v1030], []), node(v1016, [], []), node(v1030, [v1002], [])]).
:- jestDobrzeUlozony([node(v984, [v998,v1026], [v998,v1026]), node(v998, [v1012,v1026], [v984]), node(v1012, [v1026], []), node(v1026, [], [v984])]).
:- jestDobrzeUlozony([node(v984, [v998,v1026], [v998,v1026]), node(v998, [v1012,v1026], [v984]), node(v1012, [v1026,v998], []), node(v1026, [], [v984])]).
:- jestDobrzeUlozony([node(v984, [v998,v1026], [v998,v1026,v1012]), node(v998, [v1012,v1026,v1012,v1012], [v984]), node(v1012, [v1026,v998], [v984]), node(v1026, [], [v984])]).

%NOK
:- \+ jestDobrzeUlozony([node(v1926, [v1940], []), node(v1940, [v1926, v1926],[])]).
:- \+ jestDobrzeUlozony([node(v988, [], []), node(v1002, [], []), node(v1016, [], []), node(v1030, [v988], [])]).
:- \+ jestDobrzeUlozony([node(v1926, [v1940], []), node(v1940, [v1926],[])]).
:- \+ jestDobrzeUlozony([node(v984, [v998,v1012], []), node(v998, [v1026], []), node(v1012, [v1026], []), node(v1026, [], [])]).
:- \+ jestDobrzeUlozony([node(v984, [v998,v1026], [v998,v1026]), node(v998, [v1012,v1026,v984], [v984]), node(v1012, [v1026], []), node(v1026, [], [v984])]).
:- \+ jestDobrzeUlozony([node(v984, [v998,v1026], [v998,v1026,v1012]), node(v998, [v1012,v1026,v1012,v1012], [v984]), node(v1012, [v1026,v998], []), node(v1026, [], [v984])]).
%Wlasne
:- \+ jestDobrzeUlozony([node(v1926, [], [v1942]), node(v1940, [v1926, v1926],[v1942]), node(v1941, [v1941], [v1942]), node(v1942, [v1942], [v1926, v1940, v1941, v1942])]).

%OK
:- jestDobrzePermutujacy([node(v1926, [], []), node(v1940, [v1926, v1926],[])]).
:- jestDobrzePermutujacy([node(v988, [v1002], []), node(v1002, [v1016], []), node(v1016, [v1002,v1030], []), node(v1030, [v1044], []), node(v1044, [], [])]).
:- jestDobrzePermutujacy([node(v988, [v1002], []), node(v1002, [v1016,v1030], []), node(v1016, [], []), node(v1030, [v1002], [])]).
:- jestDobrzePermutujacy([node(v984, [v998], [v998]), node(v998, [v1012], [v1012]), node(v1012, [], [v998])]).
:- jestDobrzePermutujacy([node(v984, [v998], [v998]), node(v998, [v1012], [v1012]), node(v1012, [v1026], [v1026]),node(v1026, [], [v1012])]).


%NOK
:- \+ jestDobrzePermutujacy([node(v984, [v998,v1026], [v998,v1026]), node(v998, [v1012,v1026], [v984]), node(v1012, [v1026], []), node(v1026, [], [v984])]).
:- \+ jestDobrzePermutujacy([node(v984, [v998,v1026], [v998,v1026]), node(v998, [v1012,v1026], [v984]), node(v1012, [v1026,v998], []), node(v1026, [], [v984])]).
:- \+ jestDobrzePermutujacy([node(v984, [v998,v1026], [v998,v1026,v1012]), node(v998, [v1012,v1026,v1012,v1012], [v984]), node(v1012, [v1026,v998], [v984]), node(v1026, [], [v984])]).
:- \+ jestDobrzePermutujacy([node(v984, [v998,v1026], [v998,v1026,v1012]), node(v998, [v1012,v1026,v1012,v1012], [v984]), node(v1012, [v1026,v998], [v984]), node(v1026, [], [v984])]).

%OK
:- jestSucc([node(v1926, [], []), node(v1940, [v1926, v1926],[])],[],[]).
:- jestSucc([node(v1926, [], []), node(v1940, [v1926, v1926],[])],[],[v1940]).
:- jestSucc([node(v984, [v998], [v998]), node(v998, [v1012], [v1012]), node(v1012, [], [v998])],[v984],[v998]).
:- jestSucc([node(v984, [v998], [v998]), node(v998, [v1012], [v1012]), node(v1012, [], [v998])],[v984],[v998,v1012]).
:- jestSucc([node(v984, [v998], [v998]), node(v998, [v1012], [v1012]), node(v1012, [v1026], [v1026]),node(v1026, [], [v1012])],[v984],[v998]).
:- jestSucc([node(v984, [v998], [v998]), node(v998, [v1012], [v1012]), node(v1012, [v1026], [v1026]),node(v1026, [], [v1012])],[v984,v998],[v998,v1012]).
:- jestSucc([node(v984, [v998], [v998]), node(v998, [v1012], [v1012]), node(v1012, [v1026], [v1026]),node(v1026, [], [v1012])],[v984,v998,v1012],[v998,v1012,v1026]).


%NOK
:- \+ jestSucc([node(v984, [v998], [v998]), node(v998, [v1012], [v1012]), node(v1012, [], [v998])],[v984],[v984]).
:- \+ jestSucc([node(v984, [v998], [v998]), node(v998, [v1012], [v1012]), node(v1012, [v1026], [v1026]),node(v1026, [], [v1012])],[v984,v998,v1012],[v998,v1012]).
:- \+ jestSucc([node(v984, [v998], [v998]), node(v998, [v1012], [v1012]), node(v1012, [v1026], [v1026]),node(v1026, [], [v1012])],[v984,v998,v1012],[]).
:- \+ jestSucc([node(v984, [v998], [v998]), node(v998, [v1012], [v1012]), node(v1012, [v1026], [v1026]),node(v1026, [], [v1012])],[v984,v1012,v998],[v998,v1012,v1026]).
:- \+ jestSucc([node(v984, [v998], [v998]), node(v998, [v1012], [v1012]), node(v1012, [v1026], [v1026]),node(v1026, [], [v1012])],[v984,v998,v1012],[v998,v1026,v1012]).