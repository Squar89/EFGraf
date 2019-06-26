/**
 * Funkcja sprawdzajaca czy dana etykieta wierzcholka wystapila juz wczesniej
 */
brakPowtorzenEtykiet([], _) :- true.
brakPowtorzenEtykiet([node(Etykieta, _, _)|TWierz], ListaWierz) :-
    \+ member(Etykieta, ListaWierz),
    brakPowtorzenEtykiet(TWierz, [Etykieta|ListaWierz]).

/**
 * Funkcja zwracajaca zbior wierzcholkow (V) obecnego grafu
 */
wierzcholki([], []) :- true.
wierzcholki([node(Etykieta, _, _)|TWierz], [Etykieta|V]) :-
    wierzcholki(TWierz, V).

/**
 * Funkcja zwracajaca pelny zbior krawedzi skierowanych (E) obecnego grafu
 */
%krawedzieE() :- true.%TODO?

/**
 * Funkcja zwracajaca pelny zbior krawedzi nieskierowanych (F) obecnego grafu
 */
krawedzieF([], []) :- true.
krawedzieF([node(Etykieta, _, [])|TWierz], F) :-
    krawedzieF(TWierz, F).
krawedzieF([node(Etykieta, _, [HF|TF])|TWierz], [edgeF(Etykieta, HF)|F]) :-
    krawedzieF([node(Etykieta, _, TF)|TWierz], F).

/**
 * Funkcja sprawdzajaca czy dana lista krawedzi zawiera etykiety tylko ze zbioru V
 */
poprawneEtykiety([], _) :- true.
poprawneEtykiety([Etykieta|Krawedzie], V) :-
    member(Etykieta, V),
    poprawneEtykiety(Krawedzie, V).

/**
 * Funkcja sprawdzająca czy dla danej krawedzi nieskierowanej ta krawedz pojawila sie na listach
 * sasiedztwa obu wierzcholkow
 */
istniejeOdbitaKrawedz(Etykieta, [], _) :- true.
istniejeOdbitaKrawedz(Etykieta, [HF|TF], F) :- 
    member(edgeF(HF, Etykieta), F),
    istniejeOdbitaKrawedz(Etykieta, TF, F).

/**
 * Funkcja sprawdzająca poprawność krawędzi danego termu
 */
poprawneListySasiedztwa([], _, _) :- true.
poprawneListySasiedztwa([node(Etykieta, VELista, VFLista)|TWierz], V, F) :-
    poprawneEtykiety(VELista, V),
    poprawneEtykiety(VFLista, V),
    istniejeOdbitaKrawedz(Etykieta, VFLista, F),
    poprawneListySasiedztwa(TWierz, V, F).

/**
 * Funkcja sprawdzajaca czy podany term jest EFGrafem, a dokladniej sprawdza czy:
 * - Podany term jest lista
 * - Podany term ma format zgodny ze specyfikacja zadania
 * - W podanej liscie wystepuja powtorzenia etykiet dla zbioru V
 * - Etykieta podana na liscie sasiedztwa wystepuje w zbiorze V
 * - Krawedzie nieskierowane (ze zbioru F) wystepuja na liscie sasiedztwa dla obu wierzcholkow
 */
jestEFGrafem([]) :- true.
jestEFGrafem(Term) :-
    is_list(Term),
    brakPowtorzenEtykiet(Term, []),
    wierzcholki(Term, V),
    krawedzieF(Term, F),
    poprawneListySasiedztwa(Term, V, F).

/**
 * Funkcja sprawdzajaca czy podany EFGraf jest EFGrafem i czy jest dobrze ulozony
 */
jestDobrzeUlozony(EFGraf) :-
    jestEFGrafem(EFGraf),
    jestDobrzeUlozonyNieMusiBycEFGrafem(EFGraf).

/**
 * Funkcja sprawdzajaca czy podany EFGraf jest dobrze ulozony
 */
jestDobrzeUlozonyNieMusiBycEFGrafem(EFGraf) :-
    /** TODO
    findall(
        {X, Y},
        testXY,
        bag
    )
    forall(bag, test). */

/**
 * Funkcja sprawdzajaca czy podany EFGraf jest dobrze ulozony
 * (ale nie musi byc EFGrafem) i czy jest dobrze permutujacy
 */
jestDobrzePermutujacy(EFGraf) :-
    jestDobrzeUlozonyNieMusiBycEFGrafem(EFGraf).