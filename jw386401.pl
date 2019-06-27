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
krawedzieE([], []) :- true.
krawedzieE([node(_, [], _)|TWierz], E) :-
    krawedzieE(TWierz, E).
krawedzieE([node(Etykieta, [HE|TE], _)|TWierz], [edgeE(Etykieta, HE)|E]) :-
    krawedzieE([node(Etykieta, TE, _)|TWierz], E).

/**
 * Funkcja zwracajaca pelny zbior krawedzi nieskierowanych (F) obecnego grafu
 */
krawedzieF([], []) :- true.
krawedzieF([node(_, _, [])|TWierz], F) :-
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
istniejeOdbitaKrawedz(_, [], _) :- true.
istniejeOdbitaKrawedz(Etykieta, [HF|TF], F) :- 
    member(edgeF(HF, Etykieta), F),
    istniejeOdbitaKrawedz(Etykieta, TF, F).

/**
 * Funkcja sprawdzająca poprawność krawedzi danego termu
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
    krawedzieF(Term, DupF),
    sort(DupF, F),
    poprawneListySasiedztwa(Term, V, F).

/**
 * Funkcja sprawdzajaca czy podany EFGraf jest EFGrafem i czy jest dobrze ulozony
 */
jestDobrzeUlozony(EFGraf) :-
    %TODOjestEFGrafem(EFGraf),
    jestDobrzeUlozonyNieMusiBycEFGrafem(EFGraf).


/**
 * Funkcja sprawdzajaca pierwszy warunek dobrze ulozonego grafu, czyli:
 *      Istnieje dokladnie jedna para roznych wierzcholkow, vs,ve, takich ze nie ma
 *      E-krawedzi postaci (v, vs) oraz E-krawedzi postaci (ve, v′)
 */
wierzcholkiVsVe(V, E, Vs, Ve) :-
    findall(
        (V1, V2),
        (
            member(V1, V),
            member(V2, V),
            V1 \= V2,
            \+ member(edgeE(_, V1), E),
            \+ member(edgeE(V2, _), E)
        ),
        [(Vs, Ve)|T]
    ),
    %jesli findall zwroci tylko jeden element, to T powinno byc pusta lista
    \+ nth0(0, T, _).

/**
 * Funkcja sprawdzajaca czy dana lista wierzcholkow tworzy sciezke polaczona E krawedziami
 */
sciezkaPolaczonaEKrawedziami([], _) :-
    write("Pusta lista"),
    true.
sciezkaPolaczonaEKrawedziami([H|[]], _) :-
    write("Jednoelementowa lista"),
    true.
sciezkaPolaczonaEKrawedziami([H1,H2|T], E) :-
    write("Przynajmniej dwuelementowa lista"),
    member(edgeE(H1, H2), E),
    sciezkaPolaczonaEKrawedziami([H2|T], E).

/**
 * Funkcja sprawdzajaca drugi warunek dobrze ulozonego grafu, czyli:
 *      Istnieje sciezka v1,...,vn, taka ze V = {v1,...,vn}, v1 = vs, vn = ve oraz
 *      (vi, vi+1) ∈ E dla i = 1, . . . , n − 1.
 */
istniejeESciezkaVsVe(V, E, Vs, Ve) :-
    length(V, LiczbaWierzcholkow),
    length(E, LiczbaKrawedzi),
    findall(
        (PotencjalneSciezki),
        (
            %kazdy element V nalezy do sciezki
            forall(member(Wierz, V), member(Wierz, PotencjalneSciezki)),
            
            %dlugosc sciezki jest ograniczona przez (liczba wierzcholkow * liczba krawedzi)
            length(PotencjalneSciezki, DlugoscSciezki),
            DlugoscSciezki =< LiczbaWierzcholkow * LiczbaKrawedzi,

            %pierwszy element to Vs, ostatni to Ve
            nth0(0, PotencjalneSciezki, Vs),
            nth1(DlugoscSciezki, PotencjalneSciezki, Ve),
            
            %wierzcholki na sciezce sa polaczane E krawedzia
            sciezkaPolaczonaEKrawedziami(PotencjalneSciezki, E)
        ),
        ListaSzukanychSciezek
    ),
    nth0(0, ListaSzukanychSciezek, _).

/**
 * Funkcja sprawdzajaca trzeci warunek dobrze ulozonego grafu, czyli:
 *      Dla kazedego wierzcholka v ∈ V istnieja co najwyzej trzy F-krawedzie zawierajace v
 */
limitFKrawedzi(V, F) :-
    forall(
        member(Wierz, V),    
        (
            findall(
                (_),
                (member(edgeF(Wierz, _), F)),
                ListaKrawedzi
            ),
            length(ListaKrawedzi, DlugoscListy),
            DlugoscListy =< 3
        )
    ).

/**
 * Funkcja sprawdzajaca czy podany EFGraf jest dobrze ulozony
 */
jestDobrzeUlozonyNieMusiBycEFGrafem(EFGraf) :-
    wierzcholki(EFGraf, V),
    krawedzieE(EFGraf, DupE),
    krawedzieF(EFGraf, DupF),
    sort(DupE, E),
    sort(DupF, F),
    wierzcholkiVsVe(V, E, Vs, Ve),
    istniejeESciezkaVsVe(V, E, Vs, Ve),
    limitFKrawedzi(V, F).

/**
 * Funkcja sprawdzajaca czy podany EFGraf jest dobrze ulozony
 * (ale nie musi byc EFGrafem) i czy jest dobrze permutujacy
 */
jestDobrzePermutujacy(EFGraf) :-
    jestDobrzeUlozonyNieMusiBycEFGrafem(EFGraf).