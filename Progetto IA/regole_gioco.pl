
:- module(regole_gioco, [
    inizializza_gioco/1,
    controlla_vittoria/2,
    mossa_valida/4,
    esegui_mossa/4,
    gioco_terminato/2,
    sostituisci_nth0/4,
    giocatore_x/1,
    giocatore_o/1,
    imposta_giocatore_umano/1,
    leggi_giocatore_umano/1,
    resetta_mosse/0,
    incrementa_mosse/0,
    lunghezza/2,
    mappa_lista/3,
    elemento_n/3,
    membro/2,
    tra/3,
    imposta_livello_difficolta/1,
    leggi_livello_difficolta/1,
    imposta_lato_temp/1,
    leggi_lato_temp/1,
    imposta_difficolta_temp/1,
    leggi_difficolta_temp/1,
    leggi_mosse/1,
    conta_celle_vuote/2
]).

:- use_module(library(lists)).

lunghezza(Lista, N) :- length(Lista, N). 
mappa_lista(Predicato, Lista1, Lista2) :- maplist(Predicato, Lista1, Lista2).
mappa_lista(Predicato, Lista) :- maplist(Predicato, Lista).
elemento_n(N, Lista, Elemento) :- nth0(N, Lista, Elemento).
membro(Elemento, Lista) :- member(Elemento, Lista).
tra(Min, Max, Valore) :- between(Min, Max, Valore).

:- dynamic conta_mosse/1.
:- dynamic giocatore_umano/1.
:- dynamic livello_difficolta/1.
:- dynamic lato_temp/1.
:- dynamic difficolta_temp/1.

imposta_giocatore_umano(Lato) :-
    retractall(giocatore_umano(_)),
    assert(giocatore_umano(Lato)).

leggi_giocatore_umano(Lato) :-
    giocatore_umano(Lato).

imposta_livello_difficolta(Livello) :-
    retractall(livello_difficolta(_)),
    assert(livello_difficolta(Livello)).

leggi_livello_difficolta(Livello) :-
    (   livello_difficolta(Livello) -> true ; Livello = difficile ).

imposta_lato_temp(Lato) :- 
    retractall(lato_temp(_)),
    assert(lato_temp(Lato)).

leggi_lato_temp(Lato) :-
    (   lato_temp(Lato) -> true ; Lato = x ).

imposta_difficolta_temp(Livello) :-
    retractall(difficolta_temp(_)),
    assert(difficolta_temp(Livello)).

leggi_difficolta_temp(Livello) :-
    (   difficolta_temp(Livello) -> true ; Livello = difficile ).

resetta_mosse :-
    retractall(conta_mosse(_)),
    assert(conta_mosse(0)).

incrementa_mosse :-
    conta_mosse(M),
    M1 is M + 1,
    retractall(conta_mosse(_)),
    assert(conta_mosse(M1)).

leggi_mosse(M) :-
    (   conta_mosse(M) -> true ; M = 0 ).

giocatore_x(1).
giocatore_o(-1).

inizializza_gioco(stato_gioco(Tavole, TavolaPrincipale, Turno, TavolaCorrente, true)) :-
    lunghezza(TavolaVuota, 9),
    mappa_lista(=(0), TavolaVuota),
    lunghezza(Tavole, 9),
    mappa_lista(=(TavolaVuota), Tavole),
    mappa_lista(=(0), TavolaPrincipale),
    Turno = 1,
    TavolaCorrente = -1.

controlla_vittoria(Tavola, Vincitore) :-
    (   controlla_linea(Tavola, [0,1,2], Vincitore)
    ;   controlla_linea(Tavola, [3,4,5], Vincitore)
    ;   controlla_linea(Tavola, [6,7,8], Vincitore)
    ;   controlla_linea(Tavola, [0,3,6], Vincitore)
    ;   controlla_linea(Tavola, [1,4,7], Vincitore)
    ;   controlla_linea(Tavola, [2,5,8], Vincitore)
    ;   controlla_linea(Tavola, [0,4,8], Vincitore)
    ;   controlla_linea(Tavola, [2,4,6], Vincitore)
    ), !.
controlla_vittoria(_, 0).

controlla_linea(Tavola, [I1, I2, I3], Vincitore) :-
    elemento_n(I1, Tavola, V1), V1 \= 0,
    elemento_n(I2, Tavola, V2), V1 = V2,
    elemento_n(I3, Tavola, V3), V1 = V3,
    Vincitore = V1.

mossa_valida(Tavole, TavolaPrincipale, IndiceTavola, IndiceCella) :-
    tra(0, 8, IndiceTavola),
    tra(0, 8, IndiceCella),
    elemento_n(IndiceTavola, TavolaPrincipale, 0),
    elemento_n(IndiceTavola, Tavole, Tavola),
    elemento_n(IndiceCella, Tavola, 0).

esegui_mossa(stato_gioco(Tavole, TavolaPrincipale, Turno, _, InCorso),
             IndiceTavola, IndiceCella,
             stato_gioco(NuoveTavole, NuovaTavolaPrincipale, NuovoTurno, ProssimaTavola, InCorso)) :-
    elemento_n(IndiceTavola, Tavole, Tavola),
    sostituisci_nth0(IndiceCella, Tavola, Turno, NuovaTavola),
    sostituisci_nth0(IndiceTavola, Tavole, NuovaTavola, NuoveTavole),
    controlla_vittoria(NuovaTavola, Vincitore),
    sostituisci_nth0(IndiceTavola, TavolaPrincipale, Vincitore, NuovaTavolaPrincipale),
    NuovoTurno is -Turno,
    (   elemento_n(IndiceCella, NuovaTavolaPrincipale, 0),
        elemento_n(IndiceCella, NuoveTavole, TavolaTarget),
        membro(0, TavolaTarget)
    ->  ProssimaTavola = IndiceCella
    ;   ProssimaTavola = -1
    ).

sostituisci_nth0(0, [_|T], X, [X|T]) :- !.
sostituisci_nth0(N, [H|T], X, [H|R]) :-
    N > 0,
    N1 is N - 1,
    sostituisci_nth0(N1, T, X, R).

gioco_terminato(stato_gioco(_, TavolaPrincipale, _, _, _), Vincitore) :-
    controlla_vittoria(TavolaPrincipale, Vincitore),
    Vincitore \= 0.
gioco_terminato(stato_gioco(Tavole, TavolaPrincipale, _, _, _), 0) :-
    \+ (tra(0, 8, I),
        elemento_n(I, TavolaPrincipale, 0),
        elemento_n(I, Tavole, Tavola),
        membro(0, Tavola)).

conta_celle_vuote(stato_gioco(Tavole, TavolaPrincipale, _, _, _), NumCelleVuote) :-
    findall(Cella,
            (tra(0, 8, I),
             elemento_n(I, TavolaPrincipale, 0),
             elemento_n(I, Tavole, Tavola),
             membro(Cella, Tavola),
             Cella =:= 0),
            CelleVuote),
    lunghezza(CelleVuote, NumCelleVuote).

