
:- module(intelligenza_artificiale, [
    trova_miglior_mossa/4,
    valuta_gioco/2,
    trova_board_giocabili/4
]).

:- use_module(library(lists)).
:- use_module(regole_gioco).

peso_posizione(0, 0.2).
peso_posizione(1, 0.17).
peso_posizione(2, 0.2).
peso_posizione(3, 0.17).
peso_posizione(4, 0.22).
peso_posizione(5, 0.17).
peso_posizione(6, 0.2).
peso_posizione(7, 0.17).
peso_posizione(8, 0.2).

moltiplicatore_board(0, 2.0).
moltiplicatore_board(1, 1.2).
moltiplicatore_board(2, 2.0).
moltiplicatore_board(3, 1.2).
moltiplicatore_board(4, 2.5).
moltiplicatore_board(5, 1.2).
moltiplicatore_board(6, 2.0).
moltiplicatore_board(7, 1.2).
moltiplicatore_board(8, 2.0).

valuta_board(Tavola, Punteggio) :-
    valuta_posizioni(Tavola, 0, 0, PunteggioPos),
    valuta_coppie(Tavola, BonusCoppie),
    controlla_vittoria(Tavola, Vincitore),
    BonusVittoria is Vincitore * -60,
    Punteggio is PunteggioPos + BonusCoppie + BonusVittoria.

valuta_posizioni([], _, Acc, Acc).
valuta_posizioni([V|Resto], Indice, Acc, Punteggio) :-
    peso_posizione(Indice, Peso),
    Contributo is -V * Peso,
    NuovoAcc is Acc + Contributo,
    Indice1 is Indice + 1,
    valuta_posizioni(Resto, Indice1, NuovoAcc, Punteggio).

valuta_coppie(Tavola, Bonus) :-
    findall(B, (
        (   controlla_coppia(Tavola, [0,1,2], B)
        ;   controlla_coppia(Tavola, [3,4,5], B)
        ;   controlla_coppia(Tavola, [6,7,8], B)
        ;   controlla_coppia(Tavola, [0,3,6], B)
        ;   controlla_coppia(Tavola, [1,4,7], B)
        ;   controlla_coppia(Tavola, [2,5,8], B)
        ;   controlla_coppia(Tavola, [0,4,8], B)
        ;   controlla_coppia(Tavola, [2,4,6], B)
        )
    ), BonusLista),
    sumlist(BonusLista, Bonus).

controlla_coppia(Tavola, [I1, I2, I3], Bonus) :-
    elemento_n(I1, Tavola, V1),
    elemento_n(I2, Tavola, V2),
    elemento_n(I3, Tavola, V3),
    (   V1 =:= 1, V2 =:= 1, V3 =:= 0 -> Bonus = -30
    ;   V1 =:= -1, V2 =:= -1, V3 =:= 0 -> Bonus = 30
    ;   Bonus = 0
    ).

valuta_gioco(stato_gioco(Tavole, TavolaPrincipale, _, TavolaCorrente, _), Valutazione) :-
    valuta_gioco_aux(Tavole, 0, 0, TavolaCorrente, TavolaPrincipale, PunteggiBoard, TavolaPrincipaleTemp),
    sumlist(PunteggiBoard, TotaleBoard),
    valuta_board(TavolaPrincipaleTemp, PunteggioMain),
    controlla_vittoria(TavolaPrincipaleTemp, VincitoreMain),
    BonusVittoriaMain is VincitoreMain * -5000,
    Valutazione is TotaleBoard + PunteggioMain * 400 + BonusVittoriaMain.

valuta_gioco_aux([], _, _, _, _, [], []).
valuta_gioco_aux([Tavola|Resto], Indice, Acc, TavolaCorrente, TavolaPrincipale, [Punteggio|Punteggi], [Vincitore|Vincitori]) :-
    valuta_board(Tavola, PunteggioBase),
    moltiplicatore_board(Indice, Molt),
    Punteggio1 is PunteggioBase * 2.0 * Molt,
    (   Indice =:= TavolaCorrente
    ->  Punteggio2 is PunteggioBase * Molt * 1.5, Punteggio is Punteggio1 + Punteggio2
    ;   Punteggio = Punteggio1
    ),
    controlla_vittoria(Tavola, Vincitore),
    Indice1 is Indice + 1,
    valuta_gioco_aux(Resto, Indice1, Acc, TavolaCorrente, TavolaPrincipale, Punteggi, Vincitori).

minimax(StatoGioco, Profondita, _Alpha, _Beta, _, MigliorPunteggio, -1) :-
    (   Profondita =< 0
    ;   gioco_terminato(StatoGioco, _)
    ), !,
    valuta_gioco(StatoGioco, MigliorPunteggio).

minimax(stato_gioco(Tavole, TavolaPrincipale, Turno, TavolaCorrente, InCorso), 
        Profondita, Alpha, Beta, GiocatoreMassimizzante, MigliorPunteggio, MigliorMossa) :-
    Profondita > 0,
    \+ gioco_terminato(stato_gioco(Tavole, TavolaPrincipale, Turno, TavolaCorrente, InCorso), _),
    trova_board_giocabili(Tavole, TavolaPrincipale, TavolaCorrente, TavoleGiocabili),
    
    (   GiocatoreMassimizzante
    ->  minimax_max(stato_gioco(Tavole, TavolaPrincipale, Turno, TavolaCorrente, InCorso),
                    TavoleGiocabili, Profondita, Alpha, Beta, -999999, -1, MigliorPunteggio, MigliorMossa)
    ;   minimax_min(stato_gioco(Tavole, TavolaPrincipale, Turno, TavolaCorrente, InCorso),
                    TavoleGiocabili, Profondita, Alpha, Beta, 999999, -1, MigliorPunteggio, MigliorMossa)
    ).

trova_board_giocabili(Tavole, TavolaPrincipale, TavolaCorrente, TavoleGiocabili) :-
    (   TavolaCorrente >= 0,
        elemento_n(TavolaCorrente, TavolaPrincipale, 0),
        elemento_n(TavolaCorrente, Tavole, Tavola),
        membro(0, Tavola)
    ->  TavoleGiocabili = [TavolaCorrente]
    ;   findall(I, (tra(0, 8, I),
                    elemento_n(I, TavolaPrincipale, 0),
                    elemento_n(I, Tavole, B),
                    membro(0, B)), TavoleGiocabili)
    ).

minimax_max(_, [], _, _, _, MigliorPunteggio, MigliorMossa, MigliorPunteggio, MigliorMossa) :- !.
minimax_max(StatoGioco, [IndiceTavola|RestoTavole], Profondita, Alpha, Beta, 
            MigliorCorrente, MossaCorrente, MigliorPunteggio, MigliorMossa) :-
    StatoGioco = stato_gioco(Tavole, _TavolaPrincipale, _Turno, _, _InCorso),
    elemento_n(IndiceTavola, Tavole, Tavola),
    findall(IndiceCella, (tra(0, 8, IndiceCella), elemento_n(IndiceCella, Tavola, 0)), Celle),
    minimax_max_celle(StatoGioco, IndiceTavola, Celle, Profondita, Alpha, Beta,
                      MigliorCorrente, MossaCorrente, NuovoMigliore, NuovaMossa, NuovoAlpha),
    (   NuovoAlpha >= Beta
    ->  MigliorPunteggio = NuovoMigliore, MigliorMossa = NuovaMossa
    ;   minimax_max(StatoGioco, RestoTavole, Profondita, NuovoAlpha, Beta,
                    NuovoMigliore, NuovaMossa, MigliorPunteggio, MigliorMossa)
    ).

minimax_max_celle(_, _, [], _, Alpha, _, Migliore, Mossa, Migliore, Mossa, Alpha) :- !.
minimax_max_celle(StatoGioco, IndiceTavola, [IndiceCella|RestoCelle], Profondita, Alpha, Beta,
                  MigliorCorrente, MossaCorrente, MigliorPunteggio, MigliorMossa, AlphaFinale) :-
    StatoGioco = stato_gioco(_Tavole, _TavolaPrincipale, _Turno, _, _InCorso),
    esegui_mossa(StatoGioco, IndiceTavola, IndiceCella, NuovoStatoGioco),
    Profondita1 is Profondita - 1,
    minimax(NuovoStatoGioco, Profondita1, Alpha, Beta, false, Punteggio, _),
    (   Punteggio > MigliorCorrente
    ->  NuovoMigliore = Punteggio, NuovaMossa = IndiceTavola, NuovoAlpha is max(Alpha, Punteggio)
    ;   NuovoMigliore = MigliorCorrente, NuovaMossa = MossaCorrente, NuovoAlpha = Alpha
    ),
    (   NuovoAlpha >= Beta
    ->  MigliorPunteggio = NuovoMigliore, MigliorMossa = NuovaMossa, AlphaFinale = NuovoAlpha
    ;   minimax_max_celle(StatoGioco, IndiceTavola, RestoCelle, Profondita, NuovoAlpha, Beta,
                          NuovoMigliore, NuovaMossa, MigliorPunteggio, MigliorMossa, AlphaFinale)
    ).

minimax_min(_, [], _, _, _, MigliorPunteggio, MigliorMossa, MigliorPunteggio, MigliorMossa) :- !.
minimax_min(StatoGioco, [IndiceTavola|RestoTavole], Profondita, Alpha, Beta,
            MigliorCorrente, MossaCorrente, MigliorPunteggio, MigliorMossa) :-
    StatoGioco = stato_gioco(Tavole, _TavolaPrincipale, _Turno, _, _InCorso),
    elemento_n(IndiceTavola, Tavole, Tavola),
    findall(IndiceCella, (tra(0, 8, IndiceCella), elemento_n(IndiceCella, Tavola, 0)), Celle),
    minimax_min_celle(StatoGioco, IndiceTavola, Celle, Profondita, Alpha, Beta,
                      MigliorCorrente, MossaCorrente, NuovoMigliore, NuovaMossa, NuovoBeta),
    (   NuovoBeta =< Alpha
    ->  MigliorPunteggio = NuovoMigliore, MigliorMossa = NuovaMossa
    ;   minimax_min(StatoGioco, RestoTavole, Profondita, Alpha, NuovoBeta,
                    NuovoMigliore, NuovaMossa, MigliorPunteggio, MigliorMossa)
    ).

minimax_min_celle(_, _, [], _, _, Beta, Migliore, Mossa, Migliore, Mossa, Beta) :- !.
minimax_min_celle(StatoGioco, IndiceTavola, [IndiceCella|RestoCelle], Profondita, Alpha, Beta,
                  MigliorCorrente, MossaCorrente, MigliorPunteggio, MigliorMossa, BetaFinale) :-
    StatoGioco = stato_gioco(_Tavole, _TavolaPrincipale, _Turno, _, _InCorso),
    esegui_mossa(StatoGioco, IndiceTavola, IndiceCella, NuovoStatoGioco),
    Profondita1 is Profondita - 1,
    minimax(NuovoStatoGioco, Profondita1, Alpha, Beta, true, Punteggio, _),
    (   Punteggio < MigliorCorrente
    ->  NuovoMigliore = Punteggio, NuovaMossa = IndiceTavola, NuovoBeta is min(Beta, Punteggio)
    ;   NuovoMigliore = MigliorCorrente, NuovaMossa = MossaCorrente, NuovoBeta = Beta
    ),
    (   NuovoBeta =< Alpha
    ->  MigliorPunteggio = NuovoMigliore, MigliorMossa = NuovaMossa, BetaFinale = NuovoBeta
    ;   minimax_min_celle(StatoGioco, IndiceTavola, RestoCelle, Profondita, Alpha, NuovoBeta,
                          NuovoMigliore, NuovaMossa, MigliorPunteggio, MigliorMossa, BetaFinale)
    ).

trova_miglior_mossa(StatoGioco, Profondita, MigliorTavolaIdx, MigliorCellaIdx) :-
    StatoGioco = stato_gioco(Tavole, TavolaPrincipale, Turno, TavolaCorrente, _),
    trova_board_giocabili(Tavole, TavolaPrincipale, TavolaCorrente, TavoleGiocabili),
    (   Turno =:= 1
    ->  trova_miglior_mossa_min(StatoGioco, TavoleGiocabili, Profondita, 999999, -1, -1, 
                          MigliorTavolaIdx, MigliorCellaIdx)
    ;   trova_miglior_mossa_max(StatoGioco, TavoleGiocabili, Profondita, -999999, -1, -1, 
                          MigliorTavolaIdx, MigliorCellaIdx)
    ).

trova_miglior_mossa_max(_, [], _, _, MigliorTavola, MigliorCella, MigliorTavola, MigliorCella) :- !.
trova_miglior_mossa_max(StatoGioco, [IndiceTavola|Resto], Profondita, MigliorCorrente, 
                   TavolaCorrente, CellaCorrente, MigliorTavola, MigliorCella) :-
    StatoGioco = stato_gioco(Tavole, _, _, _, _),
    elemento_n(IndiceTavola, Tavole, Tavola),
    findall(IndiceCella, (tra(0, 8, IndiceCella), elemento_n(IndiceCella, Tavola, 0)), Celle),
    valuta_celle_max(StatoGioco, IndiceTavola, Celle, Profondita, MigliorCorrente, 
                   TavolaCorrente, CellaCorrente, NuovoMigliore, NuovaTavola, NuovaCella),
    trova_miglior_mossa_max(StatoGioco, Resto, Profondita, NuovoMigliore, NuovaTavola, NuovaCella, 
                      MigliorTavola, MigliorCella).

valuta_celle_max(_, _, [], _, Migliore, Tavola, Cella, Migliore, Tavola, Cella) :- !.
valuta_celle_max(StatoGioco, IndiceTavola, [IndiceCella|Resto], Profondita, MigliorCorrente,
               TavolaCorrente, CellaCorrente, MigliorPunteggio, MigliorTavola, MigliorCella) :-
    esegui_mossa(StatoGioco, IndiceTavola, IndiceCella, NuovoStatoGioco),
    Profondita1 is Profondita - 1,
    minimax(NuovoStatoGioco, Profondita1, -999999, 999999, false, Punteggio, _),
    (   Punteggio > MigliorCorrente
    ->  NuovoMigliore = Punteggio, NuovaTavola = IndiceTavola, NuovaCella = IndiceCella
    ;   NuovoMigliore = MigliorCorrente, NuovaTavola = TavolaCorrente, NuovaCella = CellaCorrente
    ),
    valuta_celle_max(StatoGioco, IndiceTavola, Resto, Profondita, NuovoMigliore, NuovaTavola, NuovaCella,
                  MigliorPunteggio, MigliorTavola, MigliorCella).

trova_miglior_mossa_min(_, [], _, _, MigliorTavola, MigliorCella, MigliorTavola, MigliorCella) :- !.
trova_miglior_mossa_min(StatoGioco, [IndiceTavola|Resto], Profondita, MigliorCorrente, 
                   TavolaCorrente, CellaCorrente, MigliorTavola, MigliorCella) :-
    StatoGioco = stato_gioco(Tavole, _, _, _, _),
    elemento_n(IndiceTavola, Tavole, Tavola),
    findall(IndiceCella, (tra(0, 8, IndiceCella), elemento_n(IndiceCella, Tavola, 0)), Celle),
    valuta_celle_min(StatoGioco, IndiceTavola, Celle, Profondita, MigliorCorrente, 
                   TavolaCorrente, CellaCorrente, NuovoMigliore, NuovaTavola, NuovaCella),
    trova_miglior_mossa_min(StatoGioco, Resto, Profondita, NuovoMigliore, NuovaTavola, NuovaCella, 
                      MigliorTavola, MigliorCella).

valuta_celle_min(_, _, [], _, Migliore, Tavola, Cella, Migliore, Tavola, Cella) :- !.
valuta_celle_min(StatoGioco, IndiceTavola, [IndiceCella|Resto], Profondita, MigliorCorrente,
               TavolaCorrente, CellaCorrente, MigliorPunteggio, MigliorTavola, MigliorCella) :-
    esegui_mossa(StatoGioco, IndiceTavola, IndiceCella, NuovoStatoGioco),
    Profondita1 is Profondita - 1,
    minimax(NuovoStatoGioco, Profondita1, -999999, 999999, true, Punteggio, _),
    (   Punteggio < MigliorCorrente
    ->  NuovoMigliore = Punteggio, NuovaTavola = IndiceTavola, NuovaCella = IndiceCella
    ;   NuovoMigliore = MigliorCorrente, NuovaTavola = TavolaCorrente, NuovaCella = CellaCorrente
    ),
    valuta_celle_min(StatoGioco, IndiceTavola, Resto, Profondita, NuovoMigliore, NuovaTavola, NuovaCella,
                  MigliorPunteggio, MigliorTavola, MigliorCella).

