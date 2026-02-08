
:- module(interfaccia_grafica, [
    avvia_interfaccia/0
]).

:- use_module(library(pce)).
:- use_module(regole_gioco).
:- use_module(intelligenza_artificiale).

larghezza_finestra(800).
altezza_finestra(850).

colore_bianco(white).
colore_nero(black).
colore_blu(navy_blue).
colore_rosso(red).

:- pce_begin_class(finestra_ultimate_tictactoe, frame, "Ultimate Tic-Tac-Toe Game").

variable(canvas, object, both, "Canvas di disegno").
variable(stato_gioco, prolog, both, "Stato corrente del gioco").
variable(ia_attiva, bool := @on, both, "IA abilitata").
variable(timer_ia, timer, both, "Timer per l'IA").
variable(ia_in_esecuzione, bool := @off, both, "Flag per evitare sovrapposizione IA").

initialise(W) :->
    send_super(W, initialise, 'Ultimate Tic-Tac-Toe'),
    larghezza_finestra(Larghezza),
    altezza_finestra(Altezza),
    send(W, append, new(Canvas, picture)),
    send(Canvas, size, size(Larghezza, Altezza)),
    send(W, slot, canvas, Canvas),
    send(W, slot, timer_ia, new(_, timer(0.1, message(W, turno_ia)))),
    inizializza_gioco(StatoIniziale),
    send(W, slot, stato_gioco, StatoIniziale),
    send(Canvas, recogniser, 
         click_gesture(left, '', single, 
                      message(W, gestisci_click, @event))),
    send(W, disegna_gioco),
    send(W, open),
    send(W, richiedi_selezione_lato).

richiedi_selezione_lato(W) :->
    new(D, dialog('Configurazione Partita')),
    
    % Selezione lato con radio buttons
    send(D, append, new(LatoGroup, menu(lato, choice))),
    send(LatoGroup, label, 'Vuoi giocare come:'),
    send(LatoGroup, append, menu_item(x, label := 'X (Inizi tu)')),
    send(LatoGroup, append, menu_item(o, label := 'O (Inizia IA)')),
    send(LatoGroup, selection, x),  % Default: X
    send(LatoGroup, layout, vertical),
    
    send(D, append, new(_, label(text, ''))),  % Spazio vuoto
    
    % Selezione difficoltà con radio buttons
    send(D, append, new(DiffGroup, menu(difficolta, choice))),
    send(DiffGroup, label, 'Livello di difficoltà:'),
    send(DiffGroup, append, menu_item(facile, label := 'Facile (IA pensa 1 mossa)')),
    send(DiffGroup, append, menu_item(medio, label := 'Medio (IA prevede 3 mosse)')),
    send(DiffGroup, append, menu_item(difficile, label := 'Difficile (IA esperta)')),
    send(DiffGroup, selection, difficile),  % Default: difficile
    send(DiffGroup, layout, vertical),
    
    send(D, append, new(_, label(text, ''))),  % Spazio vuoto
    send(D, append, button('Inizia Partita', message(D, return, ok))),
    
    send(D, transient_for, W),
    send(D, modal, transient),
    get(D, confirm_centered, _),
    
    % Leggi i valori selezionati dai menu
    get(LatoGroup, selection, LatoScelto),
    get(DiffGroup, selection, DifficoltaScelta),
    
    send(D, destroy),
    
    % Imposta difficoltà
    imposta_livello_difficolta(DifficoltaScelta),
    
    % Avvia partita con il lato scelto
    (   LatoScelto == x
    ->  send(W, nuova_partita, @on, 1)
    ;   LatoScelto == o
    ->  send(W, nuova_partita, @on, -1)
    ;   send(W, nuova_partita, @on, 1)
    ).

nuova_partita(W, IAAttiva:bool, LatoUmano:int) :->
    imposta_giocatore_umano(LatoUmano),
    resetta_mosse,
    inizializza_gioco(NuovoStato),
    send(W, slot, stato_gioco, NuovoStato),
    send(W, slot, ia_attiva, IAAttiva),
    send(W, disegna_gioco),
    (   IAAttiva = @on, LatoUmano = -1
    ->  get(W, slot, timer_ia, Timer),
        send(Timer, start(once))
    ;   true
    ).

gestisci_click(W, Evento:event) :->
    get(W, slot, stato_gioco, StatoGioco),
    StatoGioco = stato_gioco(Tavole, TavolaPrincipale, Turno, IndiceTavolaCorrente, InCorso),
    leggi_giocatore_umano(LatoUmano),
    get(W, slot, ia_attiva, IAAttiva),
    
    (   InCorso = true, 
        (IAAttiva = @off ; Turno = LatoUmano)
    ->  get(Evento, position, point(X, Y)),
        (   trova_cella_cliccata(X, Y, IndiceTavola, IndiceCella),
            (   IndiceTavolaCorrente >= 0
            ->  (   IndiceTavola =:= IndiceTavolaCorrente,
                    mossa_valida(Tavole, TavolaPrincipale, IndiceTavola, IndiceCella)
                ->  esegui_mossa(StatoGioco, IndiceTavola, IndiceCella, NuovoStato),
                    send(W, slot, stato_gioco, NuovoStato),
                    send(W, disegna_gioco),
                    send(W, controlla_fine_gioco),
                    (   IAAttiva = @on, \+ gioco_terminato(NuovoStato, _)
                    ->  get(W, slot, timer_ia, Timer),
                        send(Timer, start(once))
                    ;   true
                    )
                ;   true
                )
            ;   mossa_valida(Tavole, TavolaPrincipale, IndiceTavola, IndiceCella)
            ->  esegui_mossa(StatoGioco, IndiceTavola, IndiceCella, NuovoStato),
                send(W, slot, stato_gioco, NuovoStato),
                send(W, disegna_gioco),
                send(W, controlla_fine_gioco),
                (   IAAttiva = @on, \+ gioco_terminato(NuovoStato, _)
                ->  get(W, slot, timer_ia, Timer),
                    send(Timer, start(once))
                ;   true
                )
            ;   true
            )
        ;   true
        )
    ;   true
    ).

turno_ia(W) :->
    (   object(W)
    ->  get(W, slot, ia_in_esecuzione, IAInEsecuzione),
        (   IAInEsecuzione = @on
        ->  true
        ;   send(W, slot, ia_in_esecuzione, @on),
            get(W, slot, ia_attiva, IAAttiva),
            get(W, slot, stato_gioco, StatoGioco),
            StatoGioco = stato_gioco(_, _, Turno, _, InCorso),
            leggi_giocatore_umano(LatoUmano),
            
            (   IAAttiva = @on, InCorso = true, Turno \= LatoUmano
    ->  get(W, slot, stato_gioco, StatoCorrente),
        StatoCorrente = stato_gioco(Tavole, TavolaPrincipale, _, TavolaCorrente, _),
        
        % Conta celle vuote per ottimizzazione endgame
        conta_celle_vuote(StatoCorrente, CelleVuote),
        
        leggi_mosse(Mosse),
        
        leggi_livello_difficolta(Livello),
        (   Livello = facile
        ->  Profondita = 1,
            format('IA gioca a livello FACILE (profondità 1)~n')
        ;   Livello = medio
        ->  Profondita = 3,
            format('IA gioca a livello MEDIO (profondità 3)~n')
        ;   % Modalità difficile con ottimizzazione
            trova_board_giocabili(Tavole, TavolaPrincipale, TavolaCorrente, TavoleGiocabili),
            lunghezza(TavoleGiocabili, NumTavole),
            (   CelleVuote < 15
            ->  Profondita = 3,
                format('IA DIFFICILE: profondità 3 (endgame)~n')
            ;   NumTavole >= 7
            ->  Profondita = 3,
                format('IA DIFFICILE: profondità 3 (troppe board)~n')
            ;   Profondita = 5,
                format('IA DIFFICILE: profondità 5~n')
            )
        ),
        
        get(W, slot, canvas, Canvas),
        send(Canvas, cursor, watch),
        
        (   Mosse =:= 0, TavolaCorrente =:= -1
        ->  MigliorTavola = 4, MigliorCella = 4,
            writeln('IA esegue mossa di apertura: Centro-Centro')
        ;   Mosse =:= 1, TavolaCorrente =:= 4
        ->  (   mossa_valida(Tavole, TavolaPrincipale, 4, 0)
            ->  MigliorTavola = 4, MigliorCella = 0
            ;   mossa_valida(Tavole, TavolaPrincipale, 4, 2)
            ->  MigliorTavola = 4, MigliorCella = 2
            ;   mossa_valida(Tavole, TavolaPrincipale, 4, 6)
            ->  MigliorTavola = 4, MigliorCella = 6
            ;   mossa_valida(Tavole, TavolaPrincipale, 4, 8)
            ->  MigliorTavola = 4, MigliorCella = 8
            ;   trova_miglior_mossa(StatoCorrente, Profondita, MigliorTavola, MigliorCella)
            ),
            writeln('IA esegue mossa di apertura: Angolo strategico')
        ;   trova_miglior_mossa(StatoCorrente, Profondita, MigliorTavola, MigliorCella)
        ),
        
        send(Canvas, cursor, arrow),
        
        (   MigliorTavola >= 0, MigliorCella >= 0
        ->  esegui_mossa(StatoCorrente, MigliorTavola, MigliorCella, NuovoStato),
            send(W, slot, stato_gioco, NuovoStato),
            incrementa_mosse,
            send(W, disegna_gioco),
            send(W, controlla_fine_gioco),
            true
        ;   true
        ),
        send(W, slot, ia_in_esecuzione, @off)
            )
        ;   true
        )
    ;   true
    ).

controlla_fine_gioco(W) :->
    get(W, slot, stato_gioco, StatoGioco),
    (   gioco_terminato(StatoGioco, Vincitore)
    ->  get(W, slot, timer_ia, Timer),
        send(Timer, stop),
        send(W, slot, ia_in_esecuzione, @off),
        send(W, slot, ia_attiva, @off),  % Disabilita completamente l'IA
        (   Vincitore =:= 1
        ->  send(W, report, inform, 'Vince Giocatore X!')
        ;   Vincitore =:= -1
        ->  send(W, report, inform, 'Vince Giocatore O!')
        ;   send(W, report, inform, 'Pareggio!')
        ),
        StatoGioco = stato_gioco(B, MB, T, CB, _),
        send(W, slot, stato_gioco, stato_gioco(B, MB, T, CB, false))
    ;   true
    ).

disegna_gioco(W) :->
    get(W, slot, canvas, Canvas),
    send(Canvas, clear),
    larghezza_finestra(Larghezza),
    send(Canvas, display, new(BG, box(Larghezza, Larghezza))),
    send(BG, fill_pattern, colour(white)),
    send(W, disegna_griglia),
    send(W, disegna_simboli),
    send(W, evidenzia_board_corrente).

disegna_griglia(W) :->
    get(W, slot, canvas, Canvas),
    larghezza_finestra(Larghezza),
    colore_nero(Nero),
    L1 is Larghezza // 3,
    L2 is 2 * Larghezza // 3,
    send(Canvas, display, new(Linea1, line(L1, 0, L1, Larghezza))),
    send(Linea1, colour, colour(Nero)),
    send(Canvas, display, new(Linea2, line(L2, 0, L2, Larghezza))),
    send(Linea2, colour, colour(Nero)),
    send(Canvas, display, new(Linea3, line(0, L1, Larghezza, L1))),
    send(Linea3, colour, colour(Nero)),
    send(Canvas, display, new(Linea4, line(0, L2, Larghezza, L2))),
    send(Linea4, colour, colour(Nero)),
    DimensioneQuadrato is Larghezza // 4,
    Offset is (Larghezza // 3 - DimensioneQuadrato) // 2,
    disegna_griglie_interne(Canvas, 0, Larghezza, DimensioneQuadrato, Offset, Nero).

disegna_griglie_interne(_, 9, _, _, _, _) :- !.
disegna_griglie_interne(Canvas, Indice, Larghezza, Dimensione, Offset, Colore) :-
    Indice < 9,
    I is Indice mod 3,
    J is Indice // 3,
    BaseX is I * Larghezza // 3 + Offset,
    BaseY is J * Larghezza // 3 + Offset,
    X1 is BaseX + Dimensione // 3,
    X2 is BaseX + 2 * Dimensione // 3,
    send(Canvas, display, new(L1, line(X1, BaseY, X1, BaseY + Dimensione))),
    send(L1, colour, colour(Colore)),
    send(Canvas, display, new(L2, line(X2, BaseY, X2, BaseY + Dimensione))),
    send(L2, colour, colour(Colore)),
    Y1 is BaseY + Dimensione // 3,
    Y2 is BaseY + 2 * Dimensione // 3,
    send(Canvas, display, new(L3, line(BaseX, Y1, BaseX + Dimensione, Y1))),
    send(L3, colour, colour(Colore)),
    send(Canvas, display, new(L4, line(BaseX, Y2, BaseX + Dimensione, Y2))),
    send(L4, colour, colour(Colore)),
    Indice1 is Indice + 1,
    disegna_griglie_interne(Canvas, Indice1, Larghezza, Dimensione, Offset, Colore).

disegna_simboli(W) :->
    get(W, slot, stato_gioco, stato_gioco(Tavole, TavolaPrincipale, _, _, _)),
    get(W, slot, canvas, Canvas),
    larghezza_finestra(Larghezza),
    disegna_simboli_piccoli(Canvas, Tavole, 0, Larghezza),
    disegna_simboli_grandi(Canvas, TavolaPrincipale, 0, Larghezza).

disegna_simboli_piccoli(_, _, 9, _) :- !.
disegna_simboli_piccoli(Canvas, Tavole, IndiceTavola, Larghezza) :-
    IndiceTavola < 9,
    elemento_n(IndiceTavola, Tavole, Tavola),
    DimensioneQuadrato is Larghezza // 4,
    Offset is (Larghezza // 3 - DimensioneQuadrato) // 2,
    DimensioneSimbolo is Larghezza // 36,
    BI is IndiceTavola mod 3,
    BJ is IndiceTavola // 3,
    disegna_simboli_board(Canvas, Tavola, 0, BI, BJ, Larghezza, DimensioneQuadrato, Offset, DimensioneSimbolo),
    IndiceTavola1 is IndiceTavola + 1,
    disegna_simboli_piccoli(Canvas, Tavole, IndiceTavola1, Larghezza).

disegna_simboli_board(_, _, 9, _, _, _, _, _, _) :- !.
disegna_simboli_board(Canvas, Tavola, IndiceCella, BI, BJ, Larghezza, Dimensione, Offset, DimensioneSimbolo) :-
    IndiceCella < 9,
    elemento_n(IndiceCella, Tavola, Valore),
    (   Valore \= 0
    ->  CI is IndiceCella mod 3,
        CJ is IndiceCella // 3,
        BaseX is BI * Larghezza // 3 + Offset + Dimensione // 6,
        BaseY is BJ * Larghezza // 3 + Offset + Dimensione // 6,
        X is BaseX + CI * Dimensione // 3,
        Y is BaseY + CJ * Dimensione // 3,
        disegna_simbolo(Canvas, Valore, X, Y, DimensioneSimbolo, piccolo)
    ;   true
    ),
    IndiceCella1 is IndiceCella + 1,
    disegna_simboli_board(Canvas, Tavola, IndiceCella1, BI, BJ, Larghezza, Dimensione, Offset, DimensioneSimbolo).

disegna_simboli_grandi(_, _, 9, _) :- !.
disegna_simboli_grandi(Canvas, TavolaPrincipale, Indice, Larghezza) :-
    Indice < 9,
    elemento_n(Indice, TavolaPrincipale, Valore),
    (   Valore \= 0
    ->  I is Indice mod 3,
        J is Indice // 3,
        X is Larghezza // 6 + I * Larghezza // 3,
        Y is Larghezza // 6 + J * Larghezza // 3,
        DimensioneSimbolo is Larghezza // 12,
        disegna_simbolo(Canvas, Valore, X, Y, DimensioneSimbolo, grande)
    ;   true
    ),
    Indice1 is Indice + 1,
    disegna_simboli_grandi(Canvas, TavolaPrincipale, Indice1, Larghezza).

disegna_simbolo(Canvas, 1, X, Y, Dimensione, _Tipo) :-
    colore_rosso(Rosso),
    X1 is X - Dimensione, Y1 is Y - Dimensione,
    X2 is X + Dimensione, Y2 is Y + Dimensione,
    send(Canvas, display, new(L1, line(X1, Y1, X2, Y2))),
    send(L1, colour, colour(Rosso)),
    send(L1, pen, 5),
    send(Canvas, display, new(L2, line(X1, Y2, X2, Y1))),
    send(L2, colour, colour(Rosso)),
    send(L2, pen, 5).

disegna_simbolo(Canvas, -1, X, Y, Dimensione, _Tipo) :-
    colore_blu(Blu),
    Raggio is round(Dimensione * 1.1),
    send(Canvas, display, new(C, circle(Raggio * 2))),
    send(C, center, point(X, Y)),
    send(C, pen, 5),
    send(C, colour, colour(Blu)).

evidenzia_board_corrente(W) :->
    get(W, slot, stato_gioco, stato_gioco(_, _, _, TavolaCorrente, _)),
    (   TavolaCorrente >= 0
    ->  get(W, slot, canvas, Canvas),
        larghezza_finestra(Larghezza),
        BI is TavolaCorrente mod 3,
        BJ is TavolaCorrente // 3,
        X is BI * Larghezza // 3,
        Y is BJ * Larghezza // 3,
        Dimensione is Larghezza // 3,
        colore_rosso(Rosso),
        send(Canvas, display, new(Box, box(Dimensione, Dimensione))),
        send(Box, position, point(X, Y)),
        send(Box, fill_pattern, @nil),
        send(Box, colour, colour(Rosso)),
        send(Box, pen, 3)
    ;   true
    ).

unlink(W) :->
    (   get(W, slot, timer_ia, Timer),
        object(Timer)
    ->  send(Timer, stop)
    ;   true
    ),
    send_super(W, unlink).

:- pce_end_class(finestra_ultimate_tictactoe).

trova_cella_cliccata(X, Y, IndiceBoard, IndiceCella) :-
    larghezza_finestra(Larghezza),
    DimensioneQuadrato is Larghezza // 4,
    Offset is (Larghezza // 3 - DimensioneQuadrato) // 2,
    DimensioneSimbolo is Larghezza // 36,
    BI is X // (Larghezza // 3),
    BJ is Y // (Larghezza // 3),
    between(0, 2, BI),
    between(0, 2, BJ),
    IndiceBoard is BJ * 3 + BI,
    BaseX is BI * Larghezza // 3 + Offset + DimensioneQuadrato // 6 - DimensioneSimbolo,
    BaseY is BJ * Larghezza // 3 + Offset + DimensioneQuadrato // 6 - DimensioneSimbolo,
    DimensioneCella is DimensioneQuadrato // 3,
    RelX is X - BaseX,
    RelY is Y - BaseY,
    CI is RelX // DimensioneCella,
    CJ is RelY // DimensioneCella,
    between(0, 2, CI),
    between(0, 2, CJ),
    IndiceCella is CJ * 3 + CI.

avvia_interfaccia :-
    resetta_mosse,
    new(_, finestra_ultimate_tictactoe).

