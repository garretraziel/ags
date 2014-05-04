+step(0) : true <- +places_to_visit([[5,5], [6,26], [1,0], [6,26], [1,0]]);
	do(skip).
+step(N) : moving_plan(_) <- !do_step.
+step(N) : end_plan(X,Y) & pos(X,Y) <- -end_plan(_,_); do(skip).
+step(N) : places_to_visit([[X1,Y1]|T]) & pos(X2,Y2) <-
	.print("dalsi misto");
	-places_to_visit(_);
	?shortest_path(X2,Y2,X1,Y1,[_|TP]);
	.print("cesta spocitana");
	+moving_plan(TP); +end_plan(X1,Y1);
	+places_to_visit(T);
	!do_step.
+step(N) : true <- .print("nevim co mam delat"); do(skip).

+obstacle(X,Y) : true <- +obs(X,Y); !send_all(obs(X,Y)).
+gold(X,Y) : true <- +g(X,Y); !send_all(obs(X,Y)).
+wood(X,Y) : true <- +w(X,Y); !send_all(obs(X,Y)).

+!send_all(X) : true <- !send_slow(X); !send_middle(X); !send_fast(X).
+!send_slow(X) : friend(F) & .substring("Slow", F) <- .send(F, tell, X).
+!send_slow(_) : true <- true.
+!send_middle(X) : friend(F) & .substring("Middle", F) <- .send(F, tell, X).
+!send_middle(_) : true <- true.
+!send_fast(X) : friend(F) & .substring("Fast", F) <- .send(F, tell, X).
+!send_fast(_) : true <- true.

+!do_step : moving_plan([[X,Y]]) <- -moving_plan(_); !do_direction_step(X,Y).
+!do_step : moving_plan([[X,Y]|T]) <- -moving_plan(_); +moving_plan(T);
	!do_direction_step(X,Y).
+!do_step : true <- do(skip).

+!do_direction_step(X1,Y1) : pos(X2,Y2) & ((X1 < X2 & obstacle(X2-1,Y2)) |
		(Y1 < Y2 & obstacle(X2,Y2-1)) | (X1 > X2 & obstacle(X2+1,Y2)) |
		(Y1 > Y2 & obstacle(X2,Y2+1))) <-
	?end_plan(X3,Y3);
	?shortest_path(X2,Y2,X3,Y3,[_|T]);
	-moving_plan(_);
	+moving_plan(T);
	!do_step.
+!do_direction_step(X1,Y1) : pos(X2,Y2) & X1 < X2 <- do(left).
+!do_direction_step(X1,Y1) : pos(X2,Y2) & Y1 < Y2 <- do(up).
+!do_direction_step(X1,Y1) : pos(X2,Y2) & X1 > X2 <- do(right).
+!do_direction_step(X1,Y1) : pos(X2,Y2) & Y1 > Y2 <- do(down).

+?distance(X1,Y1,X2,Y2,D) : true <- D = math.abs(X1-X2) + math.abs(Y1-Y2).

+?pop_lowest_score([H|T], LOWEST, REST) : true <-
	?pop_lowest_score_d(H, T, [], LOWEST, REST).
+?pop_lowest_score_d(LOWEST, [], REST, LOWEST, REST).
+?pop_lowest_score_d([SCORE1|R1], [[SCORE2|R2]|R3], REST_SO_FAR, LOWEST, REST) :
	SCORE1 < SCORE2 <- ?pop_lowest_score_d([SCORE1|R1], R3, [[SCORE2|R2]|REST_SO_FAR], LOWEST, REST).
+?pop_lowest_score_d([SCORE1|R1], [[SCORE2|R2]|R3], REST_SO_FAR, LOWEST, REST) :
	true <- ?pop_lowest_score_d([SCORE2|R2], R3, [[SCORE1|R1]|REST_SO_FAR], LOWEST, REST).

+?is_coord_in_set(X,Y,[[X,Y]|_],true).
+?is_coord_in_set(_,_,[],false).
+?is_coord_in_set(X,Y,[_|R],B) : true <- ?is_coord_in_set(X,Y,R,B).

+?pop_open_if_exists(X,Y,OPENSET,B,E,R) : true <-
	?pop_open_d(X,Y,OPENSET,[],B,E,R).
+?pop_open_d(X,Y,[[C,D,X,Y,P]|T],REST_SO_FAR,true,[C,D,X,Y,P],R) : true <-
	.concat(T,REST_SO_FAR,R).
+?pop_open_d(_,_,[],REST_SO_FAR,false,[0,0,0,0,[]],REST_SO_FAR).
+?pop_open_d(X,Y,[H|T],REST_SO_FAR,B,E,R) : true <-
	?pop_open_d(X,Y,T,[H|REST_SO_FAR],B,E,R).

+?add_neighbour(X,Y,_,_,_,_,OPEN,_,OPEN) : obs(X,Y) | X < 0 | Y < 0
	| (grid(Xg,Yg) & X >= Xg) | (grid(Xg,Yg) & Y >= Yg) <- true.
+?add_neighbour(X,Y,FROM_BEGIN,Xto,Yto,P1,OPEN,CLOSED,NEWOPEN) : true <-
	?is_coord_in_set(X,Y,CLOSED,B);
	if ( B == true ) {
		NEWOPEN = OPEN;
	} else {
		?distance(X,Y,Xto,Yto,D);
		COST = FROM_BEGIN+1+D;
		PATH = [[X,Y]|P1];
		?pop_open_if_exists(X,Y,OPEN,B2,[C,_,_,_,_],REST);
		if ( B2 == true ) {
			if (COST < C) {
				NEWOPEN = [[COST,FROM_BEGIN+1,X,Y,PATH]|REST];
			} else {
				NEWOPEN = OPEN;
			}
		} else {
			NEWOPEN = [[COST,FROM_BEGIN+1,X,Y,PATH]|OPEN];
		}
	}.
	
+?neighbours(X,Y,FROM_BEGIN,Xto,Yto,P1,OPEN,CLOSED,NEWOPEN) : true <-
	?add_neighbour(X,Y-1,FROM_BEGIN,Xto,Yto,P1,OPEN,CLOSED,OPEN2);
	?add_neighbour(X+1,Y,FROM_BEGIN,Xto,Yto,P1,OPEN2,CLOSED,OPEN3);
	?add_neighbour(X,Y+1,FROM_BEGIN,Xto,Yto,P1,OPEN3,CLOSED,OPEN4);
	?add_neighbour(X-1,Y,FROM_BEGIN,Xto,Yto,P1,OPEN4,CLOSED,NEWOPEN).

// reprezentace bude: [SCORE, URAZENA_VZDALENOST, X, Y, [CESTA]]

+!remove_visited : visited(_,_) <- -visited(_,_); !remove_visited.
+!remove_visited : true <- true.

+?shortest_path(X1,Y1,X2,Y2,P) : true <- ?distance(X1,Y1,X2,Y2,DISTANCE);
	?astar(X2,Y2,[[DISTANCE,0,X1,Y1,[[X1,Y1]]]],[],PRev); !remove_visited; .reverse(PRev,P).
+?astar(_,_,[],_,[],_).
+?astar(Xto,Yto,OPEN,CLOSED,P) : true <- ?pop_lowest_score(OPEN,[SCORE,FROM_BEGIN,X,Y,P1],REST);
	if ( Xto == X & Yto == Y) {
		P=P1;
	} else {
		if (visited(X,Y)) {
			.print("FAIL!!!");
		}
		+visited(X,Y);
		?neighbours(X,Y,FROM_BEGIN,Xto,Yto,P1,REST,CLOSED,NEWOPEN);
		?astar(Xto,Yto,NEWOPEN,[[X,Y]|CLOSED],P);
	}.
