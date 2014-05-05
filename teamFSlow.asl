+!add_known_path(Xfrom,Yfrom,Xto,Yto,P) : true <-
	+known_path(Xfrom,Yfrom,Xto,Yto,P);
	+known_path(Xfrom,Yfrom,Xto+1,Yto,P);
	+known_path(Xfrom,Yfrom,Xto+1,Yto+1,P);
	+known_path(Xfrom,Yfrom,Xto,Yto+1,P);
	+known_path(Xfrom,Yfrom,Xto-1,Yto,P);
	+known_path(Xfrom,Yfrom,Xto-1,Yto-1,P);
	+known_path(Xfrom,Yfrom,Xto,Yto-1,P);
	+known_path(Xfrom,Yfrom,Xto-1,Yto+1,P);
	+known_path(Xfrom,Yfrom,Xto+1,Yto-1,P).

//+step(0) : true <- ?depot(X,Y); +places_to_visit([[X,Y]]); do(skip).
+step(0) : true <- +places_to_visit([[6,26],[16,16],[6,26],[16,16]]); do(skip).
+step(N) : have_to_load & pos(X,Y) <- -have_to_load; do(pick); -g(X,Y);
	?depot(XD,YD); +places_to_visit([[XD,YD]]); +have_to_unload.
+step(N) : have_to_unload & depot(X,Y) & pos(X,Y) <- -have_to_unload; do(drop).
+step(N) : moving_plan(_) <- .print("bla2");!do_step.
+step(N) : end_plan(X,Y) & pos(X,Y) & current_path(X1D,Y1D,X2D,Y2D,P) <-
	-current_path(X1D,Y1D,X2D,Y2D,P);
	//!add_known_path(X2D,Y2D,X1D,Y1D,P);
	.reverse(P,PRev);
	//!add_known_path(X1D,Y1D,X2D,Y2D,PRev);
	-end_plan(_,_); do(skip).
+step(N) : end_plan(X1,Y1) & pos(X2,Y2) <-
	+current_path(X2,Y2,X1,Y1,[[X2,Y2]]);
	.print("dalsi misto");
	?shortest_path(X2,Y2,X1,Y1,TP);
	.print("cesta spocitana");
	+moving_plan(TP);
	!do_step.
+step(N) : places_to_visit([[X1,Y1]|T]) & pos(X2,Y2) & known_path(X2,Y2,X1,Y1,P) <-
	-places_to_visit(_);
	.print(P);
	+moving_plan(P); +end_plan(X1,Y1);
	+places_to_visit(T);
	!do_step.
+step(N) : places_to_visit([[X1,Y1]|T]) & pos(X2,Y2) <-
	+current_path(X2,Y2,X1,Y1,[[X2,Y2]]);
	.print("dalsi misto");
	-places_to_visit(_);
	?shortest_path(X2,Y2,X1,Y1,TP);
	.print("cesta spocitana");
	+moving_plan(TP); +end_plan(X1,Y1);
	+places_to_visit(T);
	!do_step.
+step(N) : is_waiting(X,Y,A) & pos(X,Y) <- .send(A,achieve,load_it); +have_to_load;
	do(skip).
+step(N) : g(X,Y) <- +places_to_visit([[X,Y]]); !command_middle(please_go(X,Y)); !do_step.
+step(N) : true <- do(skip).

+obstacle(X,Y) : true <- +obs(X,Y); !send_all(obs(X,Y)).
+gold(X,Y) : true <- +g(X,Y); !command_middle(g(X,Y)).
+wood(X,Y) : true <- +w(X,Y); !send_all(w(X,Y)).
+known_path(X1,Y1,X2,Y2,P) : true <- +known_path(X1,Y1,X2,Y2,P); !send_all(known_path(X1,Y1,X2,Y2,P)).

+!g(X,Y) : g(X,Y) <- true.
+!g(X,Y) : true <- +g(X,Y).

+!minus_g(X,Y) : g(X,Y) <- -g(X,Y).
+!minus_g(X,Y) : true <- true.

+!send_all(X) : true <- !send_slow(X); !send_middle(X); !send_fast(X).
+!send_slow(X) : friend(F) & .substring("Slow", F) <- .send(F, tell, X).
+!send_slow(_) : true <- true.
+!send_middle(X) : friend(F) & .substring("Middle", F) <- .send(F, tell, X).
+!send_middle(_) : true <- true.
+!send_fast(X) : friend(F) & .substring("Fast", F) <- .send(F, tell, X).
+!send_fast(_) : true <- true.

+!command_slow(X) : friend(F) & .substring("Slow", F) <- .send(F, achieve, X).
+!command_slow(X) : true <- true.
+!command_middle(X) : friend(F) & .substring("Middle", F) <- .send(F, achieve, X).
+!command_middle(X) : true <- true.

+!i_am_there(X,Y)[source(A)] : pos(X,Y) <- .send(A,achieve,load_it).
+!i_am_there(X,Y)[source(A)] : true <- +is_waiting(X,Y,A).

+!do_step : moving_plan([[X,Y]]) <- -moving_plan(_); !do_direction_step(X,Y).
+!do_step : moving_plan([[X,Y]|T]) <- -moving_plan(_); +moving_plan(T);
	!do_direction_step(X,Y).
+!do_step : true <- do(skip).

+!do_direction_step(X1,Y1) : pos(X2,Y2) & ((X1 < X2 & obstacle(X2-1,Y2)) |
		(Y1 < Y2 & obstacle(X2,Y2-1)) | (X1 > X2 & obstacle(X2+1,Y2)) |
		(Y1 > Y2 & obstacle(X2,Y2+1))) <-
	?end_plan(X3,Y3);
	?shortest_path(X2,Y2,X3,Y3,T);
	-moving_plan(_);
	+moving_plan(T);
	!do_step.
+!do_direction_step(X1,Y1) : pos(X1,Y1) <- !do_step.
+!do_direction_step(X1,Y1) : pos(X2,Y2) & X1 < X2 & current_path(X1D,Y1D,X2D,Y2D,P) <-
	-current_path(X1D,Y1D,X2D,Y2D,P);
	+current_path(X1D,Y1D,X2D,Y2D,[[X1,Y1]|P]); 
	do(left).
+!do_direction_step(X1,Y1) : pos(X2,Y2) & Y1 < Y2 & current_path(X1D,Y1D,X2D,Y2D,P) <-
	-current_path(X1D,Y1D,X2D,Y2D,P);
	+current_path(X1D,Y1D,X2D,Y2D,[[X1,Y1]|P]);
	do(up).
+!do_direction_step(X1,Y1) : pos(X2,Y2) & X1 > X2 & current_path(X1D,Y1D,X2D,Y2D,P) <-
	-current_path(X1D,Y1D,X2D,Y2D,P);
	+current_path(X1D,Y1D,X2D,Y2D,[[X1,Y1]|P]);
	do(right).
+!do_direction_step(X1,Y1) : pos(X2,Y2) & Y1 > Y2 & current_path(X1D,Y1D,X2D,Y2D,P) <-
	-current_path(X1D,Y1D,X2D,Y2D,P);
	+current_path(X1D,Y1D,X2D,Y2D,[[X1,Y1]|P]);
	do(down).
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
+?pop_open_d(X,Y,[[C,D,X,Y]|T],REST_SO_FAR,true,[C,D,X,Y],R) : true <-
	.concat(T,REST_SO_FAR,R).
+?pop_open_d(_,_,[],REST_SO_FAR,false,[0,0,0,0],REST_SO_FAR).
+?pop_open_d(X,Y,[H|T],REST_SO_FAR,B,E,R) : true <-
	?pop_open_d(X,Y,T,[H|REST_SO_FAR],B,E,R).

+?add_neighbour(X,Y,_,_,_,_,_,OPEN,_,OPEN) : obs(X,Y) | X < 0 | Y < 0
	| (grid(Xg,Yg) & X >= Xg) | (grid(Xg,Yg) & Y >= Yg) <- true.
+?add_neighbour(X,Y,XOrig,YOrig,FROM_BEGIN,Xto,Yto,OPEN,CLOSED,NEWOPEN) : true <-
	?is_coord_in_set(X,Y,CLOSED,B);
	if ( B == true ) {
		NEWOPEN = OPEN;
	} else {
		?distance(X,Y,Xto,Yto,D);
		COST = FROM_BEGIN+1+D;
		?pop_open_if_exists(X,Y,OPEN,B2,[C,_,_,_],REST);
		if ( B2 == true ) {
			if (COST < C) {
				NEWOPEN = [[COST,FROM_BEGIN+1,X,Y]|REST];
				-came_from(X,Y,_,_);
				+came_from(X,Y,XOrig,YOrig);
			} else {
				NEWOPEN = OPEN;
			}
		} else {
			NEWOPEN = [[COST,FROM_BEGIN+1,X,Y]|OPEN];
			+came_from(X,Y,XOrig,YOrig);
		}
	}.
	
+?neighbours(X,Y,FROM_BEGIN,Xto,Yto,OPEN,CLOSED,NEWOPEN) : true <-
	?add_neighbour(X,Y-1,X,Y,FROM_BEGIN,Xto,Yto,OPEN,CLOSED,OPEN2);
	?add_neighbour(X+1,Y,X,Y,FROM_BEGIN,Xto,Yto,OPEN2,CLOSED,OPEN3);
	?add_neighbour(X,Y+1,X,Y,FROM_BEGIN,Xto,Yto,OPEN3,CLOSED,OPEN4);
	?add_neighbour(X-1,Y,X,Y,FROM_BEGIN,Xto,Yto,OPEN4,CLOSED,NEWOPEN).

// reprezentace bude: [SCORE, URAZENA_VZDALENOST, X, Y, [CESTA]]

+!remove_came_from : came_from(_,_,_,_) <- -came_from(_,_,_,_); !remove_came_from.
+!remove_came_from : true <- true.

+!start_reconstruction(X,Y,P) : true <- !reconstruct_path(X,Y,[],P).
+!reconstruct_path(X,Y,R,P) : came_from(X,Y,XOrig,YOrig) <- !reconstruct_path(XOrig,YOrig,[[X,Y]|R],P).
+!reconstruct_path(X,Y,P,[[X,Y]|P]).

+!remove_visited : visited(_,_) <- -visited(_,_); !remove_visited.
+!remove_visited : true <- true.

+?shortest_path(X1,Y1,X2,Y2,P) : true <- ?distance(X1,Y1,X2,Y2,DISTANCE);
	?astar(X2,Y2,[[DISTANCE,0,X1,Y1]],[],P,0);
	.reverse(P,PRev).
+?astar(_,_,[],_,[],_,_).
+?astar(Xto,Yto,OPEN,CLOSED,P,STEPS) : true <- ?pop_lowest_score(OPEN,[SCORE,FROM_BEGIN,X,Y],REST);
	if ( Xto == X & Yto == Y ) {
		!start_reconstruction(Xto,Yto,P);
		!remove_came_from;
		!remove_visited;
	} else {
		+visited(X,Y);
		.print(STEPS);
		?neighbours(X,Y,FROM_BEGIN,Xto,Yto,REST,CLOSED,NEWOPEN);
		?astar(Xto,Yto,NEWOPEN,[[X,Y]|CLOSED],P,STEPS+1);
	}.
