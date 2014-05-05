//+step(0) : true <- ?depot(X,Y); +places_to_visit([[X,Y]]); do(skip).
+step(0) : true <- +places_to_visit([[6,26],[16,16],[6,26],[16,16]]); do(skip).
+step(N) : moving_plan(_) <- .print("bla2");!do_step.
+step(N) : end_plan(X,Y) & pos(X,Y) <-
	-end_plan(_,_); do(skip).
+step(N) : places_to_visit([[X1,Y1]|T]) & pos(X2,Y2) <-
	+current_path(X2,Y2,X1,Y1,[[X2,Y2]]);
	-places_to_visit(_);
	.print("planuji...");
	?astar(X2,Y2,X1,Y1,TP);
	+moving_plan(TP); +end_plan(X1,Y1);
	+places_to_visit(T);
	!do_step.
+step(N) : true <- .print("nevim co mam delat").

+obstacle(X,Y) : true <- +obs(X,Y).

+!do_step : moving_plan([[X,Y]]) <- -moving_plan(_); !do_direction_step(X,Y).
+!do_step : moving_plan([[X,Y]|T]) <- -moving_plan(_); +moving_plan(T);
	!do_direction_step(X,Y).
+!do_step : true <- do(skip).

+!do_direction_step(X1,Y1) : pos(X2,Y2) & ((X1 < X2 & obstacle(X2-1,Y2)) |
		(Y1 < Y2 & obstacle(X2,Y2-1)) | (X1 > X2 & obstacle(X2+1,Y2)) |
		(Y1 > Y2 & obstacle(X2,Y2+1))) <-
	?end_plan(X3,Y3);
	?astar(X2,Y2,X3,Y3,T);
	-moving_plan(_);
	+moving_plan(T);
	!do_step.
+!do_direction_step(X1,Y1) : pos(X1,Y1) <- !do_step.
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

// reprezentace bude: [SCORE, URAZENA_VZDALENOST, X, Y]

+!start_reconstruction(X,Y,P) : true <- !reconstruct_path(X,Y,[],P).
+!reconstruct_path(X,Y,R,P) : came_from(X,Y,XOrig,YOrig) <- !reconstruct_path(XOrig,YOrig,[[X,Y]|R],P).
+!reconstruct_path(X,Y,P,[[X,Y]|P]).

+?astar(Xfrom,Yfrom,Xto,Yto,P) : true <-
	?distance(Xfrom,Yfrom,Xto,Yto,D);
	+steps(0);
	+x(Xfrom);
	+y(Yfrom);
	+from_start(0);
	+openset([]);
	+closedset([[Xfrom,Yfrom]]);
	while ( (x(X) & y(Y)) & (Xto \== X | Yto \== Y) ) {
		?steps(N);
		-+steps(N+1);
		.print(N);
		
		?openset(OPEN);
		?closedset(CLOSED);
		?from_start(FROM_BEGIN);
		
		?neighbours(X,Y,FROM_BEGIN,Xto,Yto,OPEN,CLOSED,NEWOPEN);
		
		?pop_lowest_score(NEWOPEN,[NEWSCORE,NEW_FROM_BEGIN,Xnew,Ynew],REST);
		
		-+x(Xnew);
		-+y(Ynew);
		-+openset(REST);
		-+closedset([[Xnew,Ynew]|CLOSED]);
		-+from_start(NEW_FROM_BEGIN);
	}
	.print("dopocitano");
	!start_reconstruction(Xto,Yto,P);
	.abolish(openset(_));
	.abolish(closedset(_));
	.abolish(came_from(_,_,_,_)).
