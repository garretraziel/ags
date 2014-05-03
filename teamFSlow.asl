+step(0) : true <- +goingto(3,11); !do_direction_step.
+step(X) : goingto(_,_) <- !do_direction_step.
+step(X) : true <- .print("konec").

+!do_direction_step : goingto(X1,Y1) & pos(X1,Y1) <- -goingto(X1, Y1); do(skip).
+!do_direction_step : goingto(X1,Y1) & pos(X2,Y2) & X1 < X2 <- do(left).
+!do_direction_step : goingto(X1,Y1) & pos(X2,Y2) & Y1 < Y2 <- do(up).
+!do_direction_step : goingto(X1,Y1) & pos(X2,Y2) & X1 > X2 <- do(right).
+!do_direction_step : goingto(X1,Y1) & pos(X2,Y2) & Y1 > Y2 <- do(down).
+!do_direction_step : true <- .print("fail"); do(skip).

+?distance(X1,Y1,X2,Y2,D) : true <- D = math.abs(X1-X2) + math.abs(Y1-Y2).

+?pop_lowest_score([H|T], LOWEST, REST) : true <- ?pop_lowest_score_d(H, T, [], LOWEST, REST).
+?pop_lowest_score_d(LOWEST, [], REST, LOWEST, REST).
+?pop_lowest_score_d([SCORE1|R1], [[SCORE2|R2]|R3], REST_SO_FAR, LOWEST, REST) :
	SCORE1 < SCORE2 <- ?pop_lowest_score_d([SCORE1|R1], R3, [[SCORE2|R2]|REST_SO_FAR], LOWEST, REST).
+?pop_lowest_score_d([SCORE1|R1], [[SCORE2|R2]|R3], REST_SO_FAR, LOWEST, REST) :
	SCORE2 <= SCORE1 <- ?pop_lowest_score_d([SCORE2|R2], R3, [[SCORE1|R1]|REST_SO_FAR], LOWEST, REST).

+?is_coord_in_set(X,Y,[[X,Y]|_],true).
+?is_coord_in_set(_,_,[],false).
+?is_coord_in_set(X,Y,[_|R],B) : true <- ?is_coord_in_set(X,Y,R,B).

+?add_north(X,Y,_,_,_,_,_,N,N) : obstacle(X,Y-1) <- true.
+?add_north(X,Y,FROM_BEGIN,Xto,Yto,P1,CLOSED,N1,N2) : true <-
	?is_coord_in_set(X,Y-1,CLOSED,B);
	if ( B == true ) {
		N2 = N1;
	} else {
		?distance(X,Y-1,Xto,Yto,D);
		COST = FROM_BEGIN+1+D;
		PATH = [[X,Y-1]|P1];
		N2 = [[COST,FROM_BEGIN+1,X,Y-1,PATH]|N1];
	}.

+?add_east(X,Y,_,_,_,_,_,N,N) : obstacle(X+1,Y) <- true.
+?add_east(X,Y,FROM_BEGIN,Xto,Yto,P1,CLOSED,N1,N2) : true <-
	?is_coord_in_set(X+1,Y,CLOSED,B);
	if ( B == true ) {
		N2 = N1;
	} else {
		?distance(X+1,Y,Xto,Yto,D);
		COST = FROM_BEGIN+1+D;
		PATH = [[X+1,Y]|P1];
		N2 = [[COST,FROM_BEGIN+1,X+1,Y,PATH]|N1];
	}.

+?add_south(X,Y,_,_,_,_,_,N,N) : obstacle(X,Y+1) <- true.
+?add_south(X,Y,FROM_BEGIN,Xto,Yto,P1,CLOSED,N1,N2) : true <-
	?is_coord_in_set(X,Y+1,CLOSED,B);
	if ( B == true ) {
		N2 = N1;
	} else {
		?distance(X,Y+1,Xto,Yto,D);
		COST = FROM_BEGIN+1+D;
		PATH = [[X,Y+1]|P1];
		N2 = [[COST,FROM_BEGIN+1,X,Y+1,PATH]|N1];
	}.

+?add_west(X,Y,_,_,_,_,_,N,N) : obstacle(X-1,Y) <- true.
+?add_west(X,Y,FROM_BEGIN,Xto,Yto,P1,CLOSED,N1,N2) : true <-
	?is_coord_in_set(X-1,Y,CLOSED,B);
	if ( B == true ) {
		N2 = N1;
	} else {
		?distance(X-1,Y,Xto,Yto,D);
		COST = FROM_BEGIN+1+D;
		PATH = [[X-1,Y]|P1];
		N2 = [[COST,FROM_BEGIN+1,X-1,Y,PATH]|N1];
	}.
	
+?neighbours(X,Y,FROM_BEGIN,Xto,Yto,P1,CLOSED,N) : true <-
	?add_north(X,Y,FROM_BEGIN,Xto,Yto,P1,CLOSED,[],N1);
	?add_east(X,Y,FROM_BEGIN,Xto,Yto,P1,CLOSED,N1,N2);
	?add_south(X,Y,FROM_BEGIN,Xto,Yto,P1,CLOSED,N2,N3);
	?add_west(X,Y,FROM_BEGIN,Xto,Yto,P1,CLOSED,N3,N).

// reprezentace bude: [SCORE, URAZENA_VZDALENOST, X, Y, [CESTA]]

+?shortest_path(X1,Y1,X2,Y2,P) : true <- ?distance(X1,Y1,X2,Y2,DISTANCE);
	?astar(X2,Y2,[[DISTANCE,0,X1,Y1,[[X1,Y1]]]],[], PRev); .reverse(PRev,P).
+?astar(_,_,[],_,[]).
+?astar(Xto,Yto,OPEN,CLOSED,P) : true <- ?pop_lowest_score(OPEN,[SCORE,FROM_BEGIN,X,Y,P1],REST);
	if ( Xto == X & Yto == Y) {
		P=P1;
	} else {
		?neighbours(X,Y,FROM_BEGIN,Xto,Yto,P1,CLOSED,N);
		.concat(N,REST,NEWOPEN);
		?astar(Xto,Yto,NEWOPEN,[[X,Y]|CLOSED],P);
	}.
