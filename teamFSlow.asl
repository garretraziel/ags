+step(0) : true <- +goingto(30,30); ?shortest_path(30,30,30,30,P); .print(P).
+step(X) : goingto(_,_) <- !do_direction_step.
+step(X) : true <- do(skip).

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

// reprezentace bude: [SCORE, URAZENA_VZDALENOST, X, Y, [CESTA]]

/*+?shortest_path(X1,Y1,X2,Y2,P) : true <- ?distance(X1,Y1,X2,Y2,DISTANCE);
	?astar(X2,Y2,[[DISTANCE,0,X1,Y1,[]]],[], P).
+?astar(X,Y,OPEN,CLOSED,P) : true <- ?pop_lowest_score(OPEN,[SCORE],REST);*/
