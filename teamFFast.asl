//+step(0) : true <- ?depot(X,Y); +places_to_visit([[X,Y]]); do(skip).
+step(0) : true <- !plan_all_ng; +places_counter(0);
	do(skip);do(skip);do(skip).
+step(N) : true <- ?moves_left(M); -+moves(M); !react.

@mydo[atomic] +!do(What) : true <-
	if(moves(M) & M > 0){
		-+moves(M-1);
		do(What);
	}.

+!do_remaining_skip : true <-
	if(moves(3)){
		!do(skip);!do(skip);!do(skip);
	} else {
	if(moves(2)){
		!do(skip);!do(skip);
	} else {
	if(moves(1)){
		!do(skip);
	}
	}
	}. 

	
+!do_remaining_steps : true <-
	if(moves(3)){
		!do_step;
		!do_step;
		!do_step;
	} else {
	if(moves(2)){
		!do_step;
		!do_step;
	} else {
	if(moves(1)){
		!do_step;
	}
	}
	}.
	
+!react : moving_plan([]) <- 
	-moving_plan(_);
	-end_plan(_,_);
	!react.
+!react : moving_plan(_) <- !do_remaining_steps.
+!react : end_plan(X,Y) & pos(X,Y) <-
	-end_plan(_,_); 
	!react.
+!react : place_to_check(_,_,_) <-
	!plan_next_point;
	!react.
+!react : true <- !do_remaining_skip.

+!plan_next_point : places_counter(C) & final(C) <- 
	for (place_to_check(Ct,X,Y)){
		-place_to_check(Ct,X,Y);
	}
	+scan_done.
	-final(C).
+!plan_next_point : places_counter(C) & place_to_check(C,X1,Y1) <-
	-+places_counter(C+1);
	//-place_to_check(C,X1,Y1);	
	!plan_path(X1,Y1).
+!plan_next_point : places_counter(C) & place_to_check(_,_,_) <-
	-+places_counter(C+1);
	!plan_next_point.
+!plan_next_point : true <- +scan_done.


+!slow_beacon(X,Y) : true <- !update_places_pos(X,Y).
+!middle_beacon(X,Y) : true <- !update_places_pos(X,Y).

+!plan_path(Xto,Yto) : pos(Xfrom,Yfrom) <-
	?astar(Xfrom,Yfrom,Xto,Yto,TP);
	+moving_plan(TP); 
	+end_plan(Xto,Yto).

+obstacle(X,Y) : obs(X,Y) <- true.
+obstacle(X,Y) : true <- +obs(X,Y); !tellall(add_obstacle(X,Y)).
+gold(X,Y) : g(X,Y) <- true.
+gold(X,Y) : true <- +g(X,Y); !tellall(add_gold(X,Y)).
+wood(X,Y) : w(X,Y) <- true.
+wood(X,Y) : true <- +w(X,Y); !tellall(add_wood(X,Y)).

+!add_gold(X,Y) : true <- +g(X,Y).
+!add_wood(X,Y) : true <- +w(X,Y).
+!add_obstacle(X,Y) : true <- +obs(X,Y).

+!tellall(X) : true <-
	for (friend(F)) {
		.send(F,achieve,X);
	}.

+!plan_all_ng : true <-
	?grid_size(Xg,Yg);
	+counter(0);
	for (.range(X,0,(Xg-1)/4)) {
		for (.range(Y,0,Yg-1))
		{	
			?counter(C);
			-+counter(C+1);
			if(X mod 2 == 0){
				+place_to_check(C,4*X,Y);
			} else {
				+place_to_check(C,4*X,Yg-Y-1);
			}
		}
	};
	for (.range(Y,0,(Yg-1)/2)) {
		for (.range(X,0,Xg-1))
		{	
			?counter(C);
			-+counter(C+1);
			if(Y mod 2 == 0){
				+place_to_check(C,X,2*Y);
			} else {
				+place_to_check(C,Xg-X-1,2*Y);
			}
		}
	};
	if(Yg mod 2 == 0){
		+interlacing(1);
	} else {
		+interlacing(2);
	}
	?interlacing(I);
	for (.range(Y,0,(Yg-1)/2)) {
		for (.range(X,0,Xg-1))
		{	
			?counter(C);
			-+counter(C+1);
			if(Y mod 2 == 0){
				+place_to_check(C,X,Yg-(2*Y)-I);
			} else {
				+place_to_check(C,Xg-X-1,Yg-(2*Y)-I);
			}
		}
	};
	?counter(C);
	+final(C+1);
	-counter(_).

+!do_step : moving_plan([[X,Y]]) <- -moving_plan(_); !do_direction_step(X,Y).
+!do_step : moving_plan([[X,Y]|T]) <- -moving_plan(_); +moving_plan(T); !do_direction_step(X,Y).
+!do_step : scan_done <- !do_remaining_skip.
+!do_step : true <- .print("harra harrr!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
	-end_plan(_,_);
	!plan_next_point;
	!do_step.

+!do_direction_step(X1,Y1) : pos(X2,Y2) & ((X1 < X2 & obstacle(X2-1,Y2)) |
		(Y1 < Y2 & obstacle(X2,Y2-1)) | (X1 > X2 & obstacle(X2+1,Y2)) |
		(Y1 > Y2 & obstacle(X2,Y2+1))) <-
	?end_plan(X3,Y3);
	?astar(X2,Y2,X3,Y3,T);
	-moving_plan(_);
	+moving_plan(T);
	!do_step.
+!do_direction_step(X1,Y1) : pos(X1,Y1) <- !do_step; !update_places.
+!do_direction_step(X1,Y1) : pos(X2,Y2) & X1 < X2 <- !do(left); !update_places.
+!do_direction_step(X1,Y1) : pos(X2,Y2) & Y1 < Y2 <- !do(up); !update_places.
+!do_direction_step(X1,Y1) : pos(X2,Y2) & X1 > X2 <- !do(right); !update_places.
+!do_direction_step(X1,Y1) : pos(X2,Y2) & Y1 > Y2 <- !do(down); !update_places.

+!update_places_pos(X,Y) : true <-
	-place_to_check(_,X,Y);
	-place_to_check(_,X,Y+1);
	-place_to_check(_,X+1,Y);
	-place_to_check(_,X+1,Y+1);
	-place_to_check(_,X,Y-1);
	-place_to_check(_,X-1,Y);
	-place_to_check(_,X-1,Y-1);
	-place_to_check(_,X-1,Y+1);
	-place_to_check(_,X+1,Y-1).

+!update_places : pos(X,Y) <- 
	!update_places_pos(X,Y).

+?distance(X1,Y1,X2,Y2,D) : true <- D = math.abs(X1-X2) + math.abs(Y1-Y2).

+!add_neighbour(X,Y) : true <-
 	?grid_size(Xg,Yg);
	if ((not obs(X,Y)) & X >= 0 & Y >= 0 & X < Xg & Y < Yg) {
		if ( not closedset(X,Y) ) {
			?xto(Xto); ?yto(Yto);
			?x(XOrig); ?y(YOrig);
			?from_start(FROM_BEGIN);
			
			?distance(X,Y,Xto,Yto,D);
			COST = FROM_BEGIN+1+D;
			if ( openset(COLD,_,X,Y) ) {
				if (COST < COLD) {
					-openset(COLD,_,X,Y);
					+openset(COST,FROM_BEGIN+1,X,Y);
					-came_from(X,Y,_,_);
					+came_from(X,Y,XOrig,YOrig);
				}
			} else {
				+openset(COST,FROM_BEGIN+1,X,Y);
				+came_from(X,Y,XOrig,YOrig);
			}
		}
	}.
	
+!add_neighbours(X,Y) : true <-
	!add_neighbour(X,Y-1);
	!add_neighbour(X+1,Y);
	!add_neighbour(X,Y+1);
	!add_neighbour(X-1,Y).

// reprezentace bude: [SCORE, URAZENA_VZDALENOST, X, Y]

+!start_reconstruction(X,Y,P) : true <- !reconstruct_path(X,Y,[],P).
+!reconstruct_path(X,Y,R,P) : came_from(X,Y,XOrig,YOrig) <- !reconstruct_path(XOrig,YOrig,[[X,Y]|R],P).
+!reconstruct_path(X,Y,P,[[X,Y]|P]).

+!pop_lowest_open(XLOW,YLOW,FROM_BEGIN_LOW) : true <-
	// vyber nejmensiho prvku a jeho pop z openset
	?openset(CTmp,DTmp,XTmp,YTmp);
	+open_lowest(CTmp,DTmp,XTmp,YTmp);
	for (openset(CT,DT,XT,YT)) {
		?open_lowest(CL,DL,XL,YL);
		if (CT < CL) {
			-open_lowest(CL,DL,XL,YL);
			+open_lowest(CT,DT,XT,YT);
		}
	}

	?open_lowest(Cnew,FROM_BEGIN_LOW,XLOW,YLOW);
	-open_lowest(Cnew,FROM_BEGIN_LOW,XLOW,YLOW);
	-openset(Cnew,FROM_BEGIN_LOW,XLOW,YLOW).

+?astar(Xfrom,Yfrom,Xto,Yto,[]) : obs(Xto,Yto) <- true.	
	
+?astar(Xfrom,Yfrom,Xto,Yto,P) : true <-
	?distance(Xfrom,Yfrom,Xto,Yto,D);
	+x(Xfrom);
	+y(Yfrom);
	+xto(Xto);
	+yto(Yto);
	+from_start(0);
	+closedset(Xfrom,Yfrom);
	+open_not_empty;
	while ( (x(X) & y(Y)) & (Xto \== X | Yto \== Y) & open_not_empty) {
		!add_neighbours(X,Y);
		
		if (openset(_,_,_,_)) {
			!pop_lowest_open(Xnew,Ynew,NEW_FROM_BEGIN);
		
			-+x(Xnew);
			-+y(Ynew);

			+closedset(Xnew,Ynew);
			-+from_start(NEW_FROM_BEGIN);
		} else {
			-open_not_empty;
		}
	}
	if (open_not_empty) {
		!start_reconstruction(Xto,Yto,P);
		-open_not_empty;
	} else {
		P = [];
	}
	.abolish(openset(_,_,_,_));
	.abolish(closedset(_,_));
	.abolish(came_from(_,_,_,_));
	.abolish(from_start(_));
	.abolish(x(_));
	.abolish(y(_));
	.abolish(xto(_));
	.abolish(yto(_)).
