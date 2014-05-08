+step(0) : true <- 
	+idle;
	?pos(Xfrom,Yfrom); ?depot(Xto,Yto);
	?astar(Xfrom, Yfrom, Xto, Yto, Plan);
	+moving_plan(Plan); +end_plan(Xto,Yto);
	do(skip).
+step(N) : true <- !react(N).


+!middle_beacon(X,Y) : true <- -+middle_pos(X,Y).
+!slow_beacon(_,_) : true <- true.

+!react(N) : idle & g(Xl,Yl) & carrying_wood(0) <- -idle;
	?pos(Xp,Yp); 
	?astar(Xp,Yp,Xl,Yl,DistPlan);
	.length(DistPlan, Dist);
	+nearest(Xl,Yl,Dist);
	for(g(Xt,Yt)){
		?astar(Xp,Yp,Xt,Yt,DistPlanFor);
		.length(DistPlanFor, DistFor);
		if(nearest(_,_,Dn) & Dn > DistFor){
			-+nearest(Xt,Yt,DistFor);
		}		
	}
	-moving_plan(_); -end_plan(_,_);
	?nearest(X,Y,_);
	-nearest(_,_,_);
	?astar(Xp,Yp,X,Y,TP);
	+moving_plan(TP); +end_plan(X,Y); !inform_middle(X,Y,cg); !do_step.
	
+!react(N) : idle & w(Xl,Yl) & carrying_gold(0) <- -idle;
	?pos(Xp,Yp); 
	?astar(Xp,Yp,Xl,Yl,DistPlan);
	.length(DistPlan, Dist);
	+nearest(Xl,Yl,Dist);
	for(g(Xt,Yt)){
		?astar(Xp,Yp,Xt,Yt,DistPlanFor);
		.length(DistPlanFor, DistFor);
		if(nearest(_,_,Dn) & Dn > DistFor){
			-+nearest(Xt,Yt,DistFor);
		}		
	}
	-moving_plan(_); -end_plan(_,_);
	?nearest(X,Y,_);
	-nearest(_,_,_);
	?astar(Xp,Yp,X,Y,TP);
	+moving_plan(TP); +end_plan(X,Y); !inform_middle(X,Y,cw); !do_step.
	
	
+!react(N) : moving_plan(M) <- !do_step.
+!react(N) : end_plan(X,Y) & pos(X,Y) & middle_is_waiting <-
	!tellmiddle(load_it(N+1));
	-middle_is_waiting;
	-end_plan(_,_);
	+load;
	do(skip).
@react[atomic] +!react(N) : pos(X,Y) & load <-
	if(g(X,Y)){
		-g(X,Y);
	} else {
		-w(X,Y);
	}
	//hahha!
	-load;
	?carrying_capacity(CapMax);
	?carrying_wood(Woods);
	?carrying_gold(Golds);
	if(CapMax-1 == Woods+Golds){
		+have_to_unload;
		?pos(Xp,Yp);
		?depot(Xd,Yd);
		?astar(Xp,Yp,Xd,Yd,TP);
		+moving_plan(TP);
		+end_plan(Xd,Yd);
	
	} else {
		+idle;
	}
	do(pick).
+!react(N) : end_plan(X,Y) & pos(X,Y) & have_to_unload <-
	-end_plan(X,Y);
	-have_to_unload;
	+idle;
	do(drop).
+!react(N) : true <- do(skip).

+obstacle(X,Y) : obs(X,Y) <- true.
+obstacle(X,Y) : true <- +obs(X,Y); !tellall(add_obstacle(X,Y)).
+gold(X,Y) : g(X,Y) <- true.
+gold(X,Y) : true <- +g(X,Y); !tellall(add_gold(X,Y)).
+wood(X,Y) : w(X,Y) <- true.
+wood(X,Y) : true <- +w(X,Y); !tellall(add_wood(X,Y)).

+!add_gold(X,Y) : true <- +g(X,Y).
+!add_wood(X,Y) : true <- +w(X,Y).
+!add_obstacle(X,Y) : true <- +obs(X,Y).

+!inform_middle(X,Y,What) : friend(F) & .substring("Middle", F) <-
	.send(F,achieve,please_go(X,Y,What)).

+!inform_fast(X,Y) : friend(F) & .substring("Fast", F) <-
	.send(F,achieve,please_go(X,Y)).

+!tellall(X) : true <-
	for (friend(F)) {
		.send(F,achieve,X);
	}.
	
+!tellmiddle(X) : friend(F) & .substring("Middle", F) <-
	.send(F,achieve,X).
	
+!i_am_ready : true <- +middle_is_waiting.

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
+!do_direction_step(X1,Y1) : pos(X2,Y2) & X1 < X2 <- do(left); !tellall(slow_beacon(X1,X2)).
+!do_direction_step(X1,Y1) : pos(X2,Y2) & Y1 < Y2 <- do(up); !tellall(slow_beacon(X1,X2)).
+!do_direction_step(X1,Y1) : pos(X2,Y2) & X1 > X2 <- do(right); !tellall(slow_beacon(X1,X2)).
+!do_direction_step(X1,Y1) : pos(X2,Y2) & Y1 > Y2 <- do(down); !tellall(slow_beacon(X1,X2)).

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

+?astar(Xfrom,Yfrom,Xto,Yto,P) : true <-
	?distance(Xfrom,Yfrom,Xto,Yto,D);
	+x(Xfrom);
	+y(Yfrom);
	+xto(Xto);
	+yto(Yto);
	+from_start(0);
	+closedset(Xfrom,Yfrom);
	while ( (x(X) & y(Y)) & (Xto \== X | Yto \== Y) ) {
		!add_neighbours(X,Y);
		
		!pop_lowest_open(Xnew,Ynew,NEW_FROM_BEGIN);
		
		-+x(Xnew);
		-+y(Ynew);

		+closedset(Xnew,Ynew);
		-+from_start(NEW_FROM_BEGIN);
	}
	!start_reconstruction(Xto,Yto,P);
	.abolish(openset(_,_,_,_));
	.abolish(closedset(_,_));
	.abolish(came_from(_,_,_,_));
	.abolish(from_start(_));
	.abolish(x(_));
	.abolish(y(_));
	.abolish(xto(_));
	.abolish(yto(_)).
