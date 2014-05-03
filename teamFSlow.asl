+step(0) : true <- +goingto(30,30); !do_direction_step.
+step(X) : goingto(_,_) <- !do_direction_step.
+step(X) : true <- do(skip).

+!do_direction_step : goingto(X1,Y1) & pos(X1,Y1) <- -goingto(X1, Y1); do(skip).
+!do_direction_step : goingto(X1,Y1) & pos(X2,Y2) & X1 < X2 <- do(left).
+!do_direction_step : goingto(X1,Y1) & pos(X2,Y2) & Y1 < Y2 <- do(up).
+!do_direction_step : goingto(X1,Y1) & pos(X2,Y2) & X1 > X2 <- do(right).
+!do_direction_step : goingto(X1,Y1) & pos(X2,Y2) & Y1 > Y2 <- do(down).
+!do_direction_step : true <- .print("fail"); do(skip).
