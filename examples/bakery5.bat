var choosing0:bool, choosing1:bool, choosing2:bool, choosing3:bool, choosing4:bool, num0:int, num1:int, num2:int, num3:int, num4:int, x:int;

initial choosing0 == false and choosing1 == false and choosing2 == false and choosing3 == false and choosing4 == false and num0 == 0 and num1 == 0 and num2 == 0 and num3 == 0 and num4 == 0 and x == 0;


thread t0:
begin
	while (true) do
		choosing0 = true;
		num0 = 1 + num0 + num1 + num2 + num3 + num4;
		choosing0 = false;

		assume(not (choosing1));
		assume(not (num1 > 0 and (num1 < num0 or (num1 == num0 and 1 < 0))));
		assume(not (choosing2));
		assume(not (num2 > 0 and (num2 < num0 or (num2 == num0 and 2 < 0))));
		assume(not (choosing3));
		assume(not (num3 > 0 and (num3 < num0 or (num3 == num0 and 3 < 0))));
		assume(not (choosing4));
@		assume(not (num4 > 0 and (num4 < num0 or (num4 == num0 and 4 < 0))));
		x = 1;
		x = x + 1;

		num0 = 0;
	done;end


thread t1:
begin
	while (true) do
		choosing1 = true;
		num1 = 1 + num0 + num1 + num2 + num3 + num4;
		choosing1 = false;

		assume(not (choosing0));
		assume(not (num0 > 0 and (num0 < num1 or (num0 == num1 and 0 < 1))));
		assume(not (choosing2));
		assume(not (num2 > 0 and (num2 < num1 or (num2 == num1 and 2 < 1))));
		assume(not (choosing3));
		assume(not (num3 > 0 and (num3 < num1 or (num3 == num1 and 3 < 1))));
		assume(not (choosing4));
@		assume(not (num4 > 0 and (num4 < num1 or (num4 == num1 and 4 < 1))));
		x = 3;
		x = x + 1;

		num1 = 0;
	done;end


thread t2:
begin
	while (true) do
		choosing2 = true;
		num2 = 1 + num0 + num1 + num2 + num3 + num4;
		choosing2 = false;

		assume(not (choosing0));
		assume(not (num0 > 0 and (num0 < num2 or (num0 == num2 and 0 < 2))));
		assume(not (choosing1));
		assume(not (num1 > 0 and (num1 < num2 or (num1 == num2 and 1 < 2))));
		assume(not (choosing3));
		assume(not (num3 > 0 and (num3 < num2 or (num3 == num2 and 3 < 2))));
		assume(not (choosing4));
@		assume(not (num4 > 0 and (num4 < num2 or (num4 == num2 and 4 < 2))));
		x = 5;
		x = x + 1;

		num2 = 0;
	done;end


thread t3:
begin
	while (true) do
		choosing3 = true;
		num3 = 1 + num0 + num1 + num2 + num3 + num4;
		choosing3 = false;

		assume(not (choosing0));
		assume(not (num0 > 0 and (num0 < num3 or (num0 == num3 and 0 < 3))));
		assume(not (choosing1));
		assume(not (num1 > 0 and (num1 < num3 or (num1 == num3 and 1 < 3))));
		assume(not (choosing2));
		assume(not (num2 > 0 and (num2 < num3 or (num2 == num3 and 2 < 3))));
		assume(not (choosing4));
@		assume(not (num4 > 0 and (num4 < num3 or (num4 == num3 and 4 < 3))));
		x = 7;
		x = x + 1;

		num3 = 0;
	done;end


thread t4:
begin
	while (true) do
		choosing4 = true;
		num4 = 1 + num0 + num1 + num2 + num3 + num4;
		choosing4 = false;

		assume(not (choosing0));
		assume(not (num0 > 0 and (num0 < num4 or (num0 == num4 and 0 < 4))));
		assume(not (choosing1));
		assume(not (num1 > 0 and (num1 < num4 or (num1 == num4 and 1 < 4))));
		assume(not (choosing2));
		assume(not (num2 > 0 and (num2 < num4 or (num2 == num4 and 2 < 4))));
		assume(not (choosing3));
@		assume(not (num3 > 0 and (num3 < num4 or (num3 == num4 and 3 < 4))));
		x = 9;
		x = x + 1;

		num4 = 0;
	done;end


