var choosing0:bool, choosing1:bool, choosing2:bool, choosing3:bool, choosing4:bool, choosing5:bool, choosing6:bool, choosing7:bool, choosing8:bool, num0:int, num1:int, num2:int, num3:int, num4:int, num5:int, num6:int, num7:int, num8:int, x:int;

initial choosing0 == false and choosing1 == false and choosing2 == false and choosing3 == false and choosing4 == false and choosing5 == false and choosing6 == false and choosing7 == false and choosing8 == false and num0 == 0 and num1 == 0 and num2 == 0 and num3 == 0 and num4 == 0 and num5 == 0 and num6 == 0 and num7 == 0 and num8 == 0 and x == 0;


thread t0:
begin
	while (true) do
		choosing0 = true;
		num0 = 1 + num0 + num1 + num2 + num3 + num4 + num5 + num6 + num7 + num8;
		choosing0 = false;

		assume(not (choosing1));
		assume(not (num1 > 0 and (num1 < num0 or (num1 == num0 and 1 < 0))));
		assume(not (choosing2));
		assume(not (num2 > 0 and (num2 < num0 or (num2 == num0 and 2 < 0))));
		assume(not (choosing3));
		assume(not (num3 > 0 and (num3 < num0 or (num3 == num0 and 3 < 0))));
		assume(not (choosing4));
		assume(not (num4 > 0 and (num4 < num0 or (num4 == num0 and 4 < 0))));
		assume(not (choosing5));
		assume(not (num5 > 0 and (num5 < num0 or (num5 == num0 and 5 < 0))));
		assume(not (choosing6));
		assume(not (num6 > 0 and (num6 < num0 or (num6 == num0 and 6 < 0))));
		assume(not (choosing7));
		assume(not (num7 > 0 and (num7 < num0 or (num7 == num0 and 7 < 0))));
		assume(not (choosing8));
@		assume(not (num8 > 0 and (num8 < num0 or (num8 == num0 and 8 < 0))));
		x = 1;
		x = x + 1;

		num0 = 0;
	done;
end


thread t1:
begin
	while (true) do
		choosing1 = true;
		num1 = 1 + num0 + num1 + num2 + num3 + num4 + num5 + num6 + num7 + num8;
		choosing1 = false;

		assume(not (choosing0));
		assume(not (num0 > 0 and (num0 < num1 or (num0 == num1 and 0 < 1))));
		assume(not (choosing2));
		assume(not (num2 > 0 and (num2 < num1 or (num2 == num1 and 2 < 1))));
		assume(not (choosing3));
		assume(not (num3 > 0 and (num3 < num1 or (num3 == num1 and 3 < 1))));
		assume(not (choosing4));
		assume(not (num4 > 0 and (num4 < num1 or (num4 == num1 and 4 < 1))));
		assume(not (choosing5));
		assume(not (num5 > 0 and (num5 < num1 or (num5 == num1 and 5 < 1))));
		assume(not (choosing6));
		assume(not (num6 > 0 and (num6 < num1 or (num6 == num1 and 6 < 1))));
		assume(not (choosing7));
		assume(not (num7 > 0 and (num7 < num1 or (num7 == num1 and 7 < 1))));
		assume(not (choosing8));
@		assume(not (num8 > 0 and (num8 < num1 or (num8 == num1 and 8 < 1))));
		x = 3;
		x = x + 1;

		num1 = 0;
	done;
end


thread t2:
begin
	while (true) do
		choosing2 = true;
		num2 = 1 + num0 + num1 + num2 + num3 + num4 + num5 + num6 + num7 + num8;
		choosing2 = false;

		assume(not (choosing0));
		assume(not (num0 > 0 and (num0 < num2 or (num0 == num2 and 0 < 2))));
		assume(not (choosing1));
		assume(not (num1 > 0 and (num1 < num2 or (num1 == num2 and 1 < 2))));
		assume(not (choosing3));
		assume(not (num3 > 0 and (num3 < num2 or (num3 == num2 and 3 < 2))));
		assume(not (choosing4));
		assume(not (num4 > 0 and (num4 < num2 or (num4 == num2 and 4 < 2))));
		assume(not (choosing5));
		assume(not (num5 > 0 and (num5 < num2 or (num5 == num2 and 5 < 2))));
		assume(not (choosing6));
		assume(not (num6 > 0 and (num6 < num2 or (num6 == num2 and 6 < 2))));
		assume(not (choosing7));
		assume(not (num7 > 0 and (num7 < num2 or (num7 == num2 and 7 < 2))));
		assume(not (choosing8));
@		assume(not (num8 > 0 and (num8 < num2 or (num8 == num2 and 8 < 2))));
		x = 5;
		x = x + 1;

		num2 = 0;
	done;
end


thread t3:
begin
	while (true) do
		choosing3 = true;
		num3 = 1 + num0 + num1 + num2 + num3 + num4 + num5 + num6 + num7 + num8;
		choosing3 = false;

		assume(not (choosing0));
		assume(not (num0 > 0 and (num0 < num3 or (num0 == num3 and 0 < 3))));
		assume(not (choosing1));
		assume(not (num1 > 0 and (num1 < num3 or (num1 == num3 and 1 < 3))));
		assume(not (choosing2));
		assume(not (num2 > 0 and (num2 < num3 or (num2 == num3 and 2 < 3))));
		assume(not (choosing4));
		assume(not (num4 > 0 and (num4 < num3 or (num4 == num3 and 4 < 3))));
		assume(not (choosing5));
		assume(not (num5 > 0 and (num5 < num3 or (num5 == num3 and 5 < 3))));
		assume(not (choosing6));
		assume(not (num6 > 0 and (num6 < num3 or (num6 == num3 and 6 < 3))));
		assume(not (choosing7));
		assume(not (num7 > 0 and (num7 < num3 or (num7 == num3 and 7 < 3))));
		assume(not (choosing8));
@		assume(not (num8 > 0 and (num8 < num3 or (num8 == num3 and 8 < 3))));
		x = 7;
		x = x + 1;

		num3 = 0;
	done;
end


thread t4:
begin
	while (true) do
		choosing4 = true;
		num4 = 1 + num0 + num1 + num2 + num3 + num4 + num5 + num6 + num7 + num8;
		choosing4 = false;

		assume(not (choosing0));
		assume(not (num0 > 0 and (num0 < num4 or (num0 == num4 and 0 < 4))));
		assume(not (choosing1));
		assume(not (num1 > 0 and (num1 < num4 or (num1 == num4 and 1 < 4))));
		assume(not (choosing2));
		assume(not (num2 > 0 and (num2 < num4 or (num2 == num4 and 2 < 4))));
		assume(not (choosing3));
		assume(not (num3 > 0 and (num3 < num4 or (num3 == num4 and 3 < 4))));
		assume(not (choosing5));
		assume(not (num5 > 0 and (num5 < num4 or (num5 == num4 and 5 < 4))));
		assume(not (choosing6));
		assume(not (num6 > 0 and (num6 < num4 or (num6 == num4 and 6 < 4))));
		assume(not (choosing7));
		assume(not (num7 > 0 and (num7 < num4 or (num7 == num4 and 7 < 4))));
		assume(not (choosing8));
@		assume(not (num8 > 0 and (num8 < num4 or (num8 == num4 and 8 < 4))));
		x = 9;
		x = x + 1;

		num4 = 0;
	done;
end


thread t5:
begin
	while (true) do
		choosing5 = true;
		num5 = 1 + num0 + num1 + num2 + num3 + num4 + num5 + num6 + num7 + num8;
		choosing5 = false;

		assume(not (choosing0));
		assume(not (num0 > 0 and (num0 < num5 or (num0 == num5 and 0 < 5))));
		assume(not (choosing1));
		assume(not (num1 > 0 and (num1 < num5 or (num1 == num5 and 1 < 5))));
		assume(not (choosing2));
		assume(not (num2 > 0 and (num2 < num5 or (num2 == num5 and 2 < 5))));
		assume(not (choosing3));
		assume(not (num3 > 0 and (num3 < num5 or (num3 == num5 and 3 < 5))));
		assume(not (choosing4));
		assume(not (num4 > 0 and (num4 < num5 or (num4 == num5 and 4 < 5))));
		assume(not (choosing6));
		assume(not (num6 > 0 and (num6 < num5 or (num6 == num5 and 6 < 5))));
		assume(not (choosing7));
		assume(not (num7 > 0 and (num7 < num5 or (num7 == num5 and 7 < 5))));
		assume(not (choosing8));
@		assume(not (num8 > 0 and (num8 < num5 or (num8 == num5 and 8 < 5))));
		x = 11;
		x = x + 1;

		num5 = 0;
	done;
end


thread t6:
begin
	while (true) do
		choosing6 = true;
		num6 = 1 + num0 + num1 + num2 + num3 + num4 + num5 + num6 + num7 + num8;
		choosing6 = false;

		assume(not (choosing0));
		assume(not (num0 > 0 and (num0 < num6 or (num0 == num6 and 0 < 6))));
		assume(not (choosing1));
		assume(not (num1 > 0 and (num1 < num6 or (num1 == num6 and 1 < 6))));
		assume(not (choosing2));
		assume(not (num2 > 0 and (num2 < num6 or (num2 == num6 and 2 < 6))));
		assume(not (choosing3));
		assume(not (num3 > 0 and (num3 < num6 or (num3 == num6 and 3 < 6))));
		assume(not (choosing4));
		assume(not (num4 > 0 and (num4 < num6 or (num4 == num6 and 4 < 6))));
		assume(not (choosing5));
		assume(not (num5 > 0 and (num5 < num6 or (num5 == num6 and 5 < 6))));
		assume(not (choosing7));
		assume(not (num7 > 0 and (num7 < num6 or (num7 == num6 and 7 < 6))));
		assume(not (choosing8));
@		assume(not (num8 > 0 and (num8 < num6 or (num8 == num6 and 8 < 6))));
		x = 13;
		x = x + 1;

		num6 = 0;
	done;
end


thread t7:
begin
	while (true) do
		choosing7 = true;
		num7 = 1 + num0 + num1 + num2 + num3 + num4 + num5 + num6 + num7 + num8;
		choosing7 = false;

		assume(not (choosing0));
		assume(not (num0 > 0 and (num0 < num7 or (num0 == num7 and 0 < 7))));
		assume(not (choosing1));
		assume(not (num1 > 0 and (num1 < num7 or (num1 == num7 and 1 < 7))));
		assume(not (choosing2));
		assume(not (num2 > 0 and (num2 < num7 or (num2 == num7 and 2 < 7))));
		assume(not (choosing3));
		assume(not (num3 > 0 and (num3 < num7 or (num3 == num7 and 3 < 7))));
		assume(not (choosing4));
		assume(not (num4 > 0 and (num4 < num7 or (num4 == num7 and 4 < 7))));
		assume(not (choosing5));
		assume(not (num5 > 0 and (num5 < num7 or (num5 == num7 and 5 < 7))));
		assume(not (choosing6));
		assume(not (num6 > 0 and (num6 < num7 or (num6 == num7 and 6 < 7))));
		assume(not (choosing8));
@		assume(not (num8 > 0 and (num8 < num7 or (num8 == num7 and 8 < 7))));
		x = 15;
		x = x + 1;

		num7 = 0;
	done;
end


thread t8:
begin
	while (true) do
		choosing8 = true;
		num8 = 1 + num0 + num1 + num2 + num3 + num4 + num5 + num6 + num7 + num8;
		choosing8 = false;

		assume(not (choosing0));
		assume(not (num0 > 0 and (num0 < num8 or (num0 == num8 and 0 < 8))));
		assume(not (choosing1));
		assume(not (num1 > 0 and (num1 < num8 or (num1 == num8 and 1 < 8))));
		assume(not (choosing2));
		assume(not (num2 > 0 and (num2 < num8 or (num2 == num8 and 2 < 8))));
		assume(not (choosing3));
		assume(not (num3 > 0 and (num3 < num8 or (num3 == num8 and 3 < 8))));
		assume(not (choosing4));
		assume(not (num4 > 0 and (num4 < num8 or (num4 == num8 and 4 < 8))));
		assume(not (choosing5));
		assume(not (num5 > 0 and (num5 < num8 or (num5 == num8 and 5 < 8))));
		assume(not (choosing6));
		assume(not (num6 > 0 and (num6 < num8 or (num6 == num8 and 6 < 8))));
		assume(not (choosing7));
@		assume(not (num7 > 0 and (num7 < num8 or (num7 == num8 and 7 < 8))));
		x = 17;
		x = x + 1;

		num8 = 0;
	done;
end


