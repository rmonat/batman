var choosing0:bool, choosing1:bool, choosing2:bool, num0:int, num1:int, num2:int, x:int;

initial choosing0 == false and choosing1 == false and choosing2 == false and num0 == 0 and num1 == 0 and num2 == 0 and x == 0;


thread t0:
begin
	while (true) do
		choosing0 = true;
		num0 = 1 + num0 + num1 + num2;
/*		assume(num0 > num1);
		assume(num0 > num2); (sanity check) */
		choosing0 = false;

		assume(not (choosing1));
		assume(not (num1 > 0 and (num1 < num0 or (num1 == num0 and 1 < 0))));
		assume(not (choosing2));
@		assume(not (num2 > 0 and (num2 < num0 or (num2 == num0 and 2 < 0))));
		x = 1;
		x = x + 1;

		num0 = 0;
	done;
end


thread t1:
begin
	while (true) do
		choosing1 = true;
		num1 = 1 + num0 + num1 + num2;
		choosing1 = false;

		assume(not (choosing0));
		assume(not (num0 > 0 and (num0 < num1 or (num0 == num1 and 0 < 1))));
		assume(not (choosing2));
@		assume(not (num2 > 0 and (num2 < num1 or (num2 == num1 and 2 < 1))));
		x = 3;
		x = x + 1;

		num1 = 0;
	done;
end


thread t2:
begin
	while (true) do
		choosing2 = true;
		num2 = 1 + num0 + num1 + num2;
		choosing2 = false;

		assume(not (choosing0));
		assume(not (num0 > 0 and (num0 < num2 or (num0 == num2 and 0 < 2))));
		assume(not (choosing1));
@		assume(not (num1 > 0 and (num1 < num2 or (num1 == num2 and 1 < 2))));
		x = 5;
		x = x + 1;

		num2 = 0;
	done;
end


