program innerFunctions;
var i : integer;

function fun(i : integer;) : integer;
var in1 : integer;
var a : integer;

function innerFun1() : integer;
var a : integer;

function innerFun2() : integer;
begin
	in1 := in1 + 1;
	return 0;
end;

begin
	in1 := in1 + 1;
	print in1;
	a := innerFun2();
	print in1;
	return 0;
end;

begin
	in1 := i;
	print in1;
	a := innerFun1();
	print in1;
	return 0;
end;

begin
	i := 0;
	i := fun(i);
end.
