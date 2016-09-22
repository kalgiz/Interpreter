program badInnerFunctions;
var i : integer;

function fun() : integer;
var a : integer;

function innerFun() : integer;

begin
	print 0;
	return 0;
end;

begin
	a := innerFun();
	return 0;
end;

begin 
	i := fun();
	i := innerFun();
end.