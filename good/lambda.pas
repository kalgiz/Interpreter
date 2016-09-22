program lambdaFun;
var a : integer;
var c : integer;
var d : integer;
var f : lambda (integer, boolean) integer;

function fun(var f : lambda (integer, boolean) integer; i : integer;) : integer;
begin
	return f(i, true);
end;

begin
	a := 1;
	c := 10;
	f := lambda (a : integer; b : boolean;) -> integer; var d : integer; begin d := 10; c := c + d; return a; end;
	d := f(a, true);
	print c;
	print d;
	
	d := fun (f, 50);
	print c;
	print d;
	
	d := fun (lambda (a : integer; b : boolean;) -> integer; var d : integer; begin d := 10; c := c + d; return a; end, 100);
	print c;
	print d;
end.
