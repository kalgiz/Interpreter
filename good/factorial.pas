program factorial; 
var i : integer; 
var b : boolean; 
var A : Array[5] of integer; 
var Afactorial : Array[5] of integer;

function foo (x : integer;) : boolean;
begin
	print x;
	return true;
end;

function factorial (a : integer;) : integer;
var b : boolean;
begin	
	b := foo(a);
	if a > 1 then
		return factorial(a-1) * a
	else
		return a; 
end;

begin 
	for i := 1 to 5 do begin 
		A[i] := i; 
		Afactorial[i] := factorial(i);
		print (A[i]);
		print(Afactorial[i]); 
	end;
	b := foo(A[1]);
end. 
