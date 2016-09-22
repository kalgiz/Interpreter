program passingByReferenceAndValue;
var a : integer;
var b : boolean;
var c : integer;
var A : Array[5] of integer;

function foo (var x : integer; y : boolean;) : integer;
var i : integer;
begin
	for i := 1 to 10 do
		x := x + 1;
	y := false;
	return x;
end;

begin
	b := true;
	A[1] := 1;
	print A[1];
	c := foo(A[1], b);
	print A[1];
end.
