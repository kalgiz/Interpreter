program wrongTypeOfFunParamters;
var i : integer;
var b : boolean;

function fun (x : integer; y : boolean;) : integer;
begin
	return x;
end;

begin
	i := 1;
	b := false;
	if b then
		i := fun(b,i)
	else 
		print 1;
end.
