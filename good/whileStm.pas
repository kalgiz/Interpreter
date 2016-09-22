program whileStm;
var i : integer;
var b : boolean;
var A : Array[5] of integer;

begin
	i := 1;
	b := true;
	for i := 1 to 5 do
		A[i] := i;
	i := 1;
	while (i < 6) && b do
	begin
		if A[i] > 3 then 
			b := false
		else
			A[i] := 2 * i;
		i := i + 1;
	end;
	for i := 1 to 5 do
		print A[i];
end.
