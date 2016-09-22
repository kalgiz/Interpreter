program recordType;

var recExample : record
begin
	first : integer;
	second : boolean;
end;
var i : integer;

function fun(i : integer;) : integer;
begin
	i := i + 7;
	print i;
	return 0;
end;

begin
	i:= 1;
	recExample.first := i;
	recExample.second := true;
	print recExample.first;
	print recExample.second;
	i := fun(recExample.first);
	print recExample.first;
end.
