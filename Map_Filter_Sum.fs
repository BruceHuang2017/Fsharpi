let rec sum = function
|(m, 0) -> m
|(m, n) -> sum (m, n-1) + m + n;;

let rec Map f l = 
	match l with
	| [] -> []
	| a::x -> (f a)::(Map f x);;

let rec Filter f l =
	match l with
	| [] -> []
	| a::x -> 
		match (f a) with
		| true -> a::(Filter f x)
		| false -> (Filter f x);;
