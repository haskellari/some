doctest : doctest-safe doctest-unsafe

doctest-safe :
	doctest src src-safe

doctest-unsafe :
	doctest src src-unsafe
