doctest : doctest-safe doctest-unsafe

doctest-safe :
	doctest some/src some/src-safe

doctest-unsafe :
	doctest some/src some/src-unsafe
