run:
	./rebar compile
	erl -pa ebin deps/*/ebin -noshell -s cb_wishlist

test:
	./rebar eunit skip_deps=true

shell:
	./rebar compile
	erl -pa ebin deps/*/ebin -s cb_wishlist


