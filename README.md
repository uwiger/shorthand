shorthand
=========

A collection of functions aimed at making file:script() files and escripts
more readable

The idea is to provide a set of useful functions with more concise names than
their original OTP counterparts, but also - and more importantly - to
consistently use the semantics that the functions either return the value
you care about, or fail.

The let-it-fail semantics makes it easier to write concise expressions free
of unpacking constructs (which are particularly annoying in file:script()
files), but also tends to give better error information.

To be really effective, these modules should be part of OTP. As a temporary
measure, I've put the following in my `.erlang` file:

```erlang
case erlang:system_info(compat_rel) of
    15 ->
	code:add_patha("/Users/uwiger/erl_patch/r15");
    _ ->
	ok
end.
```

...then copy .beam files compiled with the right OTP into the respective
directories (currently only one).
