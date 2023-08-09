file_appender
=====

An OTP application

Build
-----

    $ rebar3 compile


Usage
-----
    $ rebar3 shell
    
    >file_appender:append("filename_a.txt", "text a line 1").
    ok
    >file_appender:append("filename_b.txt", "text b line 1").
    ok
    >file_appender:append("filename_a.txt", "text a line 2").
    ok

EUnit
-----
    $ rebar3 eunit
    ===> Verifying dependencies...
    ===> Analyzing applications...
    ===> Compiling file_appender
    ===> Performing EUnit tests...
    ..
    Finished in 12.201 seconds
    2 tests, 0 failures
