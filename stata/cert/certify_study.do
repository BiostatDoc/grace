version 19
clear all
set linesize 255
local stata_dir : environment GRACEDR_STATA_DIR
assert !missing("`stata_dir'")
adopath ++ "`stata_dir'/ado"
do "`stata_dir'/cert/helpers.do"

_gracedr_study folder, study("`c(tmpdir)'")
local root "`r(folder)'/gracedr cert study"
local other_root "`r(folder)'/gracedr cert other"
local absent_root "`r(folder)'/gracedr cert absent"
remove_folder "`root'"
remove_folder "`other_root'"
local backslash = char(92)
local quote = char(34)

_gracedr_study folder, study("a b/c")
assert `"`r(folder)'"' == "a b/c"
_gracedr_study folder, study("a b`backslash'c`backslash'`backslash'")
assert `"`r(folder)'"' == "a b/c"
local slash = char(47)
_gracedr_study folder, study("a b/c`slash'`slash'`slash'")
assert `"`r(folder)'"' == "a b/c"
_gracedr_study folder, study("C:/")
assert `"`r(folder)'"' == "C:/"
_gracedr_study folder, study("/")
assert `"`r(folder)'"' == "/"
expect_failure, ///
    rc(198) ///
    message("option study() required") ///
    command(`"_gracedr_study folder, study("")"')
expect_failure, ///
    rc(198) ///
    message("gracedr: study() must not contain a double quote") ///
    command(`"_gracedr_study folder, study(`"a`quote'b"')"')
expect_failure, ///
    rc(198) ///
    message("gracedr: unknown study subcommand nonsense") ///
    command(`"_gracedr_study nonsense, study("`root'")"')
expect_failure, ///
    rc(198) ///
    message("gracedr: unknown study subcommand") ///
    command(`"_gracedr_study"')

_gracedr_study exists, study("`root'")
assert r(exists) == 0
quietly mkdir "`root'"
_gracedr_study exists, study("`root'")
assert r(exists) == 0

expect_failure, ///
    rc(601) ///
    message("gracedr: no study found in `absent_root'") ///
    command(`"_gracedr_study read, study("`absent_root'") name(design)"')
expect_failure, ///
    rc(601) ///
    message("gracedr: no study found in `root'") ///
    command(`"_gracedr_study read, study("`root'") name(design)"')
expect_failure, ///
    rc(601) ///
    message("gracedr: no study found in `root'") ///
    command(`"_gracedr_study append_audit, study("`root'") action(create) id(a)"')

make_data 3
expect_failure, ///
    rc(601) ///
    message("does not exist") ///
    command(`"_gracedr_study write, study("`absent_root'") name(design)"')
_gracedr_study write, study("`root'") name(design)
assert !fileexists("`root'/design.bak")
assert !fileexists("`root'/design.tmp")
clear
_gracedr_study read, study("`root'") name(design)
assert _N == 3
assert x[3] == 30
assert label[2] == "row2"
_gracedr_study exists, study("`root'")
assert r(exists) == 1

foreach name in audit participants codes state {
    expect_failure, ///
        rc(601) ///
        message("gracedr: `name'.dta is missing from `root'") ///
        command(`"_gracedr_study read, study("`root'") name(`name')"')
}

local windows_root = subinstr("`root'", "/", "`backslash'", .)
_gracedr_study exists, study("`windows_root'")
assert r(exists) == 1
_gracedr_study exists, study("`root'/")
assert r(exists) == 1
_gracedr_study exists, study("`root'`slash'`slash'")
assert r(exists) == 1
_gracedr_study exists, study("`windows_root'`backslash'")
assert r(exists) == 1
make_data 2
_gracedr_study write, study("`root'/") name(codes)
clear
_gracedr_study read, study("`windows_root'`backslash'") name(codes)
assert _N == 2

make_data 5
_gracedr_study write, study("`root'") name(design)
make_data 3
cf _all using "`root'/design.bak"
_gracedr_study read, study("`root'") name(design)
assert _N == 5
assert x[5] == 50

make_data 9
_gracedr_study write, study("`root'") name(design)
make_data 5
cf _all using "`root'/design.bak"

make_data 6
quietly save "`root'/design.tmp", replace
make_data 2
_gracedr_study write, study("`root'") name(design)
assert _N == 2
assert x[2] == 20
assert "`: type label'" == "str10"
assert !fileexists("`root'/design.tmp")
make_data 9
cf _all using "`root'/design.bak"
make_data 2
cf _all using "`root'/design.dta"

make_data 7
quietly save "`root'/state.tmp", replace
make_data 2
_gracedr_study write, study("`root'") name(state)
assert !fileexists("`root'/state.tmp")
assert !fileexists("`root'/state.bak")
clear
_gracedr_study read, study("`root'") name(state)
assert _N == 2

make_data 3
_gracedr_study write, study("`root'") name(participants)
make_data 4
_gracedr_study write, study("`root'") name(participants)
make_data 6
quietly mkdir "`root'/participants.tmp"
expect_failure, ///
    message("gracedr: could not write participants.dta in `root'") ///
    command(`"_gracedr_study write, study("`root'") name(participants)"')
assert _N == 6
quietly rmdir "`root'/participants.tmp"
make_data 3
cf _all using "`root'/participants.bak"
make_data 4
cf _all using "`root'/participants.dta"

make_data 1
expect_failure, ///
    rc(198) ///
    message("gracedr: unknown study dataset") ///
    command(`"_gracedr_study write, study("`root'") name(../escape)"')
expect_failure, ///
    rc(198) ///
    message("gracedr: unknown study dataset") ///
    command(`"_gracedr_study read, study("`root'") name(escape)"')
assert !fileexists("`root'/../escape.dta")

_gracedr_study empty, name(design)
assert _N == 0
confirm string variable name value
confirm numeric variable order
assert "`: type value'" == "strL"

_gracedr_study empty, name(codes)
assert _N == 0
confirm string variable code arm status id

_gracedr_study empty, name(audit)
assert _N == 0
confirm double variable time
assert "`: format time'" == "%tc"
confirm string variable action id old new old_code new_code reason
foreach variable in id old new old_code new_code reason {
    assert "`: type `variable''" == "strL"
}

expect_failure, ///
    rc(198) ///
    message("gracedr: no empty schema for participants") ///
    command(`"_gracedr_study empty, name(participants)"')

quietly mkdir "`other_root'"
foreach name in design codes audit {
    _gracedr_study empty, name(`name')
    _gracedr_study write, study("`other_root'") name(`name')
    clear
    _gracedr_study read, study("`other_root'") name(`name')
    assert _N == 0
}
confirm double variable time
assert "`: format time'" == "%tc"
assert "`: type reason'" == "strL"

make_data 3
_gracedr_utc
local before = r(time)
_gracedr_study append_audit, ///
    study("`root'") ///
    action(create) ///
    id(setup) ///
    reason(study created)
_gracedr_study append_audit, ///
    study("`root'") ///
    action(randomize) ///
    id(P001) ///
    new(A) ///
    newcode(C1)
_gracedr_study append_audit, ///
    study("`root'") ///
    action(override) ///
    id(P001) ///
    old(A) ///
    new(B) ///
    oldcode(C1) ///
    newcode(C2) ///
    reason(`"he said "no" & 50% (ok)"')
_gracedr_utc
local after = r(time)
assert _N == 3
assert x[3] == 30
assert "`: type label'" == "str10"

_gracedr_study read, study("`root'") name(audit)
assert _N == 3
assert action[1] == "create" & id[1] == "setup" & old[1] == "" & new[1] == ""
assert reason[1] == "study created"
assert action[2] == "randomize" & old[2] == "" & new[2] == "A" & new_code[2] == "C1"
assert action[3] == "override" & old[3] == "A" & new[3] == "B"
assert old_code[3] == "C1" & new_code[3] == "C2"
assert reason[3] == `"he said "no" & 50% (ok)"'
assert !missing(time[1], time[2], time[3])
assert inrange(time[1], `before', `after') & inrange(time[3], `before', `after')
assert time[1] <= time[2] & time[2] <= time[3]
assert !fileexists("`root'/audit.tmp")
make_data 3
quietly use "`root'/audit.bak", clear
assert _N == 2

local long ""
forvalues block = 1/30 {
    local long "`long'0123456789"
}
_gracedr_study append_audit, ///
    study("`root'") ///
    action(randomize) ///
    id(`long') ///
    new(`long') ///
    newcode(`long') ///
    reason(`long')
_gracedr_study read, study("`root'") name(audit)
assert _N == 4
assert id[4] == "`long'" & new[4] == "`long'" & new_code[4] == "`long'"
assert reason[4] == "`long'"

make_data 3
quietly mkdir "`root'/audit.tmp"
expect_failure, ///
    message("gracedr: could not write audit.dta in `root'") ///
    command(`"_gracedr_study append_audit, study("`root'") action(create) id(late)"')
assert _N == 3
assert x[3] == 30
quietly rmdir "`root'/audit.tmp"
_gracedr_study read, study("`root'") name(audit)
assert _N == 4

expect_failure, ///
    rc(198) ///
    message("gracedr: unknown audit action delete") ///
    command(`"_gracedr_study append_audit, study("`root'") action(delete) id(P001)"')
expect_failure, ///
    rc(198) ///
    message("option id() required") ///
    command(`"_gracedr_study append_audit, study("`root'") action(create) id("")"')
expect_failure, ///
    rc(601) ///
    message("gracedr: no study found in `absent_root'") ///
    command(`"_gracedr_study append_audit, study("`absent_root'") action(create) id(a)"')
_gracedr_study read, study("`root'") name(audit)
assert _N == 4

remove_folder "`root'"
remove_folder "`other_root'"

display "certify_study: all checks passed"
