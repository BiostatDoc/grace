version 19
clear all
set linesize 255
local stata_dir : environment GRACEDR_STATA_DIR
assert !missing("`stata_dir'")
adopath ++ "`stata_dir'/ado"
do "`stata_dir'/cert/helpers.do"

_gracedr_study folder, study("`c(tmpdir)'")
local base "`r(folder)'"
local first "`base'/gracedr cert rng first"
local second "`base'/gracedr cert rng second"
local third "`base'/gracedr cert rng third"
local fourth "`base'/gracedr cert rng fourth"
local absent "`base'/gracedr cert rng absent"
foreach folder in first second third fourth absent {
    remove_folder "``folder''"
}
make_data 1
foreach folder in first second third fourth {
    quietly mkdir "``folder''"
    _gracedr_study write, study("``folder''") name(design)
}

set seed 424242
local user_state = c(rngstate)
local user_rng = c(rng)
local draws = 12

_gracedr_rng init, study("`first'") seed(20261114)
assert c(rngstate) == "`user_state'"
assert c(rng) == "`user_rng'"
_gracedr_study read, study("`first'") name(state)
assert _N == 1
assert "`: type state'" == "strL"
assert inrange(strlen(state[1]), 1000, 100000)
assert seed[1] == 20261114
assert generator[1] == "mt64"
assert format_version[1] == 1
local initial_state = state[1]

expect_failure, ///
    rc(602) ///
    message("gracedr: state.dta already exists in `first'") ///
    command(`"_gracedr_rng init, study("`first'") seed(1)"')
expect_failure, ///
    rc(601) ///
    message("gracedr: no study found in `absent'") ///
    command(`"_gracedr_rng init, study("`absent'") seed(1)"')
foreach bad_seed in -1 2147483648 1.5 99999999999999999999 1e3 abc {
    expect_failure, ///
        rc(198) ///
        message("gracedr: seed() must be a whole number from 0 to 2147483647") ///
        command(`"_gracedr_rng init, study("`third'") seed(`bad_seed')"')
}
assert !fileexists("`third'/state.dta")
foreach good_seed in 0 2147483647 007 {
    _gracedr_rng init, study("`third'") seed(`good_seed')
    quietly erase "`third'/state.dta"
}

_gracedr_rng init, study("`second'") seed(20261114)
_gracedr_rng init, study("`third'") seed(7)
assert c(rngstate) == "`user_state'"

clear
quietly set obs `draws'
foreach which in first second third {
    generate str16 arm_`which' = ""
    generate strL before_`which' = ""
}
foreach which in first second third {
    forvalues position = 1/`draws' {
        _gracedr_rng draw, study("``which''") probs(0.5 0.3 0.2) arms(A B C)
        quietly replace arm_`which' = "`r(arm)'" in `position'
        local before `"`r(state_before)'"'
        mata: st_sstore(`position', "before_`which'", st_local("before"))
    }
}
assert _N == `draws'
assert c(rngstate) == "`user_state'"
assert c(rng) == "`user_rng'"
assert arm_first == arm_second
assert before_first == before_second
assert inlist(arm_first, "A", "B", "C")
assert before_first[1] == "`initial_state'"
local reference_arms "AABCABACBCBB"
local observed_arms ""
forvalues position = 1/`draws' {
    local observed_arms "`observed_arms'`=arm_first[`position']'"
}
assert "`observed_arms'" == "`reference_arms'"
local same_start = (before_first[1] == before_third[1])
assert `same_start' == 0
local differing = 0
forvalues position = 1/`draws' {
    local match = (arm_first[`position'] == arm_third[`position'])
    local differing = `differing' + 1 - `match'
}
assert inrange(`differing', 1, `draws')

forvalues position = 1/`draws' {
    local before `"`=before_first[`position']'"'
    _gracedr_rng replay, state(`"`before'"') probs(0.5 0.3 0.2) arms(A B C)
    assert "`r(arm)'" == arm_first[`position']
}
assert c(rngstate) == "`user_state'"

forvalues position = 2/`draws' {
    local previous = `position' - 1
    local repeated = (before_first[`position'] == before_first[`previous'])
    assert `repeated' == 0
}

local last_before `"`=before_first[`draws']'"'
preserve
_gracedr_study read, study("`first'") name(state)
assert _N == 1
assert "`: type state'" == "strL"
local advanced = (state[1] == "`last_before'")
assert `advanced' == 0
assert seed[1] == 20261114
restore

forvalues position = 1/3 {
    _gracedr_rng draw, study("`second'") probs(0 1 0) arms(A B C)
    assert "`r(arm)'" == "B"
    _gracedr_rng draw, study("`second'") probs(0 0 1) arms(A B C)
    assert "`r(arm)'" == "C"
    _gracedr_rng draw, study("`second'") probs(2 0 0) arms(A B C)
    assert "`r(arm)'" == "A"
}

_gracedr_rng draw, study("`second'") probs(1 1) arms(`""Arm one" "Arm two""')
assert inlist("`r(arm)'", "Arm one", "Arm two")

local first_arm_count = 0
local sample = 150
forvalues position = 1/`sample' {
    _gracedr_rng draw, study("`third'") probs(0.7 0.3) arms(A B)
    if "`r(arm)'" == "A" {
        local first_arm_count = `first_arm_count' + 1
    }
}
local proportion = `first_arm_count' / `sample'
assert abs(`proportion' - 0.7) < 0.12

set rng kiss32
local kiss_state = c(rngstate)
_gracedr_rng draw, study("`first'") probs(0.5 0.5) arms(A B)
assert c(rng) == "kiss32"
assert c(rngstate) == "`kiss_state'"
_gracedr_rng replay, state(`"`initial_state'"') probs(0.5 0.3 0.2) arms(A B C)
assert c(rng) == "kiss32"
assert c(rngstate) == "`kiss_state'"
preserve
_gracedr_study read, study("`first'") name(state)
assert generator[1] == "mt64"
restore
set rng default
set seed 424242
local user_state = c(rngstate)

preserve
_gracedr_study read, study("`first'") name(state)
local stored_state = state[1]
restore
expect_failure, ///
    message("gracedr: probs() and arms() must have the same number of items") ///
    command(`"_gracedr_rng draw, study("`first'") probs(0.5 0.5) arms(A B C)"')
expect_failure, ///
    message("gracedr: probs() must be numbers that are not negative") ///
    command(`"_gracedr_rng draw, study("`first'") probs(0.5 -0.1) arms(A B)"')
expect_failure, ///
    message("gracedr: probs() must be numbers that are not negative") ///
    command(`"_gracedr_rng draw, study("`first'") probs(0.5 abc) arms(A B)"')
expect_failure, ///
    message("gracedr: probs() must add up to more than zero") ///
    command(`"_gracedr_rng draw, study("`first'") probs(0 0) arms(A B)"')
expect_failure, ///
    message("gracedr: no study found in `absent'") ///
    command(`"_gracedr_rng draw, study("`absent'") probs(1 1) arms(A B)"')
expect_failure, ///
    rc(601) ///
    message("gracedr: state.dta is missing from `fourth'") ///
    command(`"_gracedr_rng draw, study("`fourth'") probs(1 1) arms(A B)"')
expect_failure, ///
    message("gracedr: state is not a valid random number state") ///
    command(`"_gracedr_rng replay, state(bogus) probs(1 1) arms(A B)"')
assert c(rngstate) == "`user_state'"
assert c(rng) == "default"
preserve
_gracedr_study read, study("`first'") name(state)
assert state[1] == "`stored_state'"
restore

foreach spec in " 0.5   0.3  0.2 " "5e-1 3e-1 2e-1" {
    _gracedr_rng draw, study("`second'") probs(`"`spec'"') arms(A B C)
    assert inlist("`r(arm)'", "A", "B", "C")
}
_gracedr_rng draw, study("`second'") probs(1 0) arms(A B)
assert "`r(arm)'" == "A"
local minus = char(45)
_gracedr_rng draw, study("`second'") probs(1e`minus'300 1) arms(A B)
assert "`r(arm)'" == "B"
_gracedr_rng draw, study("`second'") probs(1) arms(A)
assert "`r(arm)'" == "A"
expect_failure, ///
    rc(198) ///
    message("gracedr: probs() must add up to more than zero") ///
    command(`"_gracedr_rng draw, study("`second'") probs(0) arms(A)"')
expect_failure, ///
    rc(198) ///
    message("gracedr: probs() must be numbers that are not negative") ///
    command(`"_gracedr_rng draw, study("`second'") probs(. 1) arms(A B)"')
expect_failure, ///
    rc(198) ///
    message("gracedr: probs() and arms() must have the same number of items") ///
    command(`"_gracedr_rng draw, study("`second'") probs(0.5,0.5) arms(A B)"')
expect_failure, ///
    rc(198) ///
    message("gracedr: unknown rng subcommand nonsense") ///
    command(`"_gracedr_rng nonsense, study("`second'")"')
expect_failure, ///
    rc(198) ///
    message("gracedr: unknown rng subcommand") ///
    command(`"_gracedr_rng"')

preserve
_gracedr_rng draw, study("`second'") probs(1 1) arms(A B)
restore
assert _N == `draws'

expect_failure, ///
    rc(198) ///
    message("gracedr: state is not a valid random number state") ///
    command(`"_gracedr_rng replay, state(`initial_state') probs(1 1) arms(A B) generator(bogus)"')
assert c(rng) == "default"
assert c(rngstate) == "`user_state'"

set rng kiss32
set seed 3
local kiss_replay_state = c(rngstate)
local kiss_uniform = runiform()
local kiss_expected = cond(`kiss_uniform' < 0.5, "A", "B")
set rng default
set seed 424242
local user_state = c(rngstate)
_gracedr_rng replay, ///
    state(`kiss_replay_state') ///
    probs(0.5 0.5) ///
    arms(A B) ///
    generator(kiss32)
assert "`r(arm)'" == "`kiss_expected'"
assert c(rng) == "default"
assert c(rngstate) == "`user_state'"

set rng kiss32
set seed 8
local kiss_user_state = c(rngstate)
expect_failure, ///
    rc(198) ///
    message("gracedr: state is not a valid random number state") ///
    command(`"_gracedr_rng replay, state(bogus) probs(1 1) arms(A B)"')
assert c(rng) == "kiss32"
assert c(rngstate) == "`kiss_user_state'"

set rng mt64s
set seed 11
set rngstream 3
local stream_state = c(rngstate)
_gracedr_rng draw, study("`first'") probs(0.5 0.5) arms(A B)
assert c(rng) == "mt64s"
assert c(rngstream) == 3
assert c(rngstate) == "`stream_state'"
_gracedr_rng replay, state(`initial_state') probs(0.5 0.5) arms(A B)
assert c(rng) == "mt64s"
assert c(rngstream) == 3
assert c(rngstate) == "`stream_state'"
set rng default
set seed 424242
local user_state = c(rngstate)

local fifth "`base'/gracedr cert rng fifth"
remove_folder "`fifth'"
quietly mkdir "`fifth'"
preserve
make_data 1
_gracedr_study write, study("`fifth'") name(design)
_gracedr_study empty, name(state)
quietly set obs 1
mata: st_sstore(1, "state", "bogus!")
quietly replace seed = 1
quietly replace generator = "mt64"
quietly replace format_version = 1
_gracedr_study write, study("`fifth'") name(state)
restore
expect_failure, ///
    rc(198) ///
    message("gracedr: state is not a valid random number state") ///
    command(`"_gracedr_rng draw, study("`fifth'") probs(1 1) arms(A B)"')
assert !fileexists("`fifth'/state.bak")
assert !fileexists("`fifth'/state.tmp")
assert c(rngstate) == "`user_state'"
preserve
_gracedr_study read, study("`fifth'") name(state)
assert state[1] == "bogus!"
quietly set obs 2
_gracedr_study write, study("`fifth'") name(state)
restore
expect_failure, ///
    rc(198) ///
    message("gracedr: state.dta must have exactly one row in `fifth'") ///
    command(`"_gracedr_rng draw, study("`fifth'") probs(1 1) arms(A B)"')
preserve
_gracedr_study empty, name(state)
_gracedr_study write, study("`fifth'") name(state)
restore
expect_failure, ///
    rc(198) ///
    message("gracedr: state.dta must have exactly one row in `fifth'") ///
    command(`"_gracedr_rng draw, study("`fifth'") probs(1 1) arms(A B)"')
remove_folder "`fifth'"

foreach folder in first second third fourth absent {
    remove_folder "``folder''"
}

display "certify_rng: all checks passed"
