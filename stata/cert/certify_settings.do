version 19
clear all
set linesize 255
local stata_dir : environment GRACEDR_STATA_DIR
assert !missing("`stata_dir'")
adopath ++ "`stata_dir'/ado"
do "`stata_dir'/cert/helpers.do"

_gracedr_study folder, study("`c(tmpdir)'")
local base "`r(folder)'"
local scratch "`base'/gracedr cert settings"
remove_folder "`scratch'"
quietly mkdir "`scratch'"
local one "`scratch'/one"
local two "`scratch'/two"
local three "`scratch'/three"
local four "`scratch'/four"

gracedr setup, ///
    study("`one'") ///
    n(60) ///
    arms(A B) ///
    categorical(sex: male female | race: white black other) ///
    continuous(age weight) ///
    seed(1)
gracedr setup, ///
    study("`two'") ///
    n(60) ///
    arms(A B) ///
    categorical(eth: "not hispanic" hispanic) ///
    seed(2)

_gracedr_study setting, study("`one'") name(n)
assert "`r(value)'" == "60"
_gracedr_study setting, study("`one'") name(varnames)
assert "`r(value)'" == "sex race age weight"
_gracedr_study setting, study("`one'") name(levels1)
assert `"`r(value)'"' == `""male" "female""'
_gracedr_study setting, study("`one'") name(forcedat)
assert `"`r(value)'"' == ""
_gracedr_study setting, study("`two'") name(levels1)
assert `"`r(value)'"' == `""not hispanic" "hispanic""'
expect_failure, ///
    rc(111) ///
    message("gracedr: design.dta has no setting named nothing") ///
    command(`"_gracedr_study setting, study("`one'") name(nothing)"')
expect_failure, ///
    rc(601) ///
    message("gracedr: no study found in `scratch'/none") ///
    command(`"_gracedr_study setting, study("`scratch'/none") name(n)"')

local long_levels ""
forvalues position = 1/400 {
    local long_levels "`long_levels' level`=string(`position' + 1000)'"
}
gracedr setup, ///
    study("`three'") ///
    n(60) ///
    arms(A B) ///
    categorical(big: `long_levels') ///
    seed(3)
_gracedr_study setting, study("`three'") name(levels1)
assert inrange(strlen(`"`r(value)'"'), 4000, 100000)
assert wordcount(`"`r(value)'"') == 400

make_data 3
_gracedr_study folder, study("`one'")
assert r(found) == 1
_gracedr_study folder, study("`scratch'/none")
assert r(found) == 0
assert `"`r(folder)'"' == "`scratch'/none"

expect_failure, ///
    rc(198) ///
    message("gracedr: empty participants needs study()") ///
    command(`"_gracedr_study empty, name(participants)"')
_gracedr_study empty, name(participants) study("`one'")
assert _N == 0
quietly ds
assert "`r(varlist)'" == "seq id sex race age weight arm code prob1 prob2 phase rngstate time"
assert "`: type age'" == "double"
assert "`: type rngstate'" == "strL"
assert "`: format time'" == "%tc"

quietly mkdir "`four'"
make_data 2
_gracedr_study write, study("`four'") name(design)
expect_failure, ///
    rc(459) ///
    message("gracedr: design.dta is damaged: it needs the columns name and value") ///
    command(`"_gracedr_study setting, study("`four'") name(n)"')

preserve
_gracedr_study read, study("`one'") name(design)
quietly expand 2 if name == "n"
_gracedr_study write, study("`four'") name(design)
restore
expect_failure, ///
    rc(459) ///
    message("gracedr: design.dta has more than one setting named n") ///
    command(`"_gracedr_study setting, study("`four'") name(n)"')
expect_failure, ///
    rc(198) ///
    message("gracedr: setting names must be valid Stata names") ///
    command(`"_gracedr_study setting, study("`one'") name(a b)"')

make_data 3
set seed 5
local user_state = c(rngstate)
expect_failure, ///
    rc(198) ///
    message("gracedr: empty participants needs study()") ///
    command(`"_gracedr_study empty, name(participants)"')
assert _N == 3
assert x[3] == 30
expect_failure, ///
    rc(198) ///
    message("gracedr: no empty schema for nothing") ///
    command(`"_gracedr_study empty, name(nothing)"')
assert c(rngstate) == "`user_state'"

remove_folder "`scratch'"

display "certify_settings: all checks passed"
