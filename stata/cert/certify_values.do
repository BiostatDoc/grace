version 19
clear all
set linesize 255
local stata_dir : environment GRACEDR_STATA_DIR
assert !missing("`stata_dir'")
adopath ++ "`stata_dir'/ado"
do "`stata_dir'/cert/helpers.do"

_gracedr_study folder, study("`c(tmpdir)'")
local base "`r(folder)'"
local scratch "`base'/gracedr cert values"
remove_folder "`scratch'"
quietly mkdir "`scratch'"
local one "`scratch'/one"
local two "`scratch'/two"
local three "`scratch'/three"

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

local call `"_gracedr_values parse, study("`one'") id(P041)"'
local all_values "sex(male) race(white) age(45) weight(70.5)"

make_data 3
set seed 99
local user_state = c(rngstate)
`call' options("`all_values'")
assert r(id) == "P041"
assert r(nvars) == 4
assert "`r(varnames)'" == "sex race age weight"
assert "`r(vartypes)'" == "categorical categorical continuous continuous"
assert "`r(value1)'" == "male"
assert "`r(value2)'" == "white"
assert "`r(value3)'" == "45"
assert r(number3) == 45
assert "`r(value4)'" == "70.5"
assert r(number4) == 70.5
assert "`r(missingvars)'" == ""
assert _N == 3
assert x[3] == 30
assert c(rngstate) == "`user_state'"

`call' options("weight(70.5) age(45) race(white) sex(male)")
assert "`r(value1)'" == "male" & "`r(value2)'" == "white"
assert r(number3) == 45 & r(number4) == 70.5

`call' options(`"sex( MALE ) race("White") age( -3.5 ) weight(1e2)"')
assert "`r(value1)'" == "male"
assert "`r(value2)'" == "white"
assert r(number3) == -3.5
assert r(number4) == 100

foreach spaced_id in "P 007" "007" "  P041 x" "a,b" {
    _gracedr_values parse, study("`one'") id(`"`spaced_id'"') options("`all_values'")
    assert `"`r(id)'"' == `"`spaced_id'"'
}
local long_id ""
forvalues block = 1/49 {
    local long_id "`long_id'abcde"
}
expect_failure, ///
    rc(198) ///
    message("gracedr: id() must be at most 244 bytes") ///
    command(`"_gracedr_values parse, study("`one'") id(`long_id') options("`all_values'")"')
expect_failure, ///
    rc(198) ///
    message("gracedr: (study) is reserved and cannot be a participant id") ///
    command(`"_gracedr_values parse, study("`one'") id((study)) options("`all_values'")"')
expect_failure, ///
    message("option id() required") ///
    command(`"_gracedr_values parse, study("`one'") id() options("`all_values'")"')

expect_failure, ///
    rc(198) ///
    message("gracedr: missing value(s) for: race age weight") ///
    command(`"`call' options("sex(male)")"')
expect_failure, ///
    rc(198) ///
    message("gracedr: missing value(s) for: sex race age weight") ///
    command(`"`call' options("")"')
expect_failure, ///
    rc(198) ///
    message("gracedr: unexpected or repeated value name(s): sex") ///
    command(`"`call' options("`all_values' sex(female)")"')
expect_failure, ///
    rc(198) ///
    message("gracedr: unexpected or repeated value name(s): color") ///
    command(`"`call' options("`all_values' color(red)")"')
expect_failure, ///
    rc(198) ///
    message("gracedr: unexpected or repeated value name(s): id") ///
    command(`"`call' options("`all_values' id(P2)")"')
expect_failure, ///
    rc(198) ///
    message(`"gracedr: "other" is not a level of sex. Levels are "male" "female"."') ///
    command(`"`call' options("sex(other) race(white) age(1) weight(2)")"')
foreach bad_number in abc 1e500 inf 1,5 {
    expect_failure, ///
        rc(198) ///
        message("gracedr: age must be a finite number") ///
        command(`"`call' options("sex(male) race(white) age(`bad_number') weight(2)")"')
}
foreach malformed in "sex male" "sex(male" "sex(male))" "(male)" {
    expect_failure, ///
        rc(198) ///
        message("gracedr: could not read the options: `malformed'") ///
        command(`"`call' options("`malformed' race(white) age(1) weight(2)")"')
}

`call' options("sex(.) race() age(.) weight(2)")
assert "`r(value1)'" == ""
assert "`r(value2)'" == ""
assert "`r(value3)'" == "."
assert missing(r(number3))
assert r(number4) == 2
assert "`r(missingvars)'" == "sex race age"

_gracedr_values parse, study("`two'") id(1) options(`"eth( NOT HISPANIC )"')
assert "`r(value1)'" == "not hispanic"
expect_failure, ///
    rc(198) ///
    message(`"gracedr: "not  hispanic" is not a level of eth."') ///
    command(`"_gracedr_values parse, study("`two'") id(1) options("eth(Not  Hispanic)")"')

expect_failure, ///
    rc(601) ///
    message("gracedr: no study found in `scratch'/none") ///
    command(`"_gracedr_values parse, study("`scratch'/none") id(1) options("sex(male)")"')

local upper_e = uchar(201)
local lower_e = uchar(233)
gracedr setup, study("`scratch'/five") n(60) arms(A B) continuous(age) seed(5)
gracedr setup, ///
    study("`scratch'/six") ///
    n(60) ///
    arms(A B) ///
    categorical(kind: "a,b" "x(y)" "`upper_e'mile" plain) ///
    seed(6)
local five "`scratch'/five"
local six "`scratch'/six"
local minus = char(45)

_gracedr_values parse, study("`five'") id(1) options("age(7)")
assert r(nvars) == 1
assert "`r(vartypes)'" == "continuous"
assert r(number1) == 7
_gracedr_values parse, study("`six'") id(1) options("kind(plain)")
assert "`r(value1)'" == "plain"
foreach value in "a,b" "x(y)" "PLAIN" "Plain " {
    _gracedr_values parse, study("`six'") id(1) options(`"kind(`value')"')
    assert "`r(value1)'" == strlower(strtrim("`value'"))
}
_gracedr_values parse, study("`six'") id(1) options("kind(`upper_e'MILE)")
assert "`r(value1)'" == "`lower_e'mile"
_gracedr_values parse, study("`six'") id(1) options(`"kind("plain")"')
assert "`r(value1)'" == "plain"

_gracedr_values parse, ///
    study("`one'") ///
    id(1) ///
    options("sex(male) race(white) age(0.1234567890123456789) weight(1e`minus'300)")
assert r(number3) == real("0.1234567890123456789")
assert r(number4) == real("1e`minus'300")
_gracedr_values parse, ///
    study("`one'") ///
    id(1) ///
    options("sex(male) race(white) age(.3333333333333333333) weight(+5)")
assert r(number3) == real(".3333333333333333333")
assert r(number4) == 5
foreach case in ".5|.5" "5.|5" " 7 |7" "8e307|8e307" {
    gettoken text expected : case, parse("|")
    local expected = substr("`expected'", 2, .)
    _gracedr_values parse, ///
        study("`one'") ///
        id(1) ///
        options(`"sex(male) race(white) age(`text') weight(1)"')
    assert r(number3) == real("`expected'")
}
foreach bad_number in "1,000" 0x1F "1 000" 1e308 {
    expect_failure, ///
        rc(198) ///
        message("gracedr: age must be a finite number") ///
        command(`"`call' options("sex(male) race(white) age(`bad_number') weight(2)")"')
}

local tab = char(9)
_gracedr_values parse, ///
    study("`one'") ///
    id(1) ///
    options("sex(male)race(white)`tab'age(1) weight(2)")
assert "`r(value1)'" == "male" & "`r(value2)'" == "white"
_gracedr_values parse, ///
    study("`one'") ///
    id(1) ///
    options(`"sex("male") race("white") age(1) weight(2)"')
assert "`r(value1)'" == "male"
local read_error "gracedr: could not read the options:"
local good_values "sex(male) race(white) age(1) weight(2)"
foreach tail in "," " x" "(" {
    expect_failure, ///
        rc(198) ///
        message("`read_error' `good_values'`tail'") ///
        command(`"`call' options("sex(male) race(white) age(1) weight(2)`tail'")"')
}
local case_rule "Variable names are case-sensitive;"
local case_text "gracedr: Sex() is not a balance variable. `case_rule'"
local case_text2 "gracedr: RACE() is not a balance variable. `case_rule'"
expect_failure, ///
    rc(198) ///
    message("`case_text' use sex()") ///
    command(`"`call' options("Sex(male) race(white) age(1) weight(2)")"')
expect_failure, ///
    rc(198) ///
    message("`case_text2' use race()") ///
    command(`"`call' options("sex(male) RACE(white) age(1) weight(2)")"')
expect_failure, ///
    rc(198) ///
    message(`"gracedr: "1" is not a level of sex. Levels are "male" "female"."') ///
    command(`"`call' options("sex(1) race(white) age(1) weight(2)")"')

local id244 ""
forvalues block = 1/48 {
    local id244 "`id244'abcde"
}
local id244 "`id244'abcd"
assert strlen("`id244'") == 244
_gracedr_values parse, study("`one'") id(`id244') options("`all_values'")
assert r(id) == "`id244'"
local id_call `"_gracedr_values parse, study("`one'") options("`all_values'")"'
foreach reserved_id in "(STUDY)" " (study) " "(Study)" {
    expect_failure, ///
        rc(198) ///
        message("gracedr: (study) is reserved and cannot be a participant id") ///
        command(`"`id_call' id(`"`reserved_id'"')"')
}
expect_failure, ///
    rc(198) ///
    message("gracedr: id() must not be empty") ///
    command(`"_gracedr_values parse, study("`one'") id(" ") options("`all_values'")"')

local weird_id = "a" + char(36) + "HOME"
_gracedr_values parse, study("`six'") id(`"`macval(weird_id)'"') options("kind(plain)")
assert r(id) == "a" + char(36) + "HOME"

make_data 3
set seed 17
local user_state = c(rngstate)
local user_rng = c(rng)
foreach failing in "" "sex(male)" "sex(other) race(white) age(1) weight(2)" ///
    "sex(male) race(white) age(abc) weight(2)" "sex(male" {
    expect_failure, ///
        rc(198) ///
        message("gracedr:") ///
        command(`"`call' options("`failing'")"')
    assert _N == 3
    assert x[3] == 30
    assert label[2] == "row2"
    assert c(rngstate) == "`user_state'"
    assert c(rng) == "`user_rng'"
}

remove_folder "`scratch'"

display "certify_values: all checks passed"
