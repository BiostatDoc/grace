version 19
clear all
set linesize 255
local stata_dir : environment GRACEDR_STATA_DIR
assert !missing("`stata_dir'")
adopath ++ "`stata_dir'/ado"
do "`stata_dir'/cert/helpers.do"

program define write_codes_csv
    args file
    tempname handle
    file open `handle' using "`file'", write text replace
    file write `handle' "code,arm" _n
    file write `handle' "007,A" _n
    file write `handle' "008,B" _n
    file write `handle' " 009 , A " _n
    file write `handle' "0010,B" _n
    file close `handle'
end

program define write_text_file
    args file first_line second_line third_line
    tempname handle
    file open `handle' using "`file'", write text replace
    foreach line in "`first_line'" "`second_line'" "`third_line'" {
        if !missing("`line'") {
            file write `handle' "`line'" _n
        }
    }
    file close `handle'
end

_gracedr_study folder, study("`c(tmpdir)'")
local base "`r(folder)'"
local scratch "`base'/gracedr cert setup"
remove_folder "`scratch'"
quietly mkdir "`scratch'"
local balance "categorical(sex: male female | race: white black other) continuous(age)"

make_data 3
set seed 777
local user_state = c(rngstate)
local user_rng = c(rng)
gracedr setup, study("`scratch'/one") n(60) arms(A B) `balance' seed(12345)
assert _N == 3
assert x[3] == 30
assert c(rngstate) == "`user_state'"
assert c(rng) == "`user_rng'"
assert r(seed) == 12345
foreach name in design participants codes audit state {
    assert fileexists("`scratch'/one/`name'.dta")
}

local one "`scratch'/one"
foreach case in "n 60" "arms A B" "ratio 1 1" "runin 10" "runinmethod tilted" ///
    "sizebound 3" "nvars 3" "varnames sex race age" "vartypes categorical categorical continuous" {
    gettoken setting expected : case
    local expected = strtrim("`expected'")
    get_setting "`one'" "`setting'"
    assert "`r(value)'" == "`expected'"
}
get_setting "`one'" "pstar"
assert real("`r(value)'") == 0.3
get_setting "`one'" "xi"
assert real("`r(value)'") == 0.65
get_setting "`one'" "forcedat"
assert "`r(value)'" == ""
get_setting "`one'" "levels1"
assert `"`r(value)'"' == `""male" "female""'
get_setting "`one'" "levels2"
assert `"`r(value)'"' == `""white" "black" "other""'
get_setting "`one'" "levels3"
assert `"`r(value)'"' == ""
get_setting "`one'" "weights"
assert wordcount("`r(value)'") == 3
preserve
_gracedr_study read, study("`one'") name(design)
assert order == _n
assert "`: type value'" == "strL"
restore

preserve
_gracedr_study read, study("`one'") name(participants)
assert _N == 0
quietly ds
assert "`r(varlist)'" == "seq id sex race age arm code prob1 prob2 phase rngstate time"
assert "`: type seq'" == "long"
assert substr("`: type id'", 1, 3) == "str"
assert substr("`: type sex'", 1, 3) == "str"
assert "`: type age'" == "double"
assert "`: type prob1'" == "double"
assert "`: type rngstate'" == "strL"
assert "`: type time'" == "double"
assert "`: format time'" == "%tc"
_gracedr_study read, study("`one'") name(codes)
assert _N == 0
quietly ds
assert "`r(varlist)'" == "code arm status id"
_gracedr_study read, study("`one'") name(state)
assert _N == 1
assert seed[1] == 12345
assert generator[1] == "mt64"
assert format_version[1] == 1
_gracedr_study read, study("`one'") name(audit)
assert _N == 1
assert action[1] == "create"
assert id[1] == "(study)"
assert reason[1] == "study created"
assert !missing(time[1])
restore

expect_failure, ///
    rc(602) ///
    message("gracedr: a study already exists in `one'") ///
    command(`"gracedr setup, study("`one'") n(30) arms(A B) `balance'"')
get_setting "`one'" "n"
assert "`r(value)'" == "60"
assert !fileexists("`one'/design.bak")

gracedr setup, study("`scratch'/two") n(60) arms(A B) `balance'
local drawn_seed = r(seed)
assert inrange(`drawn_seed', 0, 2147483647)
assert `drawn_seed' == floor(`drawn_seed')
preserve
_gracedr_study read, study("`scratch'/two") name(state)
assert seed[1] == `drawn_seed'
restore
assert c(rngstate) == "`user_state'"

gracedr setup, ///
    study("`scratch'/three") ///
    n(100) ///
    arms(A B) ///
    ratio(2 1) ///
    weights(1 2 1) ///
    pstar(0.2) ///
    xi(0.7) ///
    runin(20) ///
    runinmethod(rar) ///
    forcedat(60 40) ///
    sizebound(2) ///
    categorical(sex: male female) ///
    continuous(age weight) ///
    seed(1)
local three "`scratch'/three"
get_setting "`three'" "ratio"
assert "`r(value)'" == "2 1"
get_setting "`three'" "runin"
assert "`r(value)'" == "20"
get_setting "`three'" "runinmethod"
assert "`r(value)'" == "rar"
get_setting "`three'" "forcedat"
assert "`r(value)'" == "40 60"
get_setting "`three'" "sizebound"
assert "`r(value)'" == "2"
get_setting "`three'" "varnames"
assert "`r(value)'" == "sex age weight"
get_setting "`three'" "weights"
local epsilon = 1 / 1000000000000
assert inrange(real(word("`r(value)'", 2)), 0.5 - `epsilon', 0.5 + `epsilon')

expect_failure, ///
    rc(198) ///
    message("gracedr: n() must be a whole number from 2 to 2000000000") ///
    command(`"gracedr setup, study("`scratch'/four") n(1) arms(A B) `balance'"')
dir_exists "`scratch'/four"
assert r(found) == 0
expect_failure, ///
    rc(198) ///
    message("gracedr: arms() must be at least 2 unique labels") ///
    command(`"gracedr setup, study("`scratch'/four") n(60) arms(A) `balance'"')
dir_exists "`scratch'/four"
assert r(found) == 0
expect_failure, ///
    rc(603) ///
    message("gracedr: could not create the study folder") ///
    command(`"gracedr setup, study("`scratch'/missing/deeper") n(60) arms(A B) `balance'"')
foreach reserved in phase rngstate time study graphs prob1 prob2 {
    expect_failure, ///
        rc(198) ///
        message("gracedr: balance variable name `reserved' is reserved") ///
        command(`"gracedr setup, study("`scratch'/four") n(60) arms(A B) continuous(`reserved')"')
}
local dollar = char(36)
local message_start "gracedr: arm labels and levels must not contain"
local message "`message_start' double quotes, backticks or dollar signs"
expect_failure, ///
    rc(198) ///
    message("`message'") ///
    command(`"gracedr setup, study("`scratch'/four") n(60) arms(A B`dollar') `balance'"')
local levels_call "gracedr setup, study("`scratch'/four") n(60) arms(A B)"
expect_failure, ///
    rc(198) ///
    message("`message'") ///
    command(`"`levels_call' categorical(sex: male fe`dollar')"')
dir_exists "`scratch'/four"
assert r(found) == 0

local csv "`scratch'/codes.csv"
write_codes_csv "`csv'"
gracedr codes using "`csv'", arms(A B)
assert _N == 4
assert code[1] == "007" & arm[1] == "A"
assert code[3] == "009" & arm[3] == "A"
assert code[4] == "0010" & arm[4] == "B"
assert status[1] == "unused" & id[1] == ""
quietly ds
assert "`r(varlist)'" == "code arm status id"
quietly save "`scratch'/csv_codes.dta", replace
keep code arm
quietly save "`scratch'/plain_codes.dta", replace

gracedr codes using "`scratch'/plain_codes.dta", arms(A B)
quietly cf _all using "`scratch'/csv_codes.dta"
assert _N == 4

gracedr setup, study("`scratch'/csvstudy") n(60) arms(A B) `balance' codes("`csv'") seed(5)
gracedr setup, study("`scratch'/dtastudy") n(60) arms(A B) `balance' ///
    codes("`scratch'/plain_codes.dta") seed(5)
_gracedr_study read, study("`scratch'/csvstudy") name(codes)
quietly save "`scratch'/from_csv.dta", replace
_gracedr_study read, study("`scratch'/dtastudy") name(codes)
quietly cf _all using "`scratch'/from_csv.dta"
assert _N == 4
assert status[1] == "unused" & id[1] == ""

clear
quietly set obs 3
generate long code = 100 + _n
generate str1 arm = cond(_n == 2, "B", "A")
quietly save "`scratch'/numeric_codes.dta", replace
gracedr codes using "`scratch'/numeric_codes.dta", arms(A B)
assert code[1] == "101" & code[3] == "103"
clear
quietly set obs 2
generate double code = _n + 0.5
generate str1 arm = "A"
quietly save "`scratch'/fraction_codes.dta", replace
expect_failure, ///
    rc(198) ///
    message("gracedr: code must be text or whole numbers") ///
    command(`"gracedr codes using "`scratch'/fraction_codes.dta", arms(A B)"')

write_text_file "`scratch'/nocolumn.csv" "code,group" "1,A"
write_text_file "`scratch'/nocolumns.csv" "x,y" "1,A"
write_text_file "`scratch'/duplicate.csv" "code,arm" "1,A" "1,B"
write_text_file "`scratch'/emptycode.csv" "code,arm" "1,A" ",B"
write_text_file "`scratch'/unknown.csv" "code,arm" "1,A" "2,C"
write_text_file "`scratch'/norows.csv" "code,arm"
local bad_call `"gracedr setup, study("`scratch'/bad") n(60) arms(A B) `balance'"'
foreach case in ///
    "nocolumn|gracedr: the codes file is missing column(s): arm" ///
    "nocolumns|gracedr: the codes file is missing column(s): code arm" ///
    "duplicate|gracedr: codes must be unique" ///
    "emptycode|gracedr: every code must be non-empty" ///
    "unknown|gracedr: unknown arm(s) in the codes file: C" ///
    "norows|gracedr: the codes file has no rows" {
    gettoken file message : case, parse("|")
    local message = substr("`message'", 2, .)
    expect_failure, ///
        rc(198) ///
        message("`message'") ///
        command(`"gracedr codes using "`scratch'/`file'.csv", arms(A B)"')
    expect_failure, ///
        rc(198) ///
        message("`message'") ///
        command(`"`bad_call' codes("`scratch'/`file'.csv")"')
    dir_exists "`scratch'/bad"
    assert r(found) == 0
}
expect_failure, ///
    rc(601) ///
    message("gracedr: codes file `scratch'/nothing.csv not found") ///
    command(`"gracedr codes using "`scratch'/nothing.csv", arms(A B)"')
expect_failure, ///
    message("option arms() required") ///
    command(`"gracedr codes using "`csv'""')

gracedr codes using "`stata_dir'/data/example_codes.csv", arms(A B)
assert _N == 200
quietly count if arm == "A"
assert r(N) == 100
quietly count if arm == "B"
assert r(N) == 100
quietly count if status == "unused"
assert r(N) == 200
quietly save "`scratch'/example_from_csv.dta", replace
gracedr codes using "`stata_dir'/data/example_codes.dta", arms(A B)
quietly cf _all using "`scratch'/example_from_csv.dta"
gracedr setup, ///
    study("`scratch'/example") ///
    n(40) ///
    arms(A B) ///
    categorical(sex: male female | race: white black other) ///
    continuous(age) ///
    codes("`stata_dir'/data/example_codes.csv") ///
    seed(20260611)
_gracedr_study read, study("`scratch'/example") name(codes)
assert _N == 200
gracedr setup, ///
    study("`scratch'/three arms") ///
    n(90) ///
    arms(A B C) ///
    ratio(2 1 1) ///
    categorical(sex: male female) ///
    runin(0)
get_setting "`scratch'/three arms" "runin"
assert "`r(value)'" == "0"
preserve
_gracedr_study read, study("`scratch'/three arms") name(participants)
quietly ds
assert "`r(varlist)'" == "seq id sex arm code prob1 prob2 prob3 phase rngstate time"
restore

use "`stata_dir'/data/example_participants.dta", clear
assert _N == 40
quietly ds
assert "`r(varlist)'" == "id sex race age"
assert id[1] == "P001" & id[40] == "P040"
assert inlist(sex, "male", "female")
assert inlist(race, "white", "black", "other")
assert age[1] == 68.1

gracedr
expect_failure, ///
    rc(198) ///
    message("gracedr: unknown subcommand nonsense") ///
    command(`"gracedr nonsense, study("`one'")"')
expect_failure, ///
    rc(199) ///
    message("gracedr: randomize is not available yet") ///
    command(`"gracedr randomize, study("`one'")"')

write_text_file "`scratch'/blankarm.csv" "code,arm" "1,A" "2,"
write_text_file "`scratch'/mixedcase.csv" "code,arm" "1,a"
write_text_file "`scratch'/trimdup.csv" "code,arm" "1,A" " 1,B"
write_text_file "`scratch'/long_code.csv" "code,arm"
tempname handle
local long_code ""
forvalues block = 1/49 {
    local long_code "`long_code'abcde"
}
file open `handle' using "`scratch'/long_code.csv", write text append
file write `handle' "`long_code',A" _n
file close `handle'
write_text_file "`scratch'/spaced.csv" "code,arm" "1,Arm one" "2,Arm two"
write_text_file "`scratch'/spaced_unknown.csv" "code,arm" "1,Arm three"
write_text_file "`scratch'/extra.csv" "Code,Arm,Note" "1,A,x" "2,B,y"
foreach case in ///
    "blankarm|gracedr: unknown arm(s) in the codes file: (blank)" ///
    "mixedcase|gracedr: unknown arm(s) in the codes file: a" ///
    "trimdup|gracedr: codes must be unique" ///
    "long_code|gracedr: code values must be at most 244 bytes" {
    gettoken file message : case, parse("|")
    local message = substr("`message'", 2, .)
    expect_failure, ///
        rc(198) ///
        message("`message'") ///
        command(`"gracedr codes using "`scratch'/`file'.csv", arms(A B)"')
}
expect_failure, ///
    rc(198) ///
    message(`"gracedr: unknown arm(s) in the codes file: "Arm three""') ///
    command(`"gracedr codes using "`scratch'/spaced_unknown.csv", arms("Arm one" "Arm two")"')

make_data 3
expect_failure, ///
    rc(198) ///
    message("gracedr: unknown arm(s) in the codes file: (blank)") ///
    command(`"gracedr codes using "`scratch'/blankarm.csv", arms(A B)"')
assert _N == 3
assert x[3] == 30
assert label[2] == "row2"

gracedr codes using "`scratch'/spaced.csv", arms("Arm one" "Arm two")
assert r(n) == 2
assert _N == 2
assert arm[1] == "Arm one" & arm[2] == "Arm two"
gracedr codes using "`scratch'/extra.csv", arms(A B)
assert r(n) == 2
quietly ds
assert "`r(varlist)'" == "code arm status id"
gracedr setup, ///
    study("`scratch'/spaced") ///
    n(40) ///
    arms("Arm one" "Arm two") ///
    `balance' ///
    codes("`scratch'/spaced.csv") ///
    seed(3)
get_setting "`scratch'/spaced" "arms"
assert `"`r(value)'"' == `""Arm one" "Arm two""'
preserve
_gracedr_study read, study("`scratch'/spaced") name(codes)
assert _N == 2
assert arm[2] == "Arm two"
restore

quietly copy "`scratch'/plain_codes.dta" "`scratch'/upper.DTA", replace
gracedr codes using "`scratch'/upper.DTA", arms(A B)
assert _N == 4
gracedr codes using "`scratch'/csv_codes.dta", arms(A B)
quietly ds
assert "`r(varlist)'" == "code arm status id"
clear
quietly set obs 2
generate str3 code = "x" + string(_n)
generate byte arm = _n
quietly save "`scratch'/numeric_arm.dta", replace
gracedr codes using "`scratch'/numeric_arm.dta", arms(1 2)
assert arm[1] == "1" & arm[2] == "2"

gracedr codes using "`scratch'/csv_codes.dta", arms(A B)
assert r(n) == 4

local four_call `"gracedr setup, study("`scratch'/four") n(60) arms(A B) `balance'"'
foreach bad_seed in -1 2147483648 1.5 abc {
    expect_failure, ///
        rc(198) ///
        message("gracedr: seed() must be a whole number from 0 to 2147483647") ///
        command(`"`four_call' seed(`bad_seed')"')
    dir_exists "`scratch'/four"
    assert r(found) == 0
}
foreach good_seed in 0 2147483647 {
    gracedr setup, ///
        study("`scratch'/seed`good_seed'") ///
        n(60) ///
        arms(A B) ///
        `balance' ///
        seed(`good_seed')
    assert r(seed) == `good_seed'
    preserve
    _gracedr_study read, study("`scratch'/seed`good_seed'") name(state)
    assert seed[1] == `good_seed'
    restore
}
gracedr setup, study("`scratch'/clock1") n(60) arms(A B) `balance'
local first_seed = r(seed)
sleep 30
gracedr setup, study("`scratch'/clock2") n(60) arms(A B) `balance'
local second_seed = r(seed)
local same_seed = `first_seed' == `second_seed'
assert `same_seed' == 0

local backslash_study = subinstr("`scratch'/norm", "/", char(92), .)
gracedr setup, study("`backslash_study'`=char(92)'") n(60) arms(A B) `balance' seed(9)
assert `"`r(study)'"' == "`scratch'/norm"

foreach reserved in id arm code seq {
    expect_failure, ///
        rc(198) ///
        message("gracedr: balance variable name `reserved' is reserved") ///
        command(`"gracedr setup, study("`scratch'/four") n(60) arms(A B) continuous(`reserved')"')
}

gracedr setup, ///
    study("`scratch'/order") ///
    n(60) ///
    arms(A B) ///
    continuous(age) ///
    categorical(sex: male female) ///
    weights(1 3) ///
    seed(2)
get_setting "`scratch'/order" "varnames"
assert "`r(value)'" == "sex age"
get_setting "`scratch'/order" "weights"
assert inrange(real(word("`r(value)'", 1)), 0.25 - `epsilon', 0.25 + `epsilon')
assert inrange(real(word("`r(value)'", 2)), 0.75 - `epsilon', 0.75 + `epsilon')


quietly mkdir "`scratch'/partial"
quietly copy "`one'/state.dta" "`scratch'/partial/state.dta"
make_data 3
set seed 31
local partial_state = c(rngstate)
expect_failure, ///
    rc(602) ///
    message("gracedr: setup stopped before it finished.") ///
    command(`"gracedr setup, study("`scratch'/partial") n(60) arms(A B) `balance' seed(4)"')
assert _N == 3
assert x[3] == 30
assert c(rngstate) == "`partial_state'"
foreach name in design codes participants state {
    assert fileexists("`scratch'/partial/`name'.dta")
}
assert !fileexists("`scratch'/partial/audit.dta")
expect_failure, ///
    rc(198) ///
    message("gracedr: n() must be a whole number from 2 to 2000000000") ///
    command(`"gracedr setup, study("`scratch'/four") n(1) arms(A B) `balance'"')
assert _N == 3
assert c(rngstate) == "`partial_state'"
expect_failure, ///
    rc(198) ///
    message("gracedr: unknown arm(s) in the codes file: (blank)") ///
    command(`"`four_call' codes("`scratch'/blankarm.csv")"')
assert _N == 3
assert c(rngstate) == "`partial_state'"


remove_folder "`scratch'"

display "certify_setup: all checks passed"
