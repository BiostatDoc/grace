version 19
clear all
set linesize 255
local stata_dir : environment GRACEDR_STATA_DIR
assert !missing("`stata_dir'")
adopath ++ "`stata_dir'/ado"
do "`stata_dir'/cert/helpers.do"

_gracedr_study folder, study("`c(tmpdir)'")
local base "`r(folder)'"
local scratch "`base'/gracedr cert codes"
remove_folder "`scratch'"
quietly mkdir "`scratch'"
local balance "categorical(sex: male female | race: white black other) continuous(age)"

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
    _gracedr_study folder, study("`scratch'/bad")
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

use "`stata_dir'/data/example_participants.dta", clear
assert _N == 40
quietly ds
assert "`r(varlist)'" == "id sex race age"
assert id[1] == "P001" & id[40] == "P040"
assert inlist(sex, "male", "female")
assert inlist(race, "white", "black", "other")
assert age[1] == 68.1

remove_folder "`scratch'"

display "certify_codes: all checks passed"
