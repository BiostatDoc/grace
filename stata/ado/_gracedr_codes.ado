program define _gracedr_codes, rclass
    version 19
    syntax using/, ARMS(string asis)
    local arms : list clean arms
    if fileexists(`"`using'"') == 0 {
        display as error `"gracedr: codes file `using' not found"'
        exit 601
    }
    local is_stata = substr(strlower(`"`using'"'), -4, 4) == ".dta"
    if `is_stata' == 1 {
        quietly use `"`using'"', clear
    }
    else {
        quietly import delimited using `"`using'"', varnames(1) stringcols(_all) clear
    }
    local absent ""
    foreach column in code arm {
        capture confirm variable `column'
        if _rc == 0 {
            continue
        }
        local absent "`absent' `column'"
    }
    if !missing("`absent'") {
        display as error "gracedr: the codes file is missing column(s):`absent'"
        exit 198
    }
    keep code arm
    foreach column in code arm {
        capture confirm string variable `column'
        if _rc == 0 {
            continue
        }
        quietly count if !missing(`column') & `column' == floor(`column')
        local whole = r(N) == _N
        if `whole' == 0 {
            display as error "gracedr: `column' must be text or whole numbers"
            exit 198
        }
        quietly tostring `column', replace format(%21.0f)
    }
    foreach column in code arm {
        quietly count if inrange(strlen(`column'), 245, 1e15)
        local fits = r(N) == 0
        if `fits' == 0 {
            display as error "gracedr: `column' values must be at most 244 bytes"
            exit 198
        }
        quietly generate str244 `column'_text = `column'
        quietly drop `column'
        quietly rename `column'_text `column'
        quietly replace `column' = ustrtrim(`column')
    }
    if _N == 0 {
        display as error "gracedr: the codes file has no rows"
        exit 198
    }
    quietly count if missing(code)
    local none_empty = r(N) == 0
    if `none_empty' == 0 {
        display as error "gracedr: every code must be non-empty"
        exit 198
    }
    tempvar copies
    quietly duplicates tag code, generate(`copies')
    quietly count if inrange(`copies', 1, 1e15)
    local repeated = r(N) == 0
    if `repeated' == 0 {
        display as error "gracedr: codes must be unique"
        exit 198
    }
    quietly count if missing(arm)
    local none_blank = r(N) == 0
    if `none_blank' == 0 {
        display as error "gracedr: unknown arm(s) in the codes file: (blank)"
        exit 198
    }
    quietly levelsof arm, local(found)
    local found : list clean found
    local unknown : list found - arms
    local unknown_count : list sizeof unknown
    if inrange(`unknown_count', 1, 1e15) {
        display as error `"gracedr: unknown arm(s) in the codes file: `macval(unknown)'"'
        exit 198
    }
    quietly generate str8 status = "unused"
    quietly generate str244 id = ""
    display as text "gracedr: `=_N' treatment codes are valid."
    foreach arm of local arms {
        quietly count if arm == "`arm'"
        display as text "  arm `arm': `r(N)' codes"
    }
    return scalar n = _N
end
