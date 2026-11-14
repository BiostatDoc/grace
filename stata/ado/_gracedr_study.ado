program define _gracedr_study
    version 19
    gettoken subcommand 0 : 0, parse(" ,")
    local known read write exists folder append_audit empty
    local is_known : list subcommand in known
    if `is_known' == 0 | missing("`subcommand'") {
        display as error "gracedr: unknown study subcommand `subcommand'"
        exit 198
    }
    _gracedr_study_`subcommand' `0'
end

program define _gracedr_study_folder, rclass
    syntax , STUDY(string)
    if regexm(`"`study'"', char(34)) == 1 {
        display as error "gracedr: study() must not contain a double quote"
        exit 198
    }
    local folder = subinstr(`"`study'"', char(92), "/", .)
    while substr(`"`folder'"', -1, 1) == "/" & !inlist(length(`"`folder'"'), 0, 1) {
        local folder = substr(`"`folder'"', 1, length(`"`folder'"') - 1)
    }
    if substr(`"`folder'"', -1, 1) == ":" {
        local folder `"`folder'/"'
    }
    return local folder `"`folder'"'
end

program define _gracedr_study_checkname
    args name
    if !inlist("`name'", "design", "participants", "codes", "audit", "state") {
        display as error "gracedr: unknown study dataset `name'"
        exit 198
    }
end

program define _gracedr_study_require
    args folder shown
    if fileexists("`folder'/design.dta") == 0 {
        display as error `"gracedr: no study found in `shown'"'
        exit 601
    }
end

program define _gracedr_study_exists, rclass
    syntax , STUDY(string)
    _gracedr_study_folder, study(`"`study'"')
    local folder `"`r(folder)'"'
    return scalar exists = fileexists(`"`folder'/design.dta"')
end

program define _gracedr_study_read
    syntax , STUDY(string) NAME(string)
    _gracedr_study_checkname "`name'"
    _gracedr_study_folder, study(`"`study'"')
    local folder `"`r(folder)'"'
    _gracedr_study_require "`folder'" "`study'"
    if fileexists("`folder'/`name'.dta") == 0 {
        display as error `"gracedr: `name'.dta is missing from `study'"'
        exit 601
    }
    use "`folder'/`name'.dta", clear
end

program define _gracedr_study_write
    syntax , STUDY(string) NAME(string)
    _gracedr_study_checkname "`name'"
    _gracedr_study_folder, study(`"`study'"')
    local folder `"`r(folder)'"'
    mata: st_local("folder_found", strofreal(direxists(st_local("folder"))))
    if `folder_found' == 0 {
        display as error `"gracedr: study folder `study' does not exist"'
        exit 601
    }
    local current "`folder'/`name'.dta"
    local pending "`folder'/`name'.tmp"
    local backup "`folder'/`name'.bak"
    capture quietly save "`pending'", replace
    if _rc == 0 {
        if fileexists("`current'") == 1 {
            copy "`current'" "`backup'", replace
        }
        copy "`pending'" "`current'", replace
        erase "`pending'"
    }
    else {
        local failure = _rc
        capture erase "`pending'"
        display as error `"gracedr: could not write `name'.dta in `study'"'
        exit `failure'
    }
end

program define _gracedr_study_empty
    syntax , NAME(string)
    clear
    if "`name'" == "design" {
        generate str244 name = ""
        generate strL value = ""
        generate double order = .
    }
    else if "`name'" == "codes" {
        generate str244 code = ""
        generate str244 arm = ""
        generate str8 status = ""
        generate str244 id = ""
    }
    else if "`name'" == "audit" {
        generate double time = .
        format time %tc
        generate str16 action = ""
        generate strL id = ""
        generate strL old = ""
        generate strL new = ""
        generate strL old_code = ""
        generate strL new_code = ""
        generate strL reason = ""
    }
    else if "`name'" == "state" {
        generate strL state = ""
        generate double seed = .
        generate str16 generator = ""
        generate double format_version = .
    }
    else {
        display as error "gracedr: no empty schema for `name'"
        exit 198
    }
end

program define _gracedr_study_append_audit
    syntax ,                ///
        STUDY(string)       ///
        ACTION(string)      ///
        ID(string)          ///
        [                   ///
        OLD(string)         ///
        NEW(string)         ///
        OLDCODE(string)     ///
        NEWCODE(string)     ///
        REASON(string)      ///
        ]
    if !inlist("`action'", "create", "randomize", "override") {
        display as error "gracedr: unknown audit action `action'"
        exit 198
    }
    _gracedr_study_folder, study(`"`study'"')
    local folder `"`r(folder)'"'
    _gracedr_study_require "`folder'" "`study'"
    _gracedr_utc
    local stamp = r(time)
    preserve
    if fileexists("`folder'/audit.dta") == 1 {
        _gracedr_study_read, study(`"`study'"') name(audit)
    }
    else {
        _gracedr_study_empty, name(audit)
    }
    local row = _N + 1
    quietly set obs `row'
    mata: st_store(`row', "time", `stamp')
    local variables action id old new old_code new_code reason
    local options action id old new oldcode newcode reason
    forvalues position = 1/7 {
        local variable : word `position' of `variables'
        local option : word `position' of `options'
        mata: st_sstore(`row', "`variable'", st_local("`option'"))
    }
    _gracedr_study_write, study(`"`study'"') name(audit)
    restore
end
