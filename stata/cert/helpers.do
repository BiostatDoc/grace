program define expect_failure
    syntax , MESSAGE(string) COMMAND(string) [RC(integer -1)]
    tempfile capture_log
    quietly log using "`capture_log'", text replace name(expect)
    capture noisily `command'
    local actual = _rc
    quietly log close expect
    if `rc' == -1 {
        assert !missing(`actual') & `actual' != 0
    }
    else {
        assert `actual' == `rc'
    }
    tempname handle
    local found = 0
    file open `handle' using "`capture_log'", read text
    file read `handle' row
    while r(eof) == 0 {
        local position = strpos(`"`macval(row)'"', `"`message'"')
        if !missing(`position') & `position' > 0 {
            local found = 1
        }
        file read `handle' row
    }
    file close `handle'
    assert `found' == 1
end

program define make_data
    args nobs
    clear
    quietly set obs `nobs'
    generate double x = _n * 10
    generate str10 label = "row" + string(_n)
end

program define dir_exists, rclass
    args folder
    mata: st_local("folder_found", strofreal(direxists(st_local("folder"))))
    return scalar found = `folder_found'
end

program define remove_folder
    args folder
    dir_exists "`folder'"
    if r(found) == 1 {
        local subfolders : dir "`folder'" dirs "*"
        foreach subfolder of local subfolders {
            remove_folder "`folder'/`subfolder'"
        }
        local files : dir "`folder'" files "*"
        foreach file of local files {
            quietly erase "`folder'/`file'"
        }
        quietly rmdir "`folder'"
    }
end

program define get_setting, rclass
    args folder name
    preserve
    _gracedr_study read, study(`"`folder'"') name(design)
    quietly keep if name == "`name'"
    assert _N == 1
    return local value = value[1]
    restore
end
