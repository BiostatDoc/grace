program define gracedr_setup, rclass
    version 19
    syntax ,                        ///
        STUDY(string)               ///
        N(string)                   ///
        ARMS(string asis)           ///
        [                           ///
        RATIO(string)               ///
        WEIGHTS(string)             ///
        PSTAR(string)               ///
        XI(string)                  ///
        RUNIN(string)               ///
        RUNINMETHOD(string)         ///
        FORCEDAT(string)            ///
        SIZEBOUND(string)           ///
        CATEGORICAL(string asis)    ///
        CONTINUOUS(string asis)     ///
        CODES(string)               ///
        SEED(string)                ///
        ]
    _gracedr_study folder, study(`"`macval(study)'"')
    mata: st_local("folder", st_global("r(folder)"))
    _gracedr_study exists, study(`"`macval(study)'"')
    if r(exists) == 1 {
        display as error `"gracedr: a study already exists in `macval(study)'"'
        exit 602
    }
    _gracedr_study reserved_id
    mata: st_local("audit_id", st_global("r(id)"))
    if missing(`"`seed'"') {
        _gracedr_utc
        local seed = string(mod(floor(r(time)), 2147483648), "%21.0f")
        local seed_note " (chosen from the clock)"
    }
    _gracedr_rng check_seed, seed(`"`seed'"')
    local call `"_gracedr_design validate, n(`n') arms(`macval(arms)')"'
    foreach option in ratio weights pstar xi runin runinmethod forcedat sizebound {
        if !missing(`"``option''"') {
            local call `"`call' `option'(``option'')"'
        }
    }
    if !missing(`"`macval(categorical)'"') {
        local call `"`call' categorical(`macval(categorical)')"'
    }
    if !missing(`"`macval(continuous)'"') {
        local call `"`call' continuous(`macval(continuous)')"'
    }
    `call'
    local variable_count = r(nvars)
    local val_n = string(r(n), "%21.0f")
    mata: st_local("val_arms", st_global("r(arms)"))
    mata: st_local("val_ratio", st_global("r(ratio)"))
    mata: st_local("val_weights", st_global("r(weights)"))
    local val_pstar = strtrim(string(r(pstar), "%21.15g"))
    local val_xi = strtrim(string(r(xi), "%21.15g"))
    local val_runin = string(r(runin), "%21.0f")
    mata: st_local("val_runinmethod", st_global("r(runinmethod)"))
    mata: st_local("val_forcedat", st_global("r(forcedat)"))
    local val_sizebound = string(r(sizebound), "%21.0f")
    local val_nvars = string(r(nvars), "%21.0f")
    mata: st_local("val_varnames", st_global("r(varnames)"))
    mata: st_local("val_vartypes", st_global("r(vartypes)"))
    forvalues position = 1/`variable_count' {
        mata: st_local("val_levels`position'", st_global("r(levels`position')"))
    }
    foreach name of local val_varnames {
        local reserved = inlist("`macval(name)'", "phase", "rngstate", "time", "study", "graphs")
        local reserved = `reserved' | regexm("`macval(name)'", "^prob[0-9]+$")
        if `reserved' == 1 {
            display as error `"gracedr: balance variable name `macval(name)' is reserved"'
            exit 198
        }
    }
    local pattern = "[" + char(96) + char(36) + "]"
    local labels `"`macval(val_arms)'"'
    forvalues position = 1/`variable_count' {
        local labels `"`labels' `val_levels`position''"'
    }
    local unsafe = 0
    foreach label of local labels {
        local unsafe = `unsafe' | regexm(`"`macval(label)'"', "`pattern'")
        local unsafe = `unsafe' | strpos(`"`macval(label)'"', char(34)) == 1
    }
    if `unsafe' == 1 {
        display as error "gracedr: arm labels and levels must not contain " ///
            "double quotes, backticks or dollar signs"
        exit 198
    }
    preserve
    if !missing(`"`macval(codes)'"') {
        _gracedr_codes using `"`macval(codes)'"', arms(`macval(val_arms)')
    }
    capture mkdir `"`macval(folder)'"'
    _gracedr_study folder, study(`"`macval(study)'"')
    if r(found) == 0 {
        display as error `"gracedr: could not create the study folder `macval(study)'"'
        exit 603
    }
    capture noisily {
        _gracedr_study empty, name(design)
        local items n arms ratio weights pstar xi runin runinmethod forcedat sizebound
        local items "`items' nvars varnames vartypes"
        forvalues position = 1/`variable_count' {
            local items "`items' levels`position'"
        }
        local row = 0
        foreach item of local items {
            local row = `macval(row)' + 1
            quietly set obs `macval(row)'
            mata: st_sstore(`macval(row)', "name", "`item'")
            mata: st_sstore(`macval(row)', "value", st_local("val_`item'"))
            quietly replace order = `macval(row)' in `macval(row)'
        }
        _gracedr_study write, study(`"`macval(study)'"') name(design)
        if !missing(`"`macval(codes)'"') {
            quietly _gracedr_codes using `"`macval(codes)'"', arms(`macval(val_arms)')
        }
        else {
            _gracedr_study empty, name(codes)
        }
        _gracedr_study write, study(`"`macval(study)'"') name(codes)
        _gracedr_study empty, name(participants) study(`"`macval(study)'"')
        _gracedr_study write, study(`"`macval(study)'"') name(participants)
        _gracedr_rng init, study(`"`macval(study)'"') seed(`seed')
        _gracedr_study append_audit, ///
            study(`"`macval(study)'"') ///
            action(create) ///
            id(`"`audit_id'"') ///
            reason("study created")
    }
    local failure = _rc
    if `failure' == 0 {
        restore
        display as text `"gracedr: study created in `macval(study)'"'
        display as text "  `val_n' participants planned, run-in `val_runin', " ///
            "seed `seed'`seed_note'"
        return scalar seed = `seed'
        return local study `"`macval(folder)'"'
    }
    else {
        display as error "gracedr: setup stopped before it finished."
        display as error `"Remove design.dta, codes.dta, participants.dta, audit.dta and "' ///
            `"state.dta from `macval(study)' and run setup again."'
        exit `failure'
    }
end
