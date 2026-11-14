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
    _gracedr_study folder, study(`"`study'"')
    local folder `"`r(folder)'"'
    _gracedr_study exists, study(`"`study'"')
    if r(exists) == 1 {
        display as error `"gracedr: a study already exists in `study'"'
        exit 602
    }
    if missing(`"`seed'"') {
        _gracedr_utc
        local seed = string(mod(floor(r(time)), 2147483648), "%21.0f")
        local seed_note " (chosen from the clock)"
    }
    _gracedr_rng check_seed, seed(`"`seed'"')
    local call `"_gracedr_design validate, n(`n') arms(`arms')"'
    foreach option in ratio weights pstar xi runin runinmethod forcedat sizebound {
        if !missing(`"``option''"') {
            local call `"`call' `option'(``option'')"'
        }
    }
    if !missing(`"`categorical'"') {
        local call `"`call' categorical(`categorical')"'
    }
    if !missing(`"`continuous'"') {
        local call `"`call' continuous(`continuous')"'
    }
    `call'
    local variable_count = r(nvars)
    local val_n = string(r(n), "%21.0f")
    local val_arms `"`r(arms)'"'
    local val_ratio `"`r(ratio)'"'
    local val_weights `"`r(weights)'"'
    local val_pstar = strtrim(string(r(pstar), "%21.15g"))
    local val_xi = strtrim(string(r(xi), "%21.15g"))
    local val_runin = string(r(runin), "%21.0f")
    local val_runinmethod `"`r(runinmethod)'"'
    local val_forcedat `"`r(forcedat)'"'
    local val_sizebound = string(r(sizebound), "%21.0f")
    local val_nvars = string(r(nvars), "%21.0f")
    local val_varnames `"`r(varnames)'"'
    local val_vartypes `"`r(vartypes)'"'
    forvalues position = 1/`variable_count' {
        local val_levels`position' `"`r(levels`position')'"'
    }
    foreach name of local val_varnames {
        local reserved = inlist("`name'", "phase", "rngstate", "time", "study", "graphs")
        local reserved = `reserved' | regexm("`name'", "^prob[0-9]+$")
        if `reserved' == 1 {
            display as error `"gracedr: balance variable name `name' is reserved"'
            exit 198
        }
    }
    local pattern = "[" + char(96) + char(36) + "]"
    local labels `"`val_arms'"'
    forvalues position = 1/`variable_count' {
        local labels `"`labels' `val_levels`position''"'
    }
    local unsafe = 0
    foreach label of local labels {
        local unsafe = `unsafe' | regexm(`"`label'"', "`pattern'")
        local unsafe = `unsafe' | strpos(`"`label'"', char(34)) == 1
    }
    if `unsafe' == 1 {
        display as error "gracedr: arm labels and levels must not contain " ///
            "double quotes, backticks or dollar signs"
        exit 198
    }
    preserve
    if !missing(`"`codes'"') {
        _gracedr_codes using `"`codes'"', arms(`val_arms')
    }
    capture mkdir `"`folder'"'
    _gracedr_study folder, study(`"`study'"')
    if r(found) == 0 {
        display as error `"gracedr: could not create the study folder `study'"'
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
            local row = `row' + 1
            quietly set obs `row'
            mata: st_sstore(`row', "name", "`item'")
            mata: st_sstore(`row', "value", st_local("val_`item'"))
            quietly replace order = `row' in `row'
        }
        _gracedr_study write, study(`"`study'"') name(design)
        if !missing(`"`codes'"') {
            quietly _gracedr_codes using `"`codes'"', arms(`val_arms')
        }
        else {
            _gracedr_study empty, name(codes)
        }
        _gracedr_study write, study(`"`study'"') name(codes)
        _gracedr_study empty, name(participants) study(`"`study'"')
        _gracedr_study write, study(`"`study'"') name(participants)
        _gracedr_rng init, study(`"`study'"') seed(`seed')
        _gracedr_study append_audit, ///
            study(`"`study'"') ///
            action(create) ///
            id("(study)") ///
            reason("study created")
    }
    local failure = _rc
    if `failure' == 0 {
        restore
        display as text `"gracedr: study created in `study'"'
        display as text "  `val_n' participants planned, run-in `val_runin', " ///
            "seed `seed'`seed_note'"
        return scalar seed = `seed'
        return local study `"`folder'"'
    }
    else {
        display as error "gracedr: setup stopped before it finished."
        display as error `"Remove design.dta, codes.dta, participants.dta, audit.dta and "' ///
            `"state.dta from `study' and run setup again."'
        exit `failure'
    }
end
