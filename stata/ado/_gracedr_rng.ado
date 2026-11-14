program define _gracedr_rng
    version 19
    gettoken subcommand 0 : 0, parse(" ,")
    local known init draw replay
    local is_known : list subcommand in known
    if `is_known' == 0 | missing("`subcommand'") {
        display as error "gracedr: unknown rng subcommand `subcommand'"
        exit 198
    }
    _gracedr_rng_`subcommand' `0'
end

program define _gracedr_rng_init
    syntax , STUDY(string) SEED(string)
    local whole = regexm(`"`seed'"', "^[0-9]+$")
    if `whole' == 1 {
        local whole = inrange(real(`"`seed'"'), 0, 2147483647)
    }
    if `whole' == 0 {
        display as error "gracedr: seed() must be a whole number from 0 to 2147483647"
        exit 198
    }
    _gracedr_study exists, study(`"`study'"')
    if r(exists) == 0 {
        display as error `"gracedr: no study found in `study'"'
        exit 601
    }
    _gracedr_study folder, study(`"`study'"')
    local folder `"`r(folder)'"'
    if fileexists("`folder'/state.dta") == 1 {
        display as error `"gracedr: state.dta already exists in `study'"'
        exit 602
    }
    local user_rng = c(rng)
    local user_state = c(rngstate)
    set rng mt64
    set seed `seed'
    local state = c(rngstate)
    set rng `user_rng'
    set rngstate `user_state'
    preserve
    _gracedr_study empty, name(state)
    quietly set obs 1
    mata: st_sstore(1, "state", st_local("state"))
    quietly replace seed = `seed'
    quietly replace generator = "mt64"
    quietly replace format_version = 1
    _gracedr_study write, study(`"`study'"') name(state)
    restore
end

program define _gracedr_rng_draw, rclass
    syntax , STUDY(string) PROBS(string) ARMS(string)
    preserve
    _gracedr_study read, study(`"`study'"') name(state)
    if _N == 1 {
        local state = state[1]
        local generator = generator[1]
        _gracedr_rng_pick, ///
            state(`"`state'"') ///
            generator(`"`generator'"') ///
            probs(`"`probs'"') ///
            arms(`"`arms'"')
        local arm `"`r(arm)'"'
        local new_state `"`r(new_state)'"'
        mata: st_sstore(1, "state", st_local("new_state"))
        _gracedr_study write, study(`"`study'"') name(state)
        restore
        return local arm `"`arm'"'
        return local state_before `"`state'"'
    }
    else {
        display as error `"gracedr: state.dta must have exactly one row in `study'"'
        exit 198
    }
end

program define _gracedr_rng_replay, rclass
    syntax , STATE(string) PROBS(string) ARMS(string) [GENERATOR(string)]
    if missing("`generator'") {
        local generator mt64
    }
    _gracedr_rng_pick, ///
        state(`"`state'"') ///
        generator(`"`generator'"') ///
        probs(`"`probs'"') ///
        arms(`"`arms'"')
    return local arm `"`r(arm)'"'
end

program define _gracedr_rng_pick, rclass
    syntax , STATE(string) GENERATOR(string) PROBS(string) ARMS(string)
    local count : word count `probs'
    local arm_count : word count `arms'
    local consistent = inrange(`count', 1, 1000) & `count' == `arm_count'
    if `consistent' == 0 {
        display as error "gracedr: probs() and arms() must have the same number of items"
        exit 198
    }
    local total = 0
    forvalues position = 1/`count' {
        local value = real(word(`"`probs'"', `position'))
        if !inrange(`value', 0, 1e300) {
            display as error "gracedr: probs() must be numbers that are not negative"
            exit 198
        }
        local total = `total' + `value'
    }
    if `total' == 0 {
        display as error "gracedr: probs() must add up to more than zero"
        exit 198
    }
    local well_formed = regexm(`"`state'"', "^[0-9A-Za-z]+$")
    if `well_formed' == 1 {
        local well_formed = regexm(`"`generator'"', "^[0-9A-Za-z]+$")
    }
    if `well_formed' == 0 {
        display as error "gracedr: state is not a valid random number state"
        exit 198
    }
    local partial = 0
    forvalues position = 1/`count' {
        local partial = `partial' + real(word(`"`probs'"', `position'))
        local cumulative`position' = `partial' / `total'
    }
    local user_rng = c(rng)
    local user_state = c(rngstate)
    capture set rng `generator'
    local failure = _rc
    if `failure' == 0 {
        capture set rngstate `state'
        local failure = _rc
    }
    if `failure' == 0 {
        local uniform = runiform()
        local new_state = c(rngstate)
    }
    set rng `user_rng'
    set rngstate `user_state'
    if `failure' == 0 {
        local chosen = 0
        forvalues position = 1/`count' {
            local below = !missing(`uniform') & `uniform' < `cumulative`position''
            if `chosen' == 0 & `below' == 1 {
                local chosen = `position'
            }
        }
        local arm : word `chosen' of `arms'
        return local arm `"`arm'"'
        return local new_state `"`new_state'"'
    }
    else {
        display as error "gracedr: state is not a valid random number state"
        exit 198
    }
end
