program define _gracedr_design
    version 19
    gettoken subcommand 0 : 0, parse(" ,")
    local known runin_size targets checkpoints validate
    local is_known : list subcommand in known
    if `is_known' == 0 | missing("`subcommand'") {
        display as error `"gracedr: unknown design subcommand `subcommand'"'
        exit 198
    }
    _gracedr_design_`subcommand' `0'
end

program define _gracedr_design_whole, rclass
    args value lower upper
    local number = real(`"`value'"')
    local valid = !missing(`number') & inrange(`number', `lower', `upper')
    if `valid' == 1 {
        local valid = `number' == floor(`number')
    }
    return scalar valid = `valid'
    return scalar number = `number'
end

program define _gracedr_design_check_counts
    syntax , N(string) RUNIN(string) [FORCEDAT(string)]
    _gracedr_design_whole `"`runin'"' 0 `n'
    if r(valid) == 0 {
        display as error "gracedr: runin() must be a whole number from 0 to `n'"
        exit 198
    }
    foreach value of local forcedat {
        _gracedr_design_whole `"`value'"' 1 `n'
        if r(valid) == 0 {
            display as error "gracedr: forcedat() values must be whole numbers from 1 to `n'"
            exit 198
        }
    }
end

program define _gracedr_design_compute_runin, rclass
    syntax , N(real) RATIO(string)
    local total = 0
    local integral = 1
    foreach value of local ratio {
        local total = `total' + `value'
        local integral = `integral' * (`value' == round(`value'))
    }
    local candidate = max(10, ceil(round(0.1 * `n', 0.0000000001)))
    if `integral' == 1 {
        local candidate = ceil(`candidate' / `total') * `total'
    }
    return scalar n = min(`candidate', `n')
end

program define _gracedr_design_compute_targets, rclass
    syntax , N(real) RATIO(string)
    local arms : word count `ratio'
    local total = 0
    foreach value of local ratio {
        local total = `total' + `value'
    }
    local assigned = 0
    forvalues arm = 1/`arms' {
        local value : word `arm' of `ratio'
        local exact = `n' * (`value' / `total')
        local base`arm' = floor(round(`exact', 0.0000000001))
        local fraction`arm' = round(`exact' - `base`arm'', 0.0000000001)
        local taken`arm' = 0
        local assigned = `assigned' + `base`arm''
    }
    local remainder = `n' - `assigned'
    forvalues turn = 1/`remainder' {
        local best = 0
        local best_fraction = -1
        forvalues arm = 1/`arms' {
            local ahead = !missing(`fraction`arm'') & `fraction`arm'' > `best_fraction'
            if `taken`arm'' == 0 & `ahead' == 1 {
                local best = `arm'
                local best_fraction = `fraction`arm''
            }
        }
        local taken`best' = 1
        local base`best' = `base`best'' + 1
    }
    local targets ""
    forvalues arm = 1/`arms' {
        local targets "`targets' `base`arm''"
    }
    return local targets = strtrim("`targets'")
end

program define _gracedr_design_check_ratio
    args ratio
    foreach value of local ratio {
        local number = real(`"`value'"')
        local positive = !missing(`number') & `number' > 0
        if `positive' == 0 {
            display as error "gracedr: ratio() values must be positive"
            exit 198
        }
    }
end

program define _gracedr_design_runin_size, rclass
    syntax , N(integer) RATIO(numlist)
    _gracedr_design_check_ratio "`ratio'"
    _gracedr_design_compute_runin, n(`n') ratio(`ratio')
    return scalar n = r(n)
end

program define _gracedr_design_targets, rclass
    syntax , N(integer) RATIO(numlist)
    _gracedr_design_check_ratio "`ratio'"
    _gracedr_design_compute_targets, n(`n') ratio(`ratio')
    return local targets `"`r(targets)'"'
end

program define _gracedr_design_checkpoints
    syntax , N(integer) RATIO(numlist) RUNIN(integer) [FORCEDAT(numlist)]
    _gracedr_design_check_ratio "`ratio'"
    _gracedr_design_check_counts, n(`n') runin(`runin') forcedat(`forcedat')
    clear
    local rows = 1
    local has_runin = inrange(`runin', 1, 1e15)
    if `has_runin' == 1 {
        local rows = `rows' + 1
    }
    local forced_count : word count `forcedat'
    local rows = `rows' + `forced_count'
    quietly set obs `rows'
    generate long n = .
    generate str6 type = ""
    generate byte priority = .
    local row = 1
    quietly replace n = `n' in `row'
    quietly replace type = "final" in `row'
    quietly replace priority = 1 in `row'
    if `has_runin' == 1 {
        local row = `row' + 1
        quietly replace n = `runin' in `row'
        quietly replace type = "run-in" in `row'
        quietly replace priority = 2 in `row'
    }
    foreach value of local forcedat {
        local row = `row' + 1
        quietly replace n = `value' in `row'
        quietly replace type = "forced" in `row'
        quietly replace priority = 3 in `row'
    }
    sort n priority
    quietly by n: keep if _n == 1
    drop priority
    local arms : word count `ratio'
    forvalues arm = 1/`arms' {
        quietly generate long target`arm' = .
    }
    local total_rows = _N
    forvalues row = 1/`total_rows' {
        local checkpoint = n[`row']
        _gracedr_design_compute_targets, n(`checkpoint') ratio(`ratio')
        forvalues arm = 1/`arms' {
            local count : word `arm' of `r(targets)'
            quietly replace target`arm' = `count' in `row'
        }
    }
end

program define _gracedr_design_check_name
    args name
    local words : word count `name'
    local single = `words' == 1
    if `single' == 0 {
        display as error `"gracedr: `name' is not a valid Stata variable name"'
        exit 198
    }
    if inlist(`"`name'"', "id", "arm", "code", "seq") {
        display as error `"gracedr: balance variable name `name' is reserved"'
        exit 198
    }
    capture confirm name `name'
    if _rc == 0 {
        exit
    }
    display as error `"gracedr: `name' is not a valid Stata variable name"'
    exit 198
end

program define _gracedr_design_parse_variables, rclass
    syntax , [CATEGORICAL(string asis) CONTINUOUS(string asis)]
    local names ""
    local types ""
    local count = 0
    local remaining `"`categorical'"'
    while !missing(ustrtrim(`"`remaining'"')) {
        gettoken piece remaining : remaining, parse("|")
        if `"`piece'"' == "|" | missing(ustrtrim(`"`piece'"')) {
            continue
        }
        gettoken name levels : piece, parse(":")
        local name = ustrtrim(`"`name'"')
        local has_colon = substr(ustrtrim(`"`levels'"'), 1, 1) == ":"
        if `has_colon' == 0 {
            display as error "gracedr: categorical() entries must look like name: level level"
            exit 198
        }
        local levels = substr(ustrtrim(`"`levels'"'), 2, .)
        local cleaned ""
        foreach level of local levels {
            local level = ustrlower(ustrtrim(`"`level'"'))
            local cleaned `"`cleaned' "`level'""'
        }
        local cleaned = strtrim(`"`cleaned'"')
        local level_count : word count `cleaned'
        if `level_count' < 2 {
            display as error `"gracedr: categorical variable `name' needs at least 2 levels"'
            exit 198
        }
        local repeated : list dups cleaned
        if !missing(`"`repeated'"') {
            display as error ///
                `"gracedr: levels of `name' must be unique after trimming and lower-casing"'
            exit 198
        }
        _gracedr_design_check_name `"`name'"'
        local count = `count' + 1
        local names "`names' `name'"
        local types "`types' categorical"
        local levels`count' `"`cleaned'"'
    }
    foreach name of local continuous {
        _gracedr_design_check_name `"`name'"'
        local count = `count' + 1
        local names "`names' `name'"
        local types "`types' continuous"
        local levels`count' ""
    }
    local repeated : list dups names
    if !missing("`repeated'") {
        display as error "gracedr: balance variable names must be unique"
        exit 198
    }
    if `count' == 0 {
        display as error "gracedr: at least one balance variable is required"
        exit 198
    }
    return scalar nvars = `count'
    return local varnames = strtrim("`names'")
    return local vartypes = strtrim("`types'")
    forvalues position = 1/`count' {
        return local levels`position' `"`levels`position''"'
    }
end

program define _gracedr_design_validate, rclass
    syntax ,                    ///
        N(string)               ///
        ARMS(string asis)       ///
        [                       ///
        RATIO(string)           ///
        WEIGHTS(string)         ///
        PSTAR(string)           ///
        XI(string)              ///
        RUNIN(string)           ///
        RUNINMETHOD(string)     ///
        FORCEDAT(string)        ///
        SIZEBOUND(string)       ///
        CATEGORICAL(string asis) ///
        CONTINUOUS(string asis)  ///
        ]
    _gracedr_design_whole `"`n'"' 2 2000000000
    if r(valid) == 0 {
        display as error "gracedr: n() must be a whole number from 2 to 2000000000"
        exit 198
    }
    local total = r(number)
    local arms : list clean arms
    local arm_count : word count `arms'
    local repeated : list dups arms
    if `arm_count' < 2 | !missing(`"`repeated'"') {
        display as error "gracedr: arms() must be at least 2 unique labels"
        exit 198
    }
    if missing(`"`ratio'"') {
        local ratio ""
        forvalues position = 1/`arm_count' {
            local ratio "`ratio' 1"
        }
    }
    local ratio = strtrim(`"`ratio'"')
    local ratio_count : word count `ratio'
    local counts_match = `ratio_count' == `arm_count'
    if `counts_match' == 0 {
        display as error "gracedr: ratio() must have one value per arm"
        exit 198
    }
    _gracedr_design_check_ratio "`ratio'"
    _gracedr_design_parse_variables, categorical(`categorical') continuous(`continuous')
    local variable_count = r(nvars)
    local varnames `"`r(varnames)'"'
    local vartypes `"`r(vartypes)'"'
    forvalues position = 1/`variable_count' {
        local levels`position' `"`r(levels`position')'"'
    }
    if missing(`"`pstar'"') {
        local pstar 0.30
    }
    local number = real(`"`pstar'"')
    local valid = !missing(`number') & `number' > 0 & `number' < 1
    if `valid' == 0 {
        display as error "gracedr: pstar() must be a number between 0 and 1"
        exit 198
    }
    local pstar_value = `number'
    if missing(`"`xi'"') {
        local xi 0.65
    }
    local number = real(`"`xi'"')
    local valid = !missing(`number') & `number' > 0.5 & `number' < 1
    if `valid' == 0 {
        display as error "gracedr: xi() must be a number between 0.5 and 1"
        exit 198
    }
    local xi_value = `number'
    if missing(`"`sizebound'"') {
        local sizebound 3
    }
    _gracedr_design_whole `"`sizebound'"' 1 1e15
    if r(valid) == 0 {
        display as error "gracedr: sizebound() must be a whole number of at least 1"
        exit 198
    }
    local sizebound_value = r(number)
    local weight_total = 0
    if missing(`"`weights'"') {
        local weights ""
        forvalues position = 1/`variable_count' {
            local weights "`weights' 1"
        }
    }
    local weight_count : word count `weights'
    local counts_match = `weight_count' == `variable_count'
    if `counts_match' == 0 {
        display as error "gracedr: weights() must have one value per balance variable"
        exit 198
    }
    foreach value of local weights {
        local number = real(`"`value'"')
        local positive = !missing(`number') & `number' > 0
        if `positive' == 0 {
            display as error "gracedr: weights() values must be positive"
            exit 198
        }
        local weight_total = `weight_total' + `number'
    }
    local normalized ""
    foreach value of local weights {
        local scaled = real(`"`value'"') / `weight_total'
        local piece = strtrim(string(`scaled', "%21.15g"))
        local normalized "`normalized' `piece'"
    }
    if missing(`"`runin'"') {
        _gracedr_design_compute_runin, n(`total') ratio(`ratio')
        local runin = r(n)
    }
    if !missing(`"`forcedat'"') {
        capture numlist `"`forcedat'"'
        if _rc == 0 {
            local forcedat `r(numlist)'
        }
    }
    _gracedr_design_check_counts, n(`total') runin(`"`runin'"') forcedat(`"`forcedat'"')
    local forced_sorted ""
    if !missing(`"`forcedat'"') {
        numlist `"`forcedat'"', sort
        local forced_sorted `r(numlist)'
        local forced_sorted : list uniq forced_sorted
    }
    if missing(`"`runinmethod'"') {
        local runinmethod tilted
    }
    if !inlist("`runinmethod'", "tilted", "rar") {
        display as error "gracedr: runinmethod() must be tilted or rar"
        exit 198
    }
    return scalar n = `total'
    return local arms `"`arms'"'
    return local ratio `"`ratio'"'
    return local weights = strtrim("`normalized'")
    return scalar pstar = `pstar_value'
    return scalar xi = `xi_value'
    return scalar runin = `runin'
    return local runinmethod "`runinmethod'"
    return local forcedat "`forced_sorted'"
    return scalar sizebound = `sizebound_value'
    return scalar nvars = `variable_count'
    return local varnames `"`varnames'"'
    return local vartypes `"`vartypes'"'
    forvalues position = 1/`variable_count' {
        return local levels`position' `"`levels`position''"'
    }
end
