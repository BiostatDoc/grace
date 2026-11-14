program define gracedr_codes, rclass
    version 19
    syntax using/, ARMS(string asis)
    preserve
    _gracedr_codes using `"`using'"', arms(`arms')
    return scalar n = r(n)
    restore, not
end
