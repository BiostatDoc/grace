program define _gracedr_utc, rclass
    version 19
    local system_root : environment SYSTEMROOT
    local launcher "`system_root'/System32/WindowsPowerShell/v1.0/powershell.exe"
    if fileexists("`macval(launcher)'") == 0 {
        display as error "gracedr: PowerShell was not found at `macval(launcher)'"
        exit 601
    }
    tempfile stamp
    local flags "-NoProfile -Command"
    local clock "[DateTimeOffset]::UtcNow.ToUnixTimeMilliseconds()"
    local redirect = char(62)
    quietly shell "`macval(launcher)'" `flags' "`clock'" `redirect' "`macval(stamp)'"
    local reading ""
    tempname handle
    capture file open `handle' using "`macval(stamp)'", read text
    if _rc == 0 {
        file read `handle' reading
        file close `handle'
    }
    local milliseconds = real(strtrim(`"`macval(reading)'"'))
    if missing(`milliseconds') {
        display as error "gracedr: could not read the UTC clock from PowerShell"
        exit 601
    }
    return scalar time = `milliseconds' + tc(01jan1970 00:00:00)
end
