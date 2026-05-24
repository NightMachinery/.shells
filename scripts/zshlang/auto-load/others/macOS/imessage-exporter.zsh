##
function h-imessage-exporter-contacts-diag-paths {
    local source_root="${imessage_exporter_contacts_diag_source_root:-${HOME}/Library/Application Support/AddressBook/Sources}"
    local contacts_db_name="${imessage_exporter_contacts_diag_contacts_db_name:-AddressBook-v22.abcddb}"

    local -a db_paths=("${source_root}"/*/"${contacts_db_name}"(N.))
    db_paths=("${(@o)db_paths}")

    ec "${(@F)db_paths}"
}

function imessage-exporter-contacts-diag {
    local source_root="${imessage_exporter_contacts_diag_source_root:-${HOME}/Library/Application Support/AddressBook/Sources}"
    local contacts_db_name="${imessage_exporter_contacts_diag_contacts_db_name:-AddressBook-v22.abcddb}"

    ensure-array imessage_exporter_contacts_diag_diagnostics_args
    local -a diagnostics_args=("${imessage_exporter_contacts_diag_diagnostics_args[@]}")
    local -a db_paths=("${@}")

    if (( "${#db_paths[@]}" == 0 )) ; then
        db_paths=("${source_root}"/*/"${contacts_db_name}"(N.))
        db_paths=("${(@o)db_paths}")
    fi

    if (( "${#db_paths[@]}" == 0 )) ; then
        ecerr "$0: no ${contacts_db_name} files found under ${source_root}"
        return 1
    fi

    local db_path
    local diagnostics
    local retcode

    for db_path in "${db_paths[@]}" ; do
        ec "* ${db_path}"

        diagnostics="$(
            imessage-exporter \
                --diagnostics \
                --contacts-path "${db_path}" \
                "${diagnostics_args[@]}" \
                2>&1
        )"
        retcode="$?"

        ec "${diagnostics}"

        if (( retcode != 0 )) ; then
            ecerr "$0: diagnostics failed for ${db_path} with exit code ${retcode}"
        fi

        ec
    done
}
##
