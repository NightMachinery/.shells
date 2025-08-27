function ubuntu-version {
    cat /etc/os-release
    #lsb_release -a
    echo
    uname -r # kernel
}

function ubuntu-upgrade {
    sudo apt update --fix-missing
    sudo DEBIAN_FRONTEND='noninteractive' apt-get --fix-missing -y -o Dpkg::Options::='--force-confdef' -o Dpkg::Options::='--force-confold' upgrade

    sudo DEBIAN_FRONTEND='noninteractive' apt-get --fix-missing -y -o Dpkg::Options::='--force-confdef' -o Dpkg::Options::='--force-confold' dist-upgrade

    sudo DEBIAN_FRONTEND='noninteractive' apt-get --fix-missing -y -o Dpkg::Options::='--force-confdef' -o Dpkg::Options::='--force-confold' autoremove

    sudo DEBIAN_FRONTEND='noninteractive' apt-get --fix-missing -y -o Dpkg::Options::='--force-confdef' -o Dpkg::Options::='--force-confold' clean

    sudo DEBIAN_FRONTEND='noninteractive' apt-get --fix-missing -y -o Dpkg::Options::='--force-confdef' -o Dpkg::Options::='--force-confold' autoclean
    
    ecerr "$0:"' Use `sudo do-release-upgrade # -d forces a develop upgrade`
    @warn Upgrading has a lot of interactive prompts:
    https://stackoverflow.com/questions/62192742/noninteractive-do-release-upgrade-with-old-configs-by-default

    You can use `do-release-upgrade -f DistUpgradeViewNonInteractive`, which replaces old config files by the new default files. This seems to also severely reduce the log verbosity.
'
}
