set -exo pipefail
##
function micromamba-setup {
    #: @duplicateCode/0d387fd1a8cb09e91286067c039ffd2a
    #: @noninteractive
    ##
    INIT_YES=n \
        </dev/null bash <(curl -L micro.mamba.pm/install.sh)

    export PATH="${HOME}/.local/bin:${PATH}"
    micromamba config append channels conda-forge --env
    micromamba self-update

    micromamba create --yes -c conda-forge --name p310 python=3.10 numpy 
}

function gost-setup {
    #: @duplicateCode/677fcfc72b5b857e88b4687a1824e9e3
    ##
    mkdir -p ~/bin/
    wget -O - 'https://github.com/ginuerzh/gost/releases/download/v2.11.5/gost-linux-amd64-2.11.5.gz' | gunzip -c > ~/bin/gost

    chmod +x ~/bin/gost
    # sudo setcap 'cap_net_bind_service=+ep' "$(realpath ~/bin/gost)"
}

function tpix-setup {
    #: @duplicateCode/12c0863cd8154859380fdff56d92b647
    ##
    mkdir -p ~/bin/
    wget -O - 'https://github.com/jesvedberg/tpix/releases/download/v1.0.0/tpix-1.0.0-x86_64-linux.tar.gz' | tar -xzf - -C ~/bin tpix
    #: `tpix` at the end specifies the specific file to be extracted from the archive. Only the `tpix` file will be extracted and placed in the `~/bin` directory.

    chmod +x ~/bin/tpix
}
##

command sh -c "$(wget -O- https://raw.githubusercontent.com/romkatv/zsh-bin/master/install)" -- -d ~/.local -e no

mkdir -p ~/.local/bin
curl -sS https://starship.rs/install.sh | sh -s -- -b ~/.local/bin -y

curl -sS https://raw.githubusercontent.com/ajeetdsouza/zoxide/main/install.sh | bash

gost-setup
tpix-setup

micromamba-setup

curl -LsSf https://astral.sh/uv/install.sh | INSTALLER_NO_MODIFY_PATH=1 INSTALLER_PRINT_VERBOSE=1 sh

##

## pip
pip install --upgrade pipx
pipx install speedtest-cli nvitop black
#: nvitop needs the isolated env (?) pipx provides or sth.

pip install -U jupyter jupyterlab py-spy

pip install -U pynight IPython aiofile docopt PySocks telethon python-telegram-bot
##
