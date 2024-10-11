# host=t21
# host=t31
# host=c0
host=pino
# host=m17-hpc
# host=m15-hpc
# host=mmd1

# fullhost=$user@$host
fullhost=$host

rsp-safe --rsync-path="mkdir -p ~/base/bootstrap && rsync" "${nightNotesPublic}"/cheatsheets/OS/unix/tangled/*(D) ${fullhost}:base/bootstrap/

rsp-safe ~/.tmux.conf ~/.inputrc "${NIGHTDIR}/setup/minimal_proxy"/**/*.(el|zsh|zshenv|zshrc|bash_profile|bashrc|bash|sh|curlrc)(D) ${fullhost}:

rsp-safe "$nightNotesPrivate"/configs/minse/{.private.env.zsh,.secret-private-config.el} ${fullhost}:

rsp-safe --rsync-path="mkdir -p ~/bin && rsync" "$NIGHTDIR/python/telegram-send/tsend.py" "$NIGHTDIR/perllang/ssh_password_auth_disable.pl" ${fullhost}:bin/

rsp-safe2 ${fullhost}:bin/ $NIGHTDIR/perllang/url_dir_count.pl
