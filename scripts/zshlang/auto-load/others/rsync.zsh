alias rsp-dl='rsync --protect-args --human-readable --xattrs --times --info=progress2 --append -r' # append assumes files only grow and do not otherwise change
alias rsp-safe='rsync --checksum --protect-args --human-readable --xattrs --times --info=progress2 --partial-dir=.rsync-partial -r' # partial-dir supports resume
alias rsp='rsp-safe --delete-after --force-delete' #--ignore-errors will delete even if there are IO errors on sender's side.
alias rspm='rsp --crtimes'
alias rspb='rsp --backup --backup-dir=.rsync-backup'
alias rspbm='rspb --crtimes'
