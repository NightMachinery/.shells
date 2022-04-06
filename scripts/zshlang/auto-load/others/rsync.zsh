aliasfn rsp-dl rsync --protect-args --human-readable --xattrs --times --info=progress2 --append -r # append assumes files only grow and do not otherwise change
aliasfn rsp-safe rsync --checksum --protect-args --human-readable --xattrs --times --info=progress2 --partial-dir=.rsync-partial -r # partial-dir supports resume
aliasfn rsp rsp-safe --delete-after --force-delete #--ignore-errors will delete even if there are IO errors on sender's side.
aliasfn rspm rsp --crtimes
aliasfn rspb rsp --backup --backup-dir=.rsync-backup
aliasfn rspbm rspb --crtimes
