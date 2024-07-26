#!/usr/bin/env -S sudo perl

use strict;
use warnings;
use File::Temp qw/ tempfile /;
use File::Copy qw/ move /;
use File::Basename qw/ dirname /;
use Fcntl qw/ :flock /;

my $ssh_config = '/etc/ssh/sshd_config';
my $backup_file = "$ssh_config.bak";

system("cp", $ssh_config, $backup_file) == 0
    or die "Failed to create backup: $!";

my ($fh, $temp_file) = tempfile(DIR => dirname($ssh_config));

open(my $lock_fh, '<', $ssh_config) or die "Cannot open $ssh_config: $!";
flock($lock_fh, LOCK_EX) or die "Cannot lock $ssh_config: $!";

open(my $ssh_fh, '<', $ssh_config) or die "Cannot open $ssh_config: $!";
while (<$ssh_fh>) {
    print $fh $_ unless /^\s*(?:#\s*)?(?:PasswordAuthentication|KbdInteractiveAuthentication|PubkeyAuthentication)\s+/;
}
close($ssh_fh);

print $fh "PasswordAuthentication no\n";
print $fh "KbdInteractiveAuthentication no\n";
print $fh "PubkeyAuthentication yes\n";

move($temp_file, $ssh_config) or die "Failed to update $ssh_config: $!";

flock($lock_fh, LOCK_UN);
close($lock_fh);

chmod 0644, $ssh_config or die "Failed to set permissions on $ssh_config: $!";

if (system("sshd -t") == 0) {
    print "SSH password authentication and keyboard-interactive authentication disabled, public key authentication enabled.\n";
    print "SSH configuration is valid. Restarting SSH service...\n";

    my $restart_command;
    if (system("systemctl is-active --quiet sshd") == 0) {
        $restart_command = "systemctl restart sshd";
    } elsif (system("systemctl is-active --quiet ssh") == 0) {
        $restart_command = "systemctl restart ssh";
    } else {
        # Fallback to a more generic method if systemctl doesn't work
        $restart_command = "service ssh restart || service sshd restart";
    }

    system($restart_command) == 0
        or die "Failed to restart SSH service: $!";

    print "SSH service has been restarted.\n";
    unlink $backup_file;
} else {
    print "SSH configuration is invalid. Restoring backup...\n";
    move($backup_file, $ssh_config) or die "Failed to restore backup: $!";
    exit 1;
}

print "Operation completed successfully.\n";
