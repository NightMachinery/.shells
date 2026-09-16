##
function cpu-frequency-darwin {
    sudo powermetrics -n 1 -i 1000 | grep -iE 'thermal pressure|E-Cluster.*freq|P-Cluster.*freq|freq_hz'
    #: Example output on M2:
    # Second underflow occured.
    # E-Cluster HW active frequency: 2423 MHz
    # P-Cluster HW active frequency: 1107 MHz
}
##
