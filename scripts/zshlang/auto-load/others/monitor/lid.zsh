function lid-angle-darwin {
    python -c '
from pybooklid import read_lid_angle

angle = read_lid_angle() or -1
print(angle)
'
}
aliasfn lid-angle lid-angle-darwin
