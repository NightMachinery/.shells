##
function remap-fn-to-f18-darwin {
  #: [[id:5fed1cf2-0ca9-44f4-b10a-98658ed4b094][hdiutil]]
  ##
  ecgray "$0"

  hidutil property --set '{"UserKeyMapping":[
            {
              "HIDKeyboardModifierMappingSrc": 0xFF00000003,
              "HIDKeyboardModifierMappingDst": 0x70000006D
            }
        ]}'
  #: This works for both the internal keyboard and external Magic Keyboards.
}
##
