set proj_dir [get_property DIRECTORY [current_project]]

set build_name "acorn"

puts "INFO: Writing .bit file to: ${proj_dir}/${build_name}.bit"
write_bitstream -force -file "${proj_dir}/${build_name}.bit"

puts "INFO: Writing .bin file to: ${proj_dir}/${build_name}.bin"
write_cfgmem -force -format bin -interface spix4 -size 16 -loadbit "up 0x0 ${proj_dir}/${build_name}.bit" -file "${proj_dir}/${build_name}.bin"

puts "SUCCESS: All files generated in the project directory."
