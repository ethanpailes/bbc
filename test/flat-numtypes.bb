
block tcp_header
  source_port : 16ub
  dest_port : 16ub
  seq_no : 32ub
  ack_no : 32ub
  header_len : 4ub
  reserved_bits : 6ub
  code_bits : 6ub
  window : 16ub
  checksum : 16ub
  urgent : 16ub
  options : 32ub
end



