
block wrapper
  msg : array 32sb 8u
end

block bytes
  b : array 16sb 8u
end

block string
  s : array 16sb 8u
end



block request_message_hdr
  api_key : 16sb
  api_version : 16sb
  correlation_id : 32sb
  client_id : string
end

block metadata_request
  topics : array 32sb string
end



block broker
  node_id : 32sb
  host : string
  port : 32sb
end

block partition_metadatum
  partition_error_code : 16sb
  partition_id : 32sb
  leader : 32sb
  replicas : array 32sb 32sb
  isr : array 32sb 32sb
end

block topic_metadatum
  topic_error_code : 16sb
  topic_name : string
  partition_metadata : array 32sb partition_metadatum
end

block metadata_response
  correlation_id : 32sb
  brokers : array 32sb broker
  topic_metadata : array 32sb topic_metadatum
end




block message
  offset : 64sb
  message_size : 32sb
  crc : 32sb
  magic_byte : 8s
  attributes : 8s
  key : bytes
  value : bytes
end

block preq_part
  partition : 32sb
  messages : array 32sb message
end

block preq_topic
  topic_name : string
  partitions : array 32sb preq_part
end

block produce_request
  required_acks : 16sb
  timeout : 32sb
  topics : array 32sb preq_topic
end



block pres_part
  partition_no : 32sb
  partition_error_code : 16sb
  offset : 64sb
end

block pres_topic
  topic_name : string
  partitions : array 32sb preq_part
end

block produce_response
  topics : array 32sb pres_topic
end
