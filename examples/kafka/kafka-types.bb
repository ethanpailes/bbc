
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
  hdr : request_message_hdr
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
