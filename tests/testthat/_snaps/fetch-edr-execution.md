# M7l plans one bounded EDR position request without host activity

    Code
      str(list(policy = first$policy, request = first$request))
    Output
      List of 2
       $ policy :List of 31
        ..$ slice_id                 : chr "edr_position_single_response_v1"
        ..$ handler_id               : chr "edr"
        ..$ implementation_id        : chr "geoconnexr:edr4r"
        ..$ implementation_package   : chr "edr4r"
        ..$ minimum_version          : chr "0.1.1"
        ..$ query_symbol             : chr "edr_position"
        ..$ normalizer_symbol        : chr "covjson_to_tibble"
        ..$ method                   : chr "GET"
        ..$ query_type               : chr "position"
        ..$ response_format          : chr "CoverageJSON"
        ..$ accept                   : chr "application/prs.coverage+json, application/json;q=0.9"
        ..$ accept_encoding          : chr "identity"
        ..$ body_bytes               : int 0
        ..$ body_sha256              : chr "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
        ..$ credential_policy        : chr "source_query_allowlist_no_additional_credentials"
        ..$ redirect_policy          : chr "reject"
        ..$ max_redirects            : int 0
        ..$ retry_policy             : chr "none"
        ..$ max_retries              : int 0
        ..$ max_physical_attempts    : int 1
        ..$ cache_policy             : chr "bypass"
        ..$ success_status           : int 200
        ..$ response_media_types     : chr [1:3] "application/prs.coverage+json" "application/vnd.cov+json" "application/json"
        ..$ response_content_encoding: chr "identity"
        ..$ parser_encoding          : chr "UTF-8"
        ..$ type_inference           : chr "bounded_position_subset"
        ..$ attribute_policy         : chr "disabled"
        ..$ pagination_policy        : chr "single_response_no_follow"
        ..$ max_fields               : int 1000
        ..$ max_json_depth           : int 32
        ..$ max_json_members         : int 3024
       $ request:List of 25
        ..$ logical_request_id    : chr "0a53beee506c049be41ff19466ca3d672d565978da78a472666620da1f058fe8"
        ..$ reservation_id        : chr "b65e0f002363496aa776bda7bfa4d201ee7a4dc8e3f8dbafb1b9192b6fde6e27"
        ..$ distribution_id       : chr "3093dce1f822cf25e628600f6f81a78d85471456bfb255fedc5b709aa884fcdc"
        ..$ fetch_order           : int 5
        ..$ base_url_redacted     : chr "https://edr.example.org/api"
        ..$ collection_id         : chr "streamflow"
        ..$ query_type            : chr "position"
        ..$ coords_wkt            : chr "POINT(-77.5 38.9)"
        ..$ longitude             : num -77.5
        ..$ latitude              : num 38.9
        ..$ parameter_name        : chr "discharge"
        ..$ time_start            : chr "2025-06-01T00:00:00Z"
        ..$ time_end              : chr "2025-06-30T23:59:59Z"
        ..$ datetime              : chr "2025-06-01T00:00:00Z/2025-06-30T23:59:59Z"
        ..$ crs                   : chr "CRS84"
        ..$ response_format       : chr "CoverageJSON"
        ..$ source_url_redacted   : chr "https://edr.example.org/api/collections/streamflow/position?[redacted]"
        ..$ canonical_url_redacted: chr "https://edr.example.org/api/collections/streamflow/position?[redacted]"
        ..$ max_physical_attempts : int 1
        ..$ max_encoded_bytes     : num 20000
        ..$ max_decoded_bytes     : num 20000
        ..$ response_byte_limit   : num 20000
        ..$ max_rows              : int 10000
        ..$ max_columns           : int 100
        ..$ request_status        : chr "edr_request_planned"

---

    Code
      print(first)
    Message
      <gx_edr_request_plan>
      * Query: position; collection: streamflow
      * Parameter: discharge; format: CoverageJSON
      * Reserved attempts: 1; requests executed: 0
