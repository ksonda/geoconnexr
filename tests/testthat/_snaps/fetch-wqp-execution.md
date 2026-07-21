# M7k plans one bounded WQP request without host activity

    Code
      str(list(policy = first$policy, request = first$request))
    Output
      List of 2
       $ policy :List of 27
        ..$ slice_id                 : chr "wqp_single_response_v1"
        ..$ handler_id               : chr "wqp"
        ..$ implementation_id        : chr "geoconnexr:dataRetrieval-wqp"
        ..$ implementation_package   : chr "dataRetrieval"
        ..$ implementation_symbol    : chr "importWQP"
        ..$ method                   : chr "GET"
        ..$ service                  : chr "Result"
        ..$ profile                  : chr "narrowResult"
        ..$ accept                   : chr "text/csv"
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
        ..$ response_media_types     : chr [1:2] "text/csv" "text/plain"
        ..$ response_content_encoding: chr "identity"
        ..$ parser_encoding          : chr "UTF-8"
        ..$ type_inference           : chr "disabled"
        ..$ attribute_policy         : chr "disabled"
        ..$ pagination_policy        : chr "single_response_no_follow"
        ..$ max_fields               : int 1000
       $ request:List of 22
        ..$ logical_request_id    : chr "c930977a03e3480776b008a53fb257eda652511f0d87742614f0d01db3c169bb"
        ..$ reservation_id        : chr "dee18e18436076c2c1703e6b6f1b021f4045524d99bef951eaa817f9d8402ff5"
        ..$ distribution_id       : chr "3fb5040a217e10d94e4f773c101abaca1ce4035a1b647fd1110b21c76a22b9e0"
        ..$ fetch_order           : int 4
        ..$ service               : chr "Result"
        ..$ profile               : chr "narrowResult"
        ..$ site_id               : chr "USGS-01234567"
        ..$ characteristic_name   : chr ""
        ..$ characteristic_status : chr "not_supplied"
        ..$ time_start            : chr "2025-06-01T00:00:00Z"
        ..$ time_end              : chr "2025-06-30T23:59:59Z"
        ..$ start_date            : chr "06-01-2025"
        ..$ end_date              : chr "06-30-2025"
        ..$ source_url_redacted   : chr "https://www.waterqualitydata.us/data/Result/search?[redacted]"
        ..$ canonical_url_redacted: chr "https://www.waterqualitydata.us/data/Result/search?[redacted]"
        ..$ max_physical_attempts : int 1
        ..$ max_encoded_bytes     : num 20000
        ..$ max_decoded_bytes     : num 20000
        ..$ response_byte_limit   : num 20000
        ..$ max_rows              : int 10000
        ..$ max_columns           : int 100
        ..$ request_status        : chr "wqp_request_planned"

---

    Code
      print(first)
    Message
      <gx_wqp_request_plan>
      * Service/profile: Result/narrowResult
      * Site: USGS-01234567; characteristic: all characteristics
      * Reserved attempts: 1; requests executed: 0
