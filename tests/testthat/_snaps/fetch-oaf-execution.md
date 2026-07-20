# M7i plans one bounded OGC API Features request without host activity

    Code
      str(list(policy = first$policy, request = first$request))
    Output
      List of 2
       $ policy :List of 22
        ..$ slice_id                 : chr "ogc_api_features_single_page_v1"
        ..$ handler_id               : chr "ogc_api_features"
        ..$ implementation_id        : chr "geoconnexr:native-oaf"
        ..$ implementation_package   : chr "geoconnexr"
        ..$ implementation_symbol    : chr "gx_handler_oaf"
        ..$ method                   : chr "GET"
        ..$ accept                   : chr "application/geo+json, application/json;q=0.9"
        ..$ accept_encoding          : chr "identity"
        ..$ body_bytes               : int 0
        ..$ body_sha256              : chr "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
        ..$ credential_policy        : chr "source_url_opaque_no_additional_credentials"
        ..$ redirect_policy          : chr "reject"
        ..$ max_redirects            : int 0
        ..$ retry_policy             : chr "none"
        ..$ max_retries              : int 0
        ..$ max_physical_attempts    : int 1
        ..$ cache_policy             : chr "bypass"
        ..$ success_status           : int 200
        ..$ response_media_types     : chr [1:2] "application/geo+json" "application/json"
        ..$ response_content_encoding: chr "identity"
        ..$ pagination_policy        : chr "single_page_no_follow"
        ..$ limit                    : int 2
       $ request:List of 12
        ..$ logical_request_id    : chr "ba983218b3b114e5c397fb51f3a8b6765dd37c35181457a60f66d7641b1aa941"
        ..$ reservation_id        : chr "cafe1a872b881d229a05672a03229e20757d24de3b1c4eceac6fe90b79c9e469"
        ..$ distribution_id       : chr "3093dce1f822cf25e628600f6f81a78d85471456bfb255fedc5b709aa884fcdc"
        ..$ fetch_order           : int 5
        ..$ collection_id         : chr "gages"
        ..$ source_url_redacted   : chr "https://reference.geoconnex.us/collections/gages/items"
        ..$ canonical_url_redacted: chr "https://reference.geoconnex.us/collections/gages/items?[redacted]"
        ..$ max_physical_attempts : int 1
        ..$ max_encoded_bytes     : num 20000
        ..$ max_decoded_bytes     : num 20000
        ..$ response_byte_limit   : num 20000
        ..$ request_status        : chr "oaf_request_planned"

---

    Code
      print(first)
    Message
      <gx_oaf_request_plan>
      * Collection: gages; limit: 2
      * Reserved attempts: 1; requests executed: 0
      * Transport authorized: FALSE; runtime symbol check: pending
