# Reviewed vocabulary assets

`unit-conversions-v1.csv` is a directed ruleset. Apply a row only when the input
unit URI exactly equals `from_unit_uri` and the requested output URI exactly
equals `to_unit_uri`:

```text
converted_value = original_value * scale + offset
```

Rules are never inferred from labels. Implementations must reject a conversion
whose dimensions differ and retain the original value/unit plus `rule_id`.
Forward and reverse rules are both explicit because affine conversions cannot be
reversed by taking only the reciprocal scale.

The QUDT URI identifies each unit; the source URL records the reviewed conversion
definition. A future change to a scale, offset, identifier, or status is a
versioned vocabulary change and must alter the snapshot vocabulary hash.

`target-units-v1.csv` is the reviewed selection vocabulary used by
`gx_target_units()`. It records the admitted URI and display label choices for
each conversion dimension and exactly one default per dimension. Selecting a
target never authorizes a conversion by itself: `gx_harmonize()` still requires
an unambiguous catalog variable/unit URI, exact native-label corroboration, and
one directed reviewed rule. A provider label alone is never promoted to a unit
URI.

`wqp-timezones-v1.csv` freezes the active EPA WQX `TimeZoneCode` values reviewed
for WQP harmonization and their service-published fixed offsets in minutes.
Retired aliases are excluded. The code is used exactly as reported; the package
does not infer a timezone from site geography, replace standard/daylight codes,
or apply a regional daylight-saving calendar. Unknown codes keep the entire WQP
resource native-only.
