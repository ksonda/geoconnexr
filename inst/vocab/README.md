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
