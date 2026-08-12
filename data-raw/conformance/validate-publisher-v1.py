#!/usr/bin/env python3
"""Validate the shared publisher-v1 corpus with the Python standard library."""

from __future__ import annotations

import hashlib
import html
import json
from pathlib import Path
from typing import Any


REPOSITORY = Path(__file__).resolve().parents[2]
CORPUS = REPOSITORY / "inst" / "conformance" / "publisher-v1"
PROFILE_IRI = "https://ksonda.github.io/geoconnexr/profiles/publisher-v1"
PROFILE_VERSION = "1.0.0"
CONTEXT = {
    "schema": "https://schema.org/",
    "hyf": "https://www.opengis.net/def/schema/hy_features/hyf/",
    "geo": "http://www.opengis.net/ont/geosparql#",
    "gx": "https://ksonda.github.io/geoconnexr/vocab/",
}
APPROVED_LITERALS = {"Unknown", "hydrometricStation"}


def read_json(name: str) -> Any:
    return json.loads((CORPUS / name).read_text(encoding="utf-8"))


def sha256(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def canonical_json(value: Any) -> bytes:
    return json.dumps(
        value,
        ensure_ascii=False,
        allow_nan=False,
        separators=(",", ":"),
    ).encode("utf-8")


def provider_node(provider: dict[str, Any]) -> dict[str, Any]:
    return {
        "@id": provider["uri"],
        "schema:name": provider["name"],
        "schema:url": provider["url"],
    }


def variable_node(variable: dict[str, Any]) -> dict[str, Any]:
    node: dict[str, Any] = {"@id": variable["variable_uri"]}
    mappings = (
        ("schema:name", "variable_name"),
        ("schema:unitCode", "unit_uri"),
        ("schema:unitText", "unit_label"),
        ("schema:measurementTechnique", "measurement_technique"),
    )
    for term, field in mappings:
        if variable.get(field) is not None:
            node[term] = variable[field]
    return node


def distribution_node(distribution: dict[str, Any]) -> dict[str, Any]:
    node: dict[str, Any] = {
        "@type": "schema:DataDownload",
        "schema:contentUrl": distribution["distribution_url"],
        "schema:encodingFormat": distribution["media_type"],
    }
    if distribution["conforms_to"]:
        node["schema:conformsTo"] = distribution["conforms_to"]
    return node


def dataset_node(dataset: dict[str, Any], provider: dict[str, Any]) -> dict[str, Any]:
    node: dict[str, Any] = {
        "@type": "schema:Dataset",
        "@id": dataset["dataset_uri"],
        "schema:name": dataset["dataset_name"],
        "schema:description": dataset["dataset_description"],
        "schema:temporalCoverage": dataset["temporal_coverage"],
        "schema:license": dataset["license"],
        "schema:accessRights": dataset["access_rights"],
        "schema:provider": provider_node(provider),
    }
    node["schema:distribution"] = [
        distribution_node(value)
        for value in sorted(
            dataset["distributions"], key=lambda value: value["distribution_id"]
        )
    ]
    node["schema:variableMeasured"] = [
        variable_node(value)
        for value in sorted(
            dataset["variables"], key=lambda value: value["variable_id"]
        )
    ]
    return node


def site_node(
    site: dict[str, Any], datasets: list[dict[str, Any]], provider: dict[str, Any]
) -> dict[str, Any]:
    node: dict[str, Any] = {"@id": site["site_uri"]}
    location_type = site["site_type"]
    types = ["hyf:HY_HydrometricFeature"]
    if location_type.startswith(("http://", "https://")):
        types.append(location_type)
    elif location_type in APPROVED_LITERALS:
        node["hyf:HY_HydroLocationType"] = location_type
    else:
        raise ValueError("unsupported location type")
    node["@type"] = types
    node["schema:name"] = site["name"]
    node["schema:description"] = site["description"]
    node["schema:provider"] = provider_node(provider)
    coordinates = site["geometry"]["coordinates"]
    x, y = (format(value, "g") for value in coordinates)
    node["geo:hasGeometry"] = {
        "@type": "geo:Geometry",
        "geo:asWKT": f"POINT ({x} {y})",
        "geo:crs": "http://www.opengis.net/def/crs/OGC/1.3/CRS84",
    }
    node["hyf:referencedPosition"] = {
        "@type": "hyf:HY_IndirectPosition",
        "hyf:linearElement": {"@id": site["mainstem_uri"]},
    }
    selected = sorted(
        (value for value in datasets if value["site_uri"] == site["site_uri"]),
        key=lambda value: value["dataset_id"],
    )
    if selected:
        node["schema:subjectOf"] = [
            dataset_node(value, provider) for value in selected
        ]
    return node


def build_profile(source: dict[str, Any]) -> dict[str, Any]:
    if source["profile_version"] != PROFILE_VERSION:
        raise ValueError("unsupported input profile version")
    provider = source["provider"]
    return {
        "@context": CONTEXT,
        "gx:profile": PROFILE_IRI,
        "gx:profileVersion": PROFILE_VERSION,
        "@graph": [
            site_node(site, source["datasets"], provider)
            for site in sorted(source["sites"], key=lambda value: value["site_uri"])
        ],
    }


def finding(pointer: str, rule_id: str, message: str, fix: str) -> dict[str, str]:
    return {
        "severity": "error",
        "json_pointer": pointer,
        "rule_id": rule_id,
        "profile_version": PROFILE_VERSION,
        "message": message,
        "suggested_fix": fix,
    }


def validate_known_rules(profile: dict[str, Any]) -> list[dict[str, str]]:
    findings: list[dict[str, str]] = []
    if profile.get("gx:profileVersion") != PROFILE_VERSION:
        findings.append(
            finding(
                "/gx:profileVersion",
                "profile.version",
                "The document does not declare publisher profile version 1.0.0.",
                "Set gx:profileVersion to 1.0.0 and retain the gx prefix mapping.",
            )
        )
    for index, site in enumerate(profile.get("@graph", [])):
        pointer = f"/@graph/{index}"
        location_type = site.get("hyf:HY_HydroLocationType")
        if location_type is not None and location_type not in APPROVED_LITERALS:
            findings.append(
                finding(
                    f"{pointer}/hyf:HY_HydroLocationType",
                    "site.location_type",
                    "The site location type is not a canonical IRI or an approved literal.",
                    "Use a canonical location-type IRI, hydrometricStation, or Unknown.",
                )
            )
        provider = site.get("schema:provider", {})
        if not provider.get("schema:name"):
            findings.append(
                finding(
                    f"{pointer}/schema:provider/schema:name",
                    "site.provider_name",
                    "The site provider does not have a name.",
                    "Add a nonempty schema:name to the provider object.",
                )
            )
    return sorted(
        findings,
        key=lambda value: (
            {"error": 0, "warning": 1, "info": 2}[value["severity"]],
            value["json_pointer"],
            value["rule_id"],
        ),
    )


def build_sitemap(uris: list[str]) -> bytes:
    lines = [
        '<?xml version="1.0" encoding="UTF-8"?>',
        '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">',
    ]
    for uri in sorted(uris):
        lines.extend(("  <url>", f"    <loc>{html.escape(uri, quote=False)}</loc>", "  </url>"))
    lines.append("</urlset>")
    return ("\n".join(lines) + "\n").encode("utf-8")


def verify_manifest(manifest: dict[str, Any]) -> None:
    declared = {resource["path"] for resource in manifest["resources"]}
    actual = {
        path.name
        for path in CORPUS.iterdir()
        if path.is_file() and path.name not in {"README.md", "manifest.json"}
    }
    if actual != declared:
        raise AssertionError(f"resource inventory mismatch: {actual ^ declared}")
    for resource in manifest["resources"]:
        data = (CORPUS / resource["path"]).read_bytes()
        if len(data) != resource["bytes"]:
            raise AssertionError(f"byte mismatch: {resource['path']}")
        if sha256(data) != resource["sha256"]:
            raise AssertionError(f"digest mismatch: {resource['path']}")


def main() -> None:
    manifest = read_json("manifest.json")
    verify_manifest(manifest)

    source = read_json("input.json")
    expected_profile = read_json("expected-profile.json")
    built = build_profile(source)
    if built != expected_profile:
        raise AssertionError("Python profile result differs from the shared known answer")
    if sha256(canonical_json(built)) != manifest["known_answers"]["profile_canonical_sha256"]:
        raise AssertionError("canonical profile digest differs from the known answer")

    invalid = read_json("invalid-profile.json")
    expected_findings = read_json("expected-findings.json")
    if validate_known_rules(invalid) != expected_findings:
        raise AssertionError("Python findings differ from the shared known answer")

    sitemap = build_sitemap(read_json("sitemap-uris.json"))
    if sitemap != (CORPUS / "expected-sitemap.xml").read_bytes():
        raise AssertionError("Python sitemap differs from the shared known answer")
    if sha256(sitemap) != manifest["known_answers"]["sitemap_sha256"]:
        raise AssertionError("sitemap digest differs from the known answer")

    print("publisher-v1 conformance: 6 resources, 3 findings, all known answers pass")


if __name__ == "__main__":
    main()
