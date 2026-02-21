# JSON Handling

**Searchable keywords**: JSON, serialize, deserialize, /ui2/cl_json, REST API, API response

**CRITICAL**: Always use `/ui2/cl_json` for JSON serialization and deserialization.

## Correct Usage

```abap
" Deserialize JSON to ABAP structure
DATA ls_data TYPE ty_request.
ls_data = /ui2/cl_json=>deserialize( json = lv_json ).

" Serialize ABAP structure to JSON
lv_json = /ui2/cl_json=>serialize( data = ls_response ).
```

## Never Use

- Manual string operations (CONCATENATE, SPLIT, etc.)
- String templates for complex structures
- Direct assignment without /ui2/cl_json

This is enforced by ABAP - manual string operations for JSON parsing will cause type conflicts.
