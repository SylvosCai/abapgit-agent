# health Command

Check if the ABAP REST API is healthy and accessible.

## Command

```bash
abapgit-agent health
```

## Prerequisite

- `.abapGitAgent` file exists with valid credentials

## Version Check

The health command checks compatibility between CLI and ABAP agent versions.

If versions don't match, a warning is displayed but doesn't block the operation.

## Output

### Success

```json
{
  "status": "OK",
  "version": "1.x.x"
}
```

### Error

```
❌ Health check failed: <error message>
```

## Example

```bash
$ abapgit-agent health
{
  "status": "OK",
  "version": "1.6.0"
}
```
