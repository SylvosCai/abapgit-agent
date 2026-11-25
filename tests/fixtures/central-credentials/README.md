# Local TLS fixture

This self-signed localhost certificate and **publicly committed test-only key** are used by `central-connection-state.test.js` to verify that central-mode HTTP/ADT clients reject an untrusted TLS endpoint before sending credentials. They are not production credentials and must never be used outside tests. The legacy comparison intentionally preserves the existing unchecked-TLS behavior.
