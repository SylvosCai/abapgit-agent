---
layout: default
title: CDS Testing
nav_order: 17
parent: ABAP Coding Guidelines
grand_parent: ABAP Development
---

# CDS Testing

### Use CDS Test Double Framework for CDS View Tests

**When creating unit tests for CDS views, use the CDS Test Double Framework (`CL_CDS_TEST_ENVIRONMENT`).**

```
❌ WRONG: Use regular AUnit test class without test doubles
✅ CORRECT: Use CL_CDS_TEST_ENVIRONMENT to create test doubles for CDS views
```

**Why**: CDS views read from database tables. Using test doubles allows:
- Injecting test data without affecting production data
- Testing specific scenarios that may not exist in production
- Fast, isolated tests that don't depend on database state

See `guidelines/testing.md` for code examples.
