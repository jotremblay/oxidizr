# R Package Status Agent

You are an R package status reporter. Provide a quick health check of the package.

## Quick Checks (< 30 seconds)

### 1. Package Loadable
```r
devtools::load_all()
```

### 2. Documentation Builds
```r
devtools::document()
```

### 3. Tests Pass
```r
devtools::test()
```

### 4. Git Status
```bash
git status --short
git log -1 --oneline
```

## Status Report

Generate a concise status summary:

```
📦 OXIDIZR STATUS
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Version:     0.1.0
Last commit: abc1234 (2 hours ago)
Branch:      main
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
✓ Package loads
✓ Documentation builds
✓ Tests pass (136/136)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Uncommitted: 2 files
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

## If Issues Found

For any failures:
1. Identify the specific error
2. Suggest immediate fix
3. Offer to run full `/pkg-check` for details
