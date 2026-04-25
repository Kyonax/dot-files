---
id: index-pj-mrd-images-tracking
title: Images & Tracking Index
category: images-tracking
scope: project/mr-dotcom
---

# Images & Tracking

Rules governing image rendering with ImgBox, CMS image sourcing, skeleton loading, and analytics event tracking patterns in Madison Reed Vue components.

## Rules

| ID | File | Title | Severity |
|---|---|---|---|
| rule-pj-mrd-040 | imgbox-always.md | Use ImgBox for All Images | HIGH |
| rule-pj-mrd-041 | skeleton-imgbox.md | Skeleton Loading via ImgBox Deep Selector | MEDIUM |
| rule-pj-mrd-042 | deep-img-aspect.md | Deep img Selector for Aspect and Shape | MEDIUM |
| rule-pj-mrd-043 | tracking-function.md | Correct Tracking Function Selection | MEDIUM |
| rule-pj-mrd-044 | cms-svg-imgbox.md | CMS SVG Icons via ImgBox | MEDIUM |
| rule-pj-mrd-045 | no-static-images.md | No Static Images in Repo | HIGH |
| rule-pj-mrd-046 | experiment-exposure.md | Explicit A/B Experiment Exposure Tracking | MEDIUM |
| rule-pj-mrd-047 | double-tracking.md | Avoid Double Tracking | MEDIUM |

## Quick Reference

- **imgbox-always** -- All images must use ImgBox component, no raw img tags allowed
- **skeleton-imgbox** -- Skeleton placeholder via :deep(.image-box) background-color ui-color-4 loading state
- **deep-img-aspect** -- :deep(img) for aspect-ratio, border-radius, object-fit styling on ImgBox rendered images
- **tracking-function** -- trackMREvent (stay on page) vs trackMREventAndRedirect (hard navigation redirect)
- **cms-svg-imgbox** -- CMS-hosted SVG icons rendered via ImgBox not mr-icon, isNewSvg auto-detected
- **no-static-images** -- No PNG/JPG/GIF in repo, all raster images from CMS, only SVG icons in assets
- **experiment-exposure** -- A/B experiment exposure must be tracked explicitly with dedicated event, not just conditional branching
- **double-tracking** -- Avoid duplicate tracking events when helpers like openSignInModal already track internally
