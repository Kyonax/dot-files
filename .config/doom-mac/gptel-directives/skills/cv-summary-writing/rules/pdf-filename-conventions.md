---
title: Recruiter-Facing CV PDF Filename Conventions
impact: MEDIUM
impactDescription: Filename conventions for CV PDFs that recruiters download from a candidate's personal website. Poor naming (snake_case, abbreviated middle initial, generic 'cv.pdf') reads casual and unprofessional. Proper naming adds an ATS keyword hit before content is even parsed.
tags: cv, pdf, filename, recruiter, download-attribute, locale-agnostic, ats, naming-conventions, professional-branding, hero-component, vue, react
---

This rule defines the filename conventions for CV PDFs served from a candidate's personal website. The filename is the first impression a recruiter has after downloading — it shows in their Downloads folder, in their candidate-tracking system, and (with some ATS parsers like Workday) is parsed for the candidate's name and title.

## The Convention

### On-disk filename (for code imports and storage)

```
[Name-with-hyphens]-[Senior-Tier-Title]-[Locale].pdf
```

Examples:
- `Cristian-Moreno-Senior-Software-Engineer-EN.pdf`
- `Cristian-Moreno-Senior-Software-Engineer-ES.pdf`
- `Jane-Doe-Staff-Engineer-EN.pdf`
- `Alex-Kim-Senior-Backend-Engineer-EN.pdf`

### Download attribute filename (what recruiters see in their Downloads folder)

```
[Name-with-hyphens]-[Senior-Tier-Title]-CV.pdf
```

**Locale-agnostic** — the same filename for both EN and ES downloads. The page locale signals which language; the filename doesn't need to.

Examples:
- `Cristian-Moreno-Senior-Software-Engineer-CV.pdf` (same for EN and ES downloads)
- `Jane-Doe-Staff-Engineer-CV.pdf`
- `Alex-Kim-Senior-Backend-Engineer-CV.pdf`

## Naming Rules

| Rule | Why |
|---|---|
| **Hyphens, not underscores** | Hyphens read as word separators in URLs and filenames; underscores look like programmer notation |
| **PascalCase per word** | `Senior-Software-Engineer` not `senior-software-engineer`; capitalization signals professional formatting |
| **Name first, title second** | Recruiters file by surname; surname-first is also acceptable but name-first is more universal |
| **"Senior Software Engineer" is the most general senior IC title** | Covers FE, BE, FS opportunities equally — don't use `Senior Full-Stack Engineer` (excludes pure FE roles) or `Senior Frontend Engineer` (excludes FS roles) |
| **"CV" not "Resume" in the download attribute** | CV is universal across markets; Resume is US-only |
| **Locale suffix on disk only, not in download attribute** | On-disk locale differentiation lets both files coexist; download attribute drops locale because page already signals it |

## Title Selection Guide

The title in the filename should be the most GENERAL senior IC title that still describes the candidate accurately. Examples:

| Candidate description | Best title for filename |
|---|---|
| Senior engineer who does both frontend and full-stack work | `Senior-Software-Engineer` (most general) |
| Senior frontend specialist only | `Senior-Frontend-Engineer` |
| Senior backend specialist only | `Senior-Backend-Engineer` |
| Staff/principal-level engineer with org-wide scope | `Staff-Engineer` or `Principal-Engineer` |
| Specialist roles | `Senior-Mobile-Engineer`, `Senior-Data-Engineer`, `Senior-Platform-Engineer` |

Avoid:
- `Senior-Developer` (Developer < Engineer in ATS tier weighting for senior roles)
- `Senior-Programmer` (junior-coded term)
- `Senior-Software-Architect` (specific role, narrower than candidate scope)

## Anti-Patterns

These filename styles read as casual or unprofessional:

| Anti-pattern | Why it fails | Fix |
|---|---|---|
| `cv.pdf` | Generic — recruiter has 50 of these in their downloads | Use the full convention |
| `resume.pdf` | Same as above | Use the full convention |
| `cv_john_doe.pdf` | Underscores + lowercase reads programmer-casual | `John-Doe-Senior-Software-Engineer-CV.pdf` |
| `JOHN_DOE_RESUME_2026.pdf` | All-caps reads like a draft file | PascalCase + no year |
| `Curriculum-Vitae.pdf` | Too literal | `[Name]-[Title]-CV.pdf` |
| `My_CV_v3_final_FINAL.pdf` | Version markers expose drafting | Single clean filename, no version suffix |
| `cv_john.pdf` | Abbreviated first name — informal | Full first name |
| `john-doe-cv-en.pdf` (download attr) | Locale suffix on what the recruiter sees is unnecessary noise | Drop locale from download attr; keep on disk |

## Implementation in Web Frontends

When the candidate's personal site serves the PDFs via a download link, the on-disk filename and the `download` attribute are SEPARATE. The on-disk name is referenced in the import; the `download` attribute is what the browser saves as.

### Vue 3 / Nuxt Example

Replace `Jane-Doe-Senior-Software-Engineer` with the actual candidate's `[Name]-[Title]` per the convention above.

```vue
<script setup>
import { computed } from 'vue';
import { useI18n } from 'vue-i18n';

import cv_en_url from '@assets/cv/Jane-Doe-Senior-Software-Engineer-EN.pdf?url';
import cv_es_url from '@assets/cv/Jane-Doe-Senior-Software-Engineer-ES.pdf?url';

const { locale } = useI18n();

const cv_href = computed(() => (locale.value === 'es' ? cv_es_url : cv_en_url));

// Locale-agnostic download filename — recruiter sees the same name regardless of page locale
const cv_filename = computed(() => 'Jane-Doe-Senior-Software-Engineer-CV.pdf');
</script>

<template>
  <a :href="cv_href" :download="cv_filename">Download CV</a>
</template>
```

### React / Next.js Example

```tsx
import { useTranslation } from 'next-i18next';

import cvEnUrl from '@/assets/cv/Jane-Doe-Senior-Software-Engineer-EN.pdf';
import cvEsUrl from '@/assets/cv/Jane-Doe-Senior-Software-Engineer-ES.pdf';

const CV_FILENAME = 'Jane-Doe-Senior-Software-Engineer-CV.pdf';

export function CVDownloadLink() {
  const { i18n } = useTranslation();
  const href = i18n.language === 'es' ? cvEsUrl : cvEnUrl;
  return <a href={href} download={CV_FILENAME}>Download CV</a>;
}
```

### Plain HTML Example

```html
<!-- EN page -->
<a href="/assets/cv/Jane-Doe-Senior-Software-Engineer-EN.pdf"
   download="Jane-Doe-Senior-Software-Engineer-CV.pdf">Download CV</a>

<!-- ES page -->
<a href="/assets/cv/Jane-Doe-Senior-Software-Engineer-ES.pdf"
   download="Jane-Doe-Senior-Software-Engineer-CV.pdf">Descargar CV</a>
```

Note the `download` attribute is identical in both locales.

## Migration / Renaming

When renaming an existing CV file in a git-tracked repo:

```sh
git mv src/assets/cv/cv_old_name_en.pdf src/assets/cv/Cristian-Moreno-Senior-Software-Engineer-EN.pdf
git mv src/assets/cv/cv_old_name_es.pdf src/assets/cv/Cristian-Moreno-Senior-Software-Engineer-ES.pdf
```

Then update import paths AND the download attribute string in the relevant component file. The `download` attribute often gets missed during refactors — search the codebase for the OLD filename to find every reference.

## ATS Scoring Note

Some ATS systems (notably Workday and some Greenhouse configurations) parse the FILENAME for the candidate's name and title before parsing the PDF content. A filename like `Cristian-Moreno-Senior-Software-Engineer-CV.pdf` produces these pre-parsed tokens:

- `Cristian Moreno` → matched to candidate name field
- `Senior Software Engineer` → matched to candidate title field
- `CV` → matched to document type

This means the filename provides a small but measurable ATS boost BEFORE content parsing. Generic filenames (`cv.pdf`, `resume.pdf`) miss this boost entirely.
