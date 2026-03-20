---
title: Mandate Typography Utility Classes
impact: CRITICAL
impactDescription: Ensures all text adheres to brand guidelines for font family, size, weight, and color, maintaining visual consistency and a cohesive user experience.
tags: css, typography, font, color, styling, utility-first, responsive
---

## Mandate Typography Utility Classes
All text styling—including font family, size, weight, color, and transformation—**must** be controlled via the provided utility classes. Writing custom `font-*`, `color`, or `text-*` properties in component-specific stylesheets is strictly forbidden, as it undermines the central design system.

The principle is **composition**: combine multiple classes to achieve the desired style.

---

### 1. Font Family
Use these classes to set the font family.

| Class Name(s)                  | Font Family               | Primary Use Case                              |
|:-------------------------------|:--------------------------|:----------------------------------------------|
| `.f-primary`                   | Averta Regular            | Default body and UI text.                     |
| `.f-primary.bold`              | Averta Bold               | Bolded primary text.                          |
| `.f-secondary`                 | Kapra Neue                | **Always use with `.upper`**. For headings.   |
| `.f-script`                    | Betterworks               | Decorative script font for special occasions. |
| `.f-domaine-display-condensed` | Domaine Display Condensed | A serif font for specific display headings.   |
| `.didot`                       | Didot                     | Legacy serif font.                            |
| `.electra`                     | Electra                   | Legacy serif font.                            |

---

### 2. Font Size (Responsive)
Font sizes are responsive and mobile-first. Apply a base size (e.g., `.xs-f-medium`) and override it at larger breakpoints (e.g., `.md-f-large`).

| Size Name  | Base (`.font-*`)  | Mobile (`.xs-f-*`) | Tablet (`.sm-f-*`) | Desktop MD (`.md-f-*`) | Desktop LG (`.lg-f-*`) | Desktop XL (`.xl-f-*`) |
|:-----------|:------------------|:-------------------|:-------------------|:-----------------------|:-----------------------|:-----------------------|
| xxx-small  | `.font-xxxsmall`  | `.xs-f-xxxsmall`   | `.sm-f-xxxsmall`   | `.md-f-xxxsmall`       | `.lg-f-xxxsmall`       | `.xl-f-xxxsmall`       |
| xx-small   | `.font-xxsmall`   | `.xs-f-xxsmall`    | `.sm-f-xxsmall`    | `.md-f-xxsmall`        | `.lg-f-xxsmall`        | `.xl-f-xxsmall`        |
| x-small    | `.font-xsmall`    | `.xs-f-xsmall`     | `.sm-f-xsmall`     | `.md-f-xsmall`         | `.lg-f-xsmall`         | `.xl-f-xsmall`         |
| small      | `.font-small`     | `.xs-f-small`      | `.sm-f-small`      | `.md-f-small`          | `.lg-f-small`          | `.xl-f-small`          |
| medium     | `.font-medium`    | `.xs-f-medium`     | `.sm-f-medium`     | `.md-f-medium`         | `.lg-f-medium`         | `.xl-f-medium`         |
| x-medium   | `.font-xmedium`   | `.xs-f-xmedium`    | `.sm-f-xmedium`    | `.md-f-xmedium`        | `.lg-f-xmedium`        | `.xl-f-xmedium`        |
| large      | `.font-large`     | `.xs-f-large`      | `.sm-f-large`      | `.md-f-large`          | `.lg-f-large`          | `.xl-f-large`          |
| x-large    | `.font-xlarge`    | `.xs-f-xlarge`     | `.sm-f-xlarge`     | `.md-f-xlarge`         | `.lg-f-xlarge`         | `.xl-f-xlarge`         |
| xx-large   | `.font-xxlarge`   | `.xs-f-xxlarge`    | `.sm-f-xxlarge`    | `.md-f-xxlarge`        | `.lg-f-xxlarge`        | `.xl-f-xxlarge`        |
| xxx-large  | `.font-xxxlarge`  | `.xs-f-xxxlarge`   | `.sm-f-xxxlarge`   | `.md-f-xxxlarge`       | `.lg-f-xxxlarge`       | `.xl-f-xxxlarge`       |
| grande     | `.font-grande`    | `.xs-f-grande`     | `.sm-f-grande`     | `.md-f-grande`         | `.lg-f-grande`         | `.xl-f-grande`         |
| x-grande   | `.font-xgrande`   | `.xs-f-xgrande`    | `.sm-f-xgrande`    | `.md-f-xgrande`        | `.lg-f-xgrande`        | `.xl-f-xgrande`        |
| xx-grande  | `.font-xxgrande`  | `.xs-f-xxgrande`   | `.sm-f-xxgrande`   | `.md-f-xxgrande`       | `.lg-f-xxgrande`       | `.xl-f-xxgrande`       |
| xxx-grande | `.font-xxxgrande` | `.xs-f-xxxgrande`  | `.sm-f-xxxgrande`  | `.md-f-xxxgrande`      | `.lg-f-xxxgrande`      | `.xl-f-xxxgrande`      |
| poster     | `.font-poster`    | `.xs-f-poster`     | `.sm-f-poster`     | `.md-f-poster`         | `.lg-f-poster`         | `.xl-f-poster`         |
| x-poster   | `.font-xposter`   | `.xs-f-xposter`    | `.sm-f-xposter`    | `.md-f-xposter`        | `.lg-f-xposter`        | `.xl-f-xposter`        |
| billboard  | `.font-billboard` | `.xs-f-billboard`  | `.sm-f-billboard`  | `.md-f-billboard`      | `.lg-f-billboard`      | `.xl-f-billboard`      |

---

### 3. Font Weight, Style, and Decoration
| Class Name   | Purpose                         |
|:-------------|:--------------------------------|
| `.bold`      | Applies bold font weight.       |
| `.semi-bold` | Applies semi-bold font weight.  |
| `.normal`    | Applies normal font weight.     |
| `.thin`      | Applies thin font weight.       |
| `.strike`    | `text-decoration: line-through` |
| `.underline` | `text-decoration: underline`    |

---

### 4. Text Color
Use these classes to apply color. **Never use hex codes directly.**

#### Core & Brand Colors
| Class Name               | Intended Use                            |
|:-------------------------|:----------------------------------------|
| `.color-mr-purple`       | Primary brand purple (`#3A2D4A`).       |
| `.color-mr-purple-light` | Lighter brand purple.                   |
| `.color-mr-purple-dark`  | Darker brand purple.                    |
| `.color-mr-black`        | Primary text color (`#343434`).         |
| `.color-slate`           | Dark gray text color (`#434343`).       |
| `.color-smoke`           | A lighter, secondary purple-gray.       |
| `.color-accent`          | Accent color, typically a reddish-pink. |

#### Grayscale & Neutral Colors
| Class Name            | Intended Use                                       |
|:----------------------|:---------------------------------------------------|
| `.color-black`        | Pure black (`#000`).                               |
| `.color-white`        | Pure white (`#fff`), for text on dark backgrounds. |
| `.color-slate-light`  | A lighter gray for secondary text.                 |
| `.color-gray`         | Medium gray.                                       |
| `.color-gray-light`   | Very light gray.                                   |
| `.color-gray-med`     | Medium-emphasis gray.                              |
| `.color-fog`          | A neutral, soft gray.                              |
| `.color-fog-light`    | A lighter version of fog gray.                     |
| `.color-triple-nines` | `#999` gray.                                       |

#### Feedback & Status Colors
| Class Name                         | Intended Use                                |
|:-----------------------------------|:--------------------------------------------|
| `.color-font-error` / `.color-red` | For error messages and validation failures. |
| `.color-green`                     | For success states or positive feedback.    |
| `.color-blue` / `.color-mr-blue`   | For informational text.                     |

---

### 5. Text Transformation, Spacing, and Layout
| Class Name                  | Purpose                                |
|:----------------------------|:---------------------------------------|
| `.upper`                    | `text-transform: uppercase !important` |
| `.lower`                    | `text-transform: lowercase !important` |
| `.capitalize`               | `text-transform: capitalize`           |
| `.ellipsis`                 | Truncates overflowing text with `...`  |
| `.zero-letter-spacing`      | `letter-spacing: 0`                    |
| `.regular-letter-spacing`   | `letter-spacing: 0.03em`               |
| `.semi-wide-letter-spacing` | `letter-spacing: 0.1em`                |
| `.wide-letter-spacing`      | `letter-spacing: 0.2em`                |
| `.narrow-letter-spacing`    | `letter-spacing: -0.03em`              |
| `.line-height-small`        | `line-height: 1.3em`                   |
| `.line-height-medium`       | `line-height: 1.5em`                   |

---

### 6. Usage Example: Composition in Practice
**Incorrect:** Writing a block of custom CSS for a single element.

```stylus
// components/PageTitle.styl
.page-title
  font-family: 'Kapra Neue', sans-serif
  text-transform: uppercase
  font-weight: 700
  font-size: 24px // font-size-xlarge
  color: #343434 // color-mr-black
  letter-spacing: 0.1em

  @media (min-width: 960px) // mq-desktop-plus
    font-size: 32px // font-size-xxxlarge
```

**Correct:** Composing all necessary utility classes directly in the Pug template.

```pug
h1.f-secondary.upper.bold.xs-f-xlarge.lg-f-xxxlarge.color-mr-black.semi-wide-letter-spacing
  | Our New Collection
```
