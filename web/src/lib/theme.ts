// Theme color configuration using oklch values that match Tailwind CSS colors
// Each theme defines colors for chart-1 through chart-5 and sidebar-primary

export interface ThemeColor {
  name: string;
  label: string;
  colors: {
    light: {
      chart1: string;
      chart2: string;
      chart3: string;
      chart4: string;
      chart5: string;
      sidebarPrimary: string;
    };
    dark: {
      chart1: string;
      chart2: string;
      chart3: string;
      chart4: string;
      chart5: string;
      sidebarPrimary: string;
    };
  };
  // Preview color for the selector (Tailwind class)
  preview: string;
}

export const themeColors: ThemeColor[] = [
  {
    name: "pink",
    label: "Pink",
    colors: {
      light: {
        chart1: "oklch(0.78 0.16 350)",
        chart2: "oklch(0.65 0.22 340)",
        chart3: "oklch(0.55 0.24 320)",
        chart4: "oklch(0.50 0.22 300)",
        chart5: "oklch(0.45 0.18 290)",
        sidebarPrimary: "oklch(0.55 0.24 320)",
      },
      dark: {
        chart1: "oklch(0.78 0.16 350)",
        chart2: "oklch(0.65 0.22 340)",
        chart3: "oklch(0.55 0.24 320)",
        chart4: "oklch(0.50 0.22 300)",
        chart5: "oklch(0.45 0.18 290)",
        sidebarPrimary: "oklch(0.65 0.22 340)",
      },
    },
    preview: "bg-pink-500",
  },
  {
    name: "rose",
    label: "Rose",
    colors: {
      light: {
        chart1: "oklch(0.80 0.15 10)",
        chart2: "oklch(0.70 0.20 5)",
        chart3: "oklch(0.60 0.22 0)",
        chart4: "oklch(0.55 0.20 355)",
        chart5: "oklch(0.50 0.18 350)",
        sidebarPrimary: "oklch(0.60 0.22 0)",
      },
      dark: {
        chart1: "oklch(0.80 0.15 10)",
        chart2: "oklch(0.70 0.20 5)",
        chart3: "oklch(0.60 0.22 0)",
        chart4: "oklch(0.55 0.20 355)",
        chart5: "oklch(0.50 0.18 350)",
        sidebarPrimary: "oklch(0.70 0.20 5)",
      },
    },
    preview: "bg-rose-500",
  },
  {
    name: "orange",
    label: "Orange",
    colors: {
      light: {
        chart1: "oklch(0.80 0.15 55)",
        chart2: "oklch(0.72 0.18 50)",
        chart3: "oklch(0.65 0.20 45)",
        chart4: "oklch(0.58 0.18 40)",
        chart5: "oklch(0.52 0.16 35)",
        sidebarPrimary: "oklch(0.65 0.20 45)",
      },
      dark: {
        chart1: "oklch(0.80 0.15 55)",
        chart2: "oklch(0.72 0.18 50)",
        chart3: "oklch(0.65 0.20 45)",
        chart4: "oklch(0.58 0.18 40)",
        chart5: "oklch(0.52 0.16 35)",
        sidebarPrimary: "oklch(0.72 0.18 50)",
      },
    },
    preview: "bg-orange-500",
  },
  {
    name: "amber",
    label: "Amber",
    colors: {
      light: {
        chart1: "oklch(0.85 0.14 85)",
        chart2: "oklch(0.78 0.16 80)",
        chart3: "oklch(0.70 0.17 75)",
        chart4: "oklch(0.62 0.15 70)",
        chart5: "oklch(0.55 0.13 65)",
        sidebarPrimary: "oklch(0.70 0.17 75)",
      },
      dark: {
        chart1: "oklch(0.85 0.14 85)",
        chart2: "oklch(0.78 0.16 80)",
        chart3: "oklch(0.70 0.17 75)",
        chart4: "oklch(0.62 0.15 70)",
        chart5: "oklch(0.55 0.13 65)",
        sidebarPrimary: "oklch(0.78 0.16 80)",
      },
    },
    preview: "bg-amber-500",
  },
  {
    name: "green",
    label: "Green",
    colors: {
      light: {
        chart1: "oklch(0.80 0.15 145)",
        chart2: "oklch(0.72 0.18 150)",
        chart3: "oklch(0.64 0.19 155)",
        chart4: "oklch(0.56 0.17 160)",
        chart5: "oklch(0.48 0.14 165)",
        sidebarPrimary: "oklch(0.64 0.19 155)",
      },
      dark: {
        chart1: "oklch(0.80 0.15 145)",
        chart2: "oklch(0.72 0.18 150)",
        chart3: "oklch(0.64 0.19 155)",
        chart4: "oklch(0.56 0.17 160)",
        chart5: "oklch(0.48 0.14 165)",
        sidebarPrimary: "oklch(0.72 0.18 150)",
      },
    },
    preview: "bg-green-500",
  },
  {
    name: "teal",
    label: "Teal",
    colors: {
      light: {
        chart1: "oklch(0.78 0.12 180)",
        chart2: "oklch(0.70 0.14 175)",
        chart3: "oklch(0.62 0.15 170)",
        chart4: "oklch(0.54 0.13 165)",
        chart5: "oklch(0.46 0.11 160)",
        sidebarPrimary: "oklch(0.62 0.15 170)",
      },
      dark: {
        chart1: "oklch(0.78 0.12 180)",
        chart2: "oklch(0.70 0.14 175)",
        chart3: "oklch(0.62 0.15 170)",
        chart4: "oklch(0.54 0.13 165)",
        chart5: "oklch(0.46 0.11 160)",
        sidebarPrimary: "oklch(0.70 0.14 175)",
      },
    },
    preview: "bg-teal-500",
  },
  {
    name: "cyan",
    label: "Cyan",
    colors: {
      light: {
        chart1: "oklch(0.80 0.12 200)",
        chart2: "oklch(0.72 0.14 205)",
        chart3: "oklch(0.64 0.15 210)",
        chart4: "oklch(0.56 0.13 215)",
        chart5: "oklch(0.48 0.11 220)",
        sidebarPrimary: "oklch(0.64 0.15 210)",
      },
      dark: {
        chart1: "oklch(0.80 0.12 200)",
        chart2: "oklch(0.72 0.14 205)",
        chart3: "oklch(0.64 0.15 210)",
        chart4: "oklch(0.56 0.13 215)",
        chart5: "oklch(0.48 0.11 220)",
        sidebarPrimary: "oklch(0.72 0.14 205)",
      },
    },
    preview: "bg-cyan-500",
  },
  {
    name: "blue",
    label: "Blue",
    colors: {
      light: {
        chart1: "oklch(0.78 0.14 240)",
        chart2: "oklch(0.68 0.18 245)",
        chart3: "oklch(0.58 0.20 250)",
        chart4: "oklch(0.50 0.18 255)",
        chart5: "oklch(0.42 0.15 260)",
        sidebarPrimary: "oklch(0.58 0.20 250)",
      },
      dark: {
        chart1: "oklch(0.78 0.14 240)",
        chart2: "oklch(0.68 0.18 245)",
        chart3: "oklch(0.58 0.20 250)",
        chart4: "oklch(0.50 0.18 255)",
        chart5: "oklch(0.42 0.15 260)",
        sidebarPrimary: "oklch(0.68 0.18 245)",
      },
    },
    preview: "bg-blue-500",
  },
  {
    name: "violet",
    label: "Violet",
    colors: {
      light: {
        chart1: "oklch(0.78 0.16 280)",
        chart2: "oklch(0.68 0.20 285)",
        chart3: "oklch(0.58 0.22 290)",
        chart4: "oklch(0.50 0.20 295)",
        chart5: "oklch(0.42 0.17 300)",
        sidebarPrimary: "oklch(0.58 0.22 290)",
      },
      dark: {
        chart1: "oklch(0.78 0.16 280)",
        chart2: "oklch(0.68 0.20 285)",
        chart3: "oklch(0.58 0.22 290)",
        chart4: "oklch(0.50 0.20 295)",
        chart5: "oklch(0.42 0.17 300)",
        sidebarPrimary: "oklch(0.68 0.20 285)",
      },
    },
    preview: "bg-violet-500",
  }
];

const THEME_COLOR_KEY = "moe-theme-color";

export function getStoredThemeColor(): string {
  if (typeof window === "undefined") return "pink";
  return localStorage.getItem(THEME_COLOR_KEY) || "pink";
}

export function setStoredThemeColor(colorName: string): void {
  if (typeof window === "undefined") return;
  localStorage.setItem(THEME_COLOR_KEY, colorName);
}

export function getThemeByName(name: string): ThemeColor | undefined {
  return themeColors.find((t) => t.name === name);
}

export function applyThemeColor(colorName: string): void {
  const theme = getThemeByName(colorName);
  if (!theme) return;

  const isDark = document.documentElement.classList.contains("dark");
  const colors = isDark ? theme.colors.dark : theme.colors.light;

  const root = document.documentElement;
  root.style.setProperty("--chart-1", colors.chart1);
  root.style.setProperty("--chart-2", colors.chart2);
  root.style.setProperty("--chart-3", colors.chart3);
  root.style.setProperty("--chart-4", colors.chart4);
  root.style.setProperty("--chart-5", colors.chart5);
  root.style.setProperty("--sidebar-primary", colors.sidebarPrimary);

  setStoredThemeColor(colorName);
}

export function initializeTheme(): string {
  const storedColor = getStoredThemeColor();
  applyThemeColor(storedColor);
  return storedColor;
}
