import * as React from "react";
import {
  DropdownMenu,
  DropdownMenuTrigger,
  DropdownMenuContent,
  DropdownMenuItem,
} from "@/components/ui/dropdown-menu";
import { IconPalette, IconCheck } from "@tabler/icons-react";
import {
  themeColors,
  getStoredThemeColor,
  applyThemeColor,
  type ThemeColor,
} from "@/lib/theme";
import { cn } from "@/lib/utils";

function ColorDot({ theme }: { theme: ThemeColor }) {
  return (
    <span
      className={cn(
        "size-4 rounded-full shadow-sm",
        theme.preview
      )}
    />
  );
}

export function ThemeColorSelector() {
  const [currentColor, setCurrentColor] = React.useState<string>("pink");

  React.useEffect(() => {
    const stored = getStoredThemeColor();
    setCurrentColor(stored);
    applyThemeColor(stored);
  }, []);

  // Re-apply theme when dark mode changes
  React.useEffect(() => {
    const observer = new MutationObserver((mutations) => {
      mutations.forEach((mutation) => {
        if (
          mutation.type === "attributes" &&
          mutation.attributeName === "class"
        ) {
          applyThemeColor(currentColor);
        }
      });
    });

    observer.observe(document.documentElement, {
      attributes: true,
      attributeFilter: ["class"],
    });

    return () => observer.disconnect();
  }, [currentColor]);

  const handleColorChange = (colorName: string) => {
    setCurrentColor(colorName);
    applyThemeColor(colorName);
  };

  const currentTheme = themeColors.find((t) => t.name === currentColor);

  return (
    <DropdownMenu>
      <DropdownMenuTrigger
        className={cn(
          "relative inline-flex items-center justify-center rounded-md p-2",
          "text-muted-foreground hover:bg-accent hover:text-accent-foreground",
          "focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring"
        )}
        title="Choose theme color"
      >
        <IconPalette className="size-5" />
        {currentTheme && (
          <span
            className={cn(
              "absolute -bottom-0.5 -right-0.5 size-2.5 rounded-full ring-2 ring-background",
              currentTheme.preview
            )}
          />
        )}
      </DropdownMenuTrigger>
      <DropdownMenuContent align="end" className="w-48">
        <div className="px-2 py-1.5 text-xs font-medium text-muted-foreground">
          Theme Color
        </div>
        <div className="grid grid-cols-2 gap-1 p-1">
          {themeColors.map((theme) => (
            <DropdownMenuItem
              key={theme.name}
              onClick={() => handleColorChange(theme.name)}
              className="flex items-center gap-2 rounded-md px-2 py-1.5"
            >
              <ColorDot theme={theme} />
              <span className="flex-1 text-xs">{theme.label}</span>
              {currentColor === theme.name && (
                <IconCheck className="size-3.5 text-foreground" />
              )}
            </DropdownMenuItem>
          ))}
        </div>
      </DropdownMenuContent>
    </DropdownMenu>
  );
}
