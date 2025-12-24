import * as React from "react";
import { flushSync } from "react-dom";
import { IconSun, IconMoon } from "@tabler/icons-react";

export function ThemeToggleButton() {
  const [theme, setTheme] = React.useState<"light" | "dark">("dark");
  const [isAnimating, setIsAnimating] = React.useState(false);
  const buttonRef = React.useRef<HTMLButtonElement>(null);

  const toggleTheme = () => {
    if (isAnimating) return;

    const newTheme = theme === "dark" ? "light" : "dark";

    if (document.startViewTransition) {

      setIsAnimating(true);

      const transition = document.startViewTransition(() => {
        flushSync(() => {
          setTheme(newTheme);
        });
        document.documentElement.classList.toggle("dark", newTheme === "dark");
      });

      transition.finished.then(() => {
        setIsAnimating(false);
      });
    } else {
      // Fallback for browsers without View Transitions API
      setIsAnimating(true);
      setTheme(newTheme);
      document.documentElement.classList.toggle("dark", newTheme === "dark");
      setTimeout(() => setIsAnimating(false), 800);
    }
  };

  React.useEffect(() => {
    document.documentElement.classList.add("dark");
    document.documentElement.classList.add("-animated");
  }, []);

  const isDark = theme === "dark";

  return (
    <button
      ref={buttonRef}
      onClick={toggleTheme}
      disabled={isAnimating}
      className="group relative flex size-10 items-center justify-center rounded-xl transition-all duration-300 hover:scale-110 hover:bg-foreground/5 focus:outline-none focus-visible:ring-2 focus-visible:ring-chart-1/50"
      title={isDark ? "切换到亮色模式" : "切换到暗色模式"}
      aria-label={isDark ? "Switch to light mode" : "Switch to dark mode"}
    >
      {/* Icon container with rotation animation */}
      <div
        className={`relative transition-transform duration-700 ease-out ${isAnimating ? (isDark ? "rotate-360" : "-rotate-360") : ""
          }`}
      >
        {/* Sun icon */}
        <div
          className={`absolute inset-0 flex items-center justify-center transition-all duration-700 ${isDark
            ? "rotate-0 scale-100 opacity-100"
            : "rotate-90 scale-0 opacity-0"
            }`}
        >
          <IconSun className="size-5" />
        </div>

        {/* Moon icon */}
        <div
          className={`flex items-center justify-center transition-all duration-700 ${!isDark
            ? "rotate-0 scale-100 opacity-100"
            : "-rotate-90 scale-0 opacity-0"
            }`}
        >
          <IconMoon className="size-5" />
        </div>
      </div>
    </button>
  );
}
