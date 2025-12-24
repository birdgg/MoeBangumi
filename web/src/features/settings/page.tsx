import * as React from "react";
import { cn } from "@/lib/utils";
import { IconDownload, IconFilter } from "@tabler/icons-react";
import { useQuery } from "@tanstack/react-query";
import { getSettingsOptions } from "@/lib/api";
import { DownloaderSettings, RegexFilterSettings } from "./components";

type TabId = "downloader" | "regex";

interface TabItem {
  id: TabId;
  label: string;
  icon: React.ReactNode;
}

const tabs: TabItem[] = [
  { id: "downloader", label: "下载器", icon: <IconDownload className="size-4" /> },
  { id: "regex", label: "全局过滤", icon: <IconFilter className="size-4" /> },
];

function SettingsTab({
  tab,
  isActive,
  onClick,
}: {
  tab: TabItem;
  isActive: boolean;
  onClick: () => void;
}) {
  return (
    <button
      onClick={onClick}
      className={cn(
        "group relative flex items-center gap-2 px-4 py-2.5 text-sm font-medium transition-all duration-300",
        "hover:text-foreground focus:outline-none",
        isActive
          ? "text-foreground"
          : "text-muted-foreground hover:text-foreground/80"
      )}
    >
      <span
        className={cn(
          "transition-transform duration-300",
          isActive ? "scale-110 text-chart-1" : "group-hover:scale-105"
        )}
      >
        {tab.icon}
      </span>
      <span>{tab.label}</span>

      {/* Active indicator */}
      <span
        className={cn(
          "absolute bottom-0 left-0 h-0.5 w-full origin-left scale-x-0 rounded-full bg-linear-to-r from-chart-1 via-chart-2 to-chart-3 transition-transform duration-300",
          isActive && "scale-x-100"
        )}
      />

      {/* Glow effect */}
      <span
        className={cn(
          "absolute bottom-0 left-0 h-4 w-full bg-linear-to-t from-chart-1/20 to-transparent opacity-0 blur-md transition-opacity duration-300",
          isActive && "opacity-100"
        )}
      />
    </button>
  );
}

export function SettingsPage() {
  const [activeTab, setActiveTab] = React.useState<TabId>("downloader");
  const { data: settings } = useQuery(getSettingsOptions());

  return (
    <div className="min-h-full bg-linear-to-br from-chart-1/5 via-background to-chart-3/5 dark:from-zinc-950 dark:via-background dark:to-chart-3/10">
      {/* Decorative background elements */}
      <div className="pointer-events-none fixed inset-0 overflow-hidden">
        <div className="absolute -left-40 -top-40 size-80 rounded-full bg-chart-1/20 blur-3xl dark:bg-chart-1/10" />
        <div className="absolute -right-40 top-1/3 size-96 rounded-full bg-chart-3/20 blur-3xl dark:bg-chart-3/10" />
        <div className="absolute -bottom-40 left-1/3 size-80 rounded-full bg-chart-5/20 blur-3xl dark:bg-chart-5/10" />
      </div>

      {/* Content */}
      <div className="relative px-6 py-8 md:px-8">
        <div className="mx-auto max-w-2xl">
          {/* Tabs */}
          <div className="relative mb-8">
            <div className="flex border-b border-border/50">
              {tabs.map((tab) => (
                <SettingsTab
                  key={tab.id}
                  tab={tab}
                  isActive={activeTab === tab.id}
                  onClick={() => setActiveTab(tab.id)}
                />
              ))}
            </div>
          </div>

          {/* Tab content */}
          <div className="space-y-6">
            {activeTab === "downloader" && <DownloaderSettings settings={settings?.downloader} />}
            {activeTab === "regex" && <RegexFilterSettings settings={settings?.filter} />}
          </div>
        </div>
      </div>
    </div>
  );
}
