import { cn } from "@/lib/utils";
import {
  IconDownload,
  IconFilter,
  IconWorld,
  IconBrandTelegram,
  IconSortDescending,
  IconSettings,
  IconInfoCircle,
} from "@tabler/icons-react";

export type SettingsSection = "general" | "downloader" | "filter" | "proxy" | "notification" | "priority" | "system";

interface SettingsSidebarItem {
  id: SettingsSection;
  label: string;
  icon: React.ReactNode;
  disabled?: boolean;
}

const sidebarItems: SettingsSidebarItem[] = [
  {
    id: "general",
    label: "常规",
    icon: <IconSettings className="size-4" />,
  },
  {
    id: "downloader",
    label: "下载器",
    icon: <IconDownload className="size-4" />,
  },
  {
    id: "priority",
    label: "洗版优先级",
    icon: <IconSortDescending className="size-4" />,
  },
  {
    id: "filter",
    label: "过滤器",
    icon: <IconFilter className="size-4" />,
  },
  {
    id: "proxy",
    label: "代理",
    icon: <IconWorld className="size-4" />,
  },
  {
    id: "notification",
    label: "通知",
    icon: <IconBrandTelegram className="size-4" />,
  },
  {
    id: "system",
    label: "系统",
    icon: <IconInfoCircle className="size-4" />,
  },
];

interface SettingsSidebarProps {
  activeSection: SettingsSection;
  onSectionChange: (section: SettingsSection) => void;
}

export function SettingsSidebar({
  activeSection,
  onSectionChange,
}: SettingsSidebarProps) {
  return (
    <nav className="w-56 shrink-0">
      <div className="space-y-1">
        {sidebarItems.map((item) => (
          <button
            key={item.id}
            onClick={() => !item.disabled && onSectionChange(item.id)}
            disabled={item.disabled}
            className={cn(
              "w-full flex items-center gap-3 rounded-xl px-3 py-2.5 text-left transition-all duration-200",
              activeSection === item.id
                ? "bg-chart-1/15 text-chart-1"
                : "hover:bg-muted/50 text-muted-foreground hover:text-foreground",
              item.disabled && "opacity-50 cursor-not-allowed"
            )}
          >
            <div
              className={cn(
                "flex size-7 items-center justify-center rounded-lg transition-colors",
                activeSection === item.id
                  ? "bg-chart-1/20 text-chart-1"
                  : "bg-muted text-muted-foreground"
              )}
            >
              {item.icon}
            </div>
            <span className="text-sm font-medium">{item.label}</span>
            {item.disabled && (
              <span className="ml-auto text-[10px] font-medium text-muted-foreground bg-muted px-1.5 py-0.5 rounded">
                即将推出
              </span>
            )}
          </button>
        ))}
      </div>
    </nav>
  );
}
