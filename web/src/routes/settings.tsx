import { createFileRoute } from "@tanstack/react-router";
import { SettingsPage } from "@/features/settings/page";
import type { SettingsSection } from "@/features/settings/components";

const validSections: SettingsSection[] = [
  "general",
  "downloader",
  "priority",
  "filter",
  "proxy",
  "notification",
  "system",
];

export const Route = createFileRoute("/settings")({
  component: SettingsPage,
  validateSearch: (search: Record<string, unknown>): { section: SettingsSection } => {
    const section = search.section as string;
    if (validSections.includes(section as SettingsSection)) {
      return { section: section as SettingsSection };
    }
    return { section: "general" };
  },
});
