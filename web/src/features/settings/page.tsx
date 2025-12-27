import { useState } from "react";
import { Button } from "@/components/ui/button";
import { IconLoader2, IconDeviceFloppy, IconSettings } from "@tabler/icons-react";
import { useQuery, useMutation, useQueryClient } from "@tanstack/react-query";
import { toast } from "sonner";
import {
  getSettingsOptions,
  updateSettingsMutation,
  getSettingsQueryKey,
} from "@/lib/api";
import {
  DownloaderSection,
  FilterSection,
  ProxySection,
  SettingsSidebar,
  type SettingsSection,
} from "./components";
import { useSettingsForm, formDataToUpdateSettings } from "./hooks";

export function SettingsPage() {
  const [activeSection, setActiveSection] = useState<SettingsSection>("downloader");
  const queryClient = useQueryClient();
  const { data: settings } = useQuery(getSettingsOptions());

  // Create form instance with server data
  const form = useSettingsForm(settings);

  // Save mutation
  const { mutate: saveSettings, isPending: isSaving } = useMutation({
    ...updateSettingsMutation(),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: getSettingsQueryKey() });
      toast.success("设置已保存");
    },
    onError: (error) => {
      const message = error instanceof Error ? error.message : "保存失败";
      toast.error("保存设置失败", { description: message });
    },
  });

  const handleSave = () => {
    const values = form.state.values;
    saveSettings({
      body: formDataToUpdateSettings(values),
    });
  };

  const renderContent = () => {
    switch (activeSection) {
      case "downloader":
        return <DownloaderSection form={form} />;
      case "filter":
        return <FilterSection form={form} />;
      case "proxy":
        return <ProxySection form={form} />;
      case "notification":
        return (
          <div className="flex flex-col items-center justify-center py-16 text-center">
            <div className="size-16 rounded-2xl bg-muted flex items-center justify-center mb-4">
              <IconSettings className="size-8 text-muted-foreground" />
            </div>
            <h3 className="text-lg font-medium mb-2">通知设置</h3>
            <p className="text-sm text-muted-foreground">即将推出</p>
          </div>
        );
      default:
        return null;
    }
  };

  return (
    <div className="min-h-full">
      <div className="px-6 py-8 md:px-8">
        <div className="max-w-7xl mx-auto">
          {/* Header */}
          <div className="mb-8">
            <h1 className="text-2xl font-bold tracking-tight">设置</h1>
            <p className="text-sm text-muted-foreground mt-1">
              管理应用程序配置和偏好设置
            </p>
          </div>

          {/* Two-column layout */}
          <div className="flex gap-8">
            {/* Sidebar */}
            <SettingsSidebar
              activeSection={activeSection}
              onSectionChange={setActiveSection}
            />

            {/* Content area */}
            <div className="flex-1 min-w-0">
              <form
                className="space-y-6"
                onSubmit={(e) => {
                  e.preventDefault();
                  e.stopPropagation();
                  handleSave();
                }}
              >
                {/* Content card */}
                <div className="rounded-2xl border bg-card p-6">
                  {renderContent()}
                </div>

                {/* Save Button - only show for implemented sections */}
                {(activeSection === "downloader" || activeSection === "filter" || activeSection === "proxy") && (
                  <div className="flex justify-end">
                    <form.Subscribe selector={(state) => state.canSubmit}>
                      {(canSubmit) => (
                        <Button
                          type="submit"
                          disabled={isSaving || !canSubmit}
                          className="relative gap-2 overflow-hidden bg-linear-to-r from-chart-1 to-chart-3 px-5 text-white shadow-md shadow-chart-1/20 transition-all duration-300 hover:shadow-lg hover:shadow-chart-1/30 disabled:from-muted disabled:to-muted disabled:text-muted-foreground disabled:shadow-none"
                        >
                          {/* Sparkle decorations */}
                          <span className="absolute -top-0.5 left-3 text-[8px] opacity-70 transition-opacity group-hover/button:opacity-100">
                            ✦
                          </span>
                          <span className="absolute -bottom-0.5 right-4 text-[8px] opacity-70 transition-opacity group-hover/button:opacity-100">
                            ✦
                          </span>

                          {isSaving ? (
                            <IconLoader2 className="size-4 animate-spin" />
                          ) : (
                            <IconDeviceFloppy className="size-4" />
                          )}
                          <span className="font-medium">保存</span>
                        </Button>
                      )}
                    </form.Subscribe>
                  </div>
                )}
              </form>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
