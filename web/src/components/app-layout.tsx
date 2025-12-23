import * as React from "react";
import {
  Sidebar,
  SidebarContent,
  SidebarHeader,
  SidebarInset,
  SidebarMenu,
  SidebarMenuButton,
  SidebarMenuButtonIcon,
  SidebarMenuButtonLabel,
  SidebarMenuItem,
  SidebarMobileTrigger,
  SidebarProvider,
  SidebarToggle,
} from "@/components/ui/sidebar";
import { Button } from "@/components/ui/button";
import {
  IconDeviceTv,
  IconCalendarWeek,
  IconSettings,
  IconSun,
  IconMoon,
  IconPlus,
} from "@tabler/icons-react";
import { SearchBangumiModal, AddBangumiModal } from "@/features/bangumi/components";
import { type Subject } from "@/lib/api";
import { ThemeColorSelector } from "@/components/theme-color-selector";

interface AppLayoutProps {
  children: React.ReactNode;
}

function ThemeToggleButton() {
  const [theme, setTheme] = React.useState<"light" | "dark">("dark");

  const toggleTheme = () => {
    const newTheme = theme === "dark" ? "light" : "dark";
    setTheme(newTheme);
    document.documentElement.classList.toggle("dark", newTheme === "dark");
  };

  React.useEffect(() => {
    document.documentElement.classList.add("dark");
  }, []);

  return (
    <Button
      variant="ghost"
      size="icon"
      onClick={toggleTheme}
      title={theme === "dark" ? "切换到亮色模式" : "切换到暗色模式"}
    >
      {theme === "dark" ? (
        <IconSun className="size-5" />
      ) : (
        <IconMoon className="size-5" />
      )}
    </Button>
  );
}

interface AddBangumiButtonProps {
  onClick: () => void;
}

function AddBangumiButton({ onClick }: AddBangumiButtonProps) {
  return (
    <Button
      className="gap-2 bg-linear-to-r from-chart-1 to-chart-3 text-white shadow-lg shadow-chart-3/20 hover:opacity-90"
      onClick={onClick}
    >
      <IconPlus className="size-4" />
      <span className="hidden sm:inline">添加番剧</span>
    </Button>
  );
}

export function AppLayout({ children }: AppLayoutProps) {
  const [activeItem, setActiveItem] = React.useState("anime");
  const [searchModalOpen, setSearchModalOpen] = React.useState(false);
  const [addModalOpen, setAddModalOpen] = React.useState(false);
  const [selectedSubject, setSelectedSubject] = React.useState<Subject | null>(null);

  const handleSelectBangumi = (subject: Subject) => {
    setSelectedSubject(subject);
    setAddModalOpen(true);
  };

  const handleAddSuccess = () => {
    setSelectedSubject(null);
  };

  return (
    <SidebarProvider>
      <div className="flex h-screen w-full overflow-hidden bg-background">
        <Sidebar>
          <SidebarHeader>
            <div className="flex w-full items-center justify-center gap-3 group-data-[collapsed=false]/sidebar:justify-start">
              <div className="flex size-9 shrink-0 items-center justify-center rounded-lg bg-linear-to-br from-chart-1 to-chart-3 text-sm font-bold text-white shadow-lg shadow-chart-3/20">
                M
              </div>
              <span className="hidden text-sm font-semibold tracking-tight group-data-[collapsed=false]/sidebar:block">
                Moe
              </span>
            </div>
          </SidebarHeader>

          <SidebarContent>
            <SidebarMenu>
              <SidebarMenuItem>
                <SidebarMenuButton
                  variant={activeItem === "anime" ? "active" : "default"}
                  onClick={() => setActiveItem("anime")}
                  tooltip="动漫"
                >
                  <SidebarMenuButtonIcon>
                    <IconDeviceTv />
                  </SidebarMenuButtonIcon>
                  <SidebarMenuButtonLabel>动漫</SidebarMenuButtonLabel>
                </SidebarMenuButton>
              </SidebarMenuItem>
              <SidebarMenuItem>
                <SidebarMenuButton
                  variant={activeItem === "schedule" ? "active" : "default"}
                  onClick={() => setActiveItem("schedule")}
                  tooltip="每日放送"
                >
                  <SidebarMenuButtonIcon>
                    <IconCalendarWeek />
                  </SidebarMenuButtonIcon>
                  <SidebarMenuButtonLabel>每日放送</SidebarMenuButtonLabel>
                </SidebarMenuButton>
              </SidebarMenuItem>
              <SidebarMenuItem>
                <SidebarMenuButton
                  variant={activeItem === "settings" ? "active" : "default"}
                  onClick={() => setActiveItem("settings")}
                  tooltip="设置"
                >
                  <SidebarMenuButtonIcon>
                    <IconSettings />
                  </SidebarMenuButtonIcon>
                  <SidebarMenuButtonLabel>设置</SidebarMenuButtonLabel>
                </SidebarMenuButton>
              </SidebarMenuItem>
            </SidebarMenu>
          </SidebarContent>
        </Sidebar>

        <SidebarInset>
          {/* Top bar */}
          <header className="flex h-14 shrink-0 items-center justify-between border-b border-border px-4">
            <div className="flex items-center gap-2">
              {/* Sidebar toggle (desktop) */}
              <SidebarToggle />
              {/* Mobile menu trigger */}
              <SidebarMobileTrigger />
              {/* Mobile logo */}
              <div className="flex items-center gap-2 md:hidden">
                <div className="flex size-8 items-center justify-center rounded-lg bg-linear-to-br from-chart-1 to-chart-3 text-sm font-bold text-white">
                  M
                </div>
                <span className="font-semibold">Moe</span>
              </div>
            </div>

            {/* Right side actions */}
            <div className="flex items-center gap-2">
              <AddBangumiButton onClick={() => setSearchModalOpen(true)} />
              <ThemeColorSelector />
              <ThemeToggleButton />
            </div>
          </header>

          {/* Search Modal */}
          <SearchBangumiModal
            open={searchModalOpen}
            onOpenChange={setSearchModalOpen}
            onSelect={handleSelectBangumi}
          />

          {/* Add Bangumi Modal */}
          <AddBangumiModal
            open={addModalOpen}
            onOpenChange={setAddModalOpen}
            subject={selectedSubject}
            onSuccess={handleAddSuccess}
          />

          {/* Main content area */}
          <main className="flex-1 overflow-auto">{children}</main>
        </SidebarInset>
      </div>
    </SidebarProvider>
  );
}
