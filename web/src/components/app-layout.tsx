import * as React from "react";
import {
  Sidebar,
  SidebarContent,
  SidebarGroup,
  SidebarHeader,
  SidebarInset,
  SidebarMenu,
  SidebarMenuButton,
  SidebarMenuItem,
  SidebarProvider,
} from "@/components/ui/sidebar";
import { Button } from "@/components/ui/button";
import {
  IconDeviceTv,
  IconCalendarWeek,
  IconSettings,
  IconPlus,
  IconSparkles,
} from "@tabler/icons-react";
import { SearchBangumiModal, AddBangumiModal } from "@/features/bangumi/components";
import { type Subject } from "@/lib/api";
import { ThemeColorSelector } from "@/components/theme-color-selector";
import { ThemeToggleButton } from "@/components/theme-toggle-button";

interface AppLayoutProps {
  children: React.ReactNode;
}

interface AddBangumiButtonProps {
  onClick: () => void;
}

function AddBangumiButton({ onClick }: AddBangumiButtonProps) {
  return (
    <Button
      className="group relative gap-2 overflow-hidden rounded-xl bg-linear-to-r from-chart-1 via-chart-2 to-chart-3 px-4 py-2 text-white shadow-lg shadow-chart-3/30 transition-all duration-300 hover:scale-105 hover:shadow-xl hover:shadow-chart-2/40"
      onClick={onClick}
    >
      <span className="absolute inset-0 bg-linear-to-r from-chart-2 via-chart-3 to-chart-1 opacity-0 transition-opacity duration-500 group-hover:opacity-100" />
      <IconPlus className="relative z-10 size-4 transition-transform duration-300 group-hover:rotate-90" />
      <span className="relative z-10 hidden font-medium sm:inline">添加番剧</span>
      <IconSparkles className="relative z-10 size-3 opacity-0 transition-all duration-300 group-hover:opacity-100" />
    </Button>
  );
}

function MoeLogo() {
  return (
    <div className="group flex items-center gap-4">
      <div className="relative">
        <div className="absolute -inset-1 rounded-2xl bg-linear-to-br from-chart-1 via-chart-2 to-chart-3 opacity-75 blur-sm transition-all duration-300 group-hover:opacity-100 group-hover:blur-md" />
        <div className="relative flex size-10 items-center justify-center rounded-2xl bg-linear-to-br from-chart-1 via-chart-2 to-chart-3 text-base font-bold text-white shadow-lg transition-transform duration-300 group-hover:scale-110">
          <span className="drop-shadow-sm">M</span>
          <IconSparkles className="absolute -top-1 -right-1 size-3 text-white opacity-0 transition-all duration-300 group-hover:opacity-100 group-hover:rotate-12" />
        </div>
      </div>
      <span className="bg-linear-to-r from-chart-1 via-chart-2 to-chart-3 bg-clip-text text-lg font-bold tracking-tight text-transparent">
        MoeBangumi
      </span>
    </div>
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
      <Sidebar collapsible="icon" className="border-r-0">
        <div className="pointer-events-none absolute inset-0 bg-linear-to-b from-chart-1/5 via-transparent to-chart-3/5" />

        <SidebarHeader className="relative z-10 p-4">
          <MoeLogo />
        </SidebarHeader>

        <SidebarContent className="relative z-10 pt-4">
          <SidebarGroup className="py-4">
            <SidebarMenu className="gap-4">
              <SidebarMenuItem>
                <SidebarMenuButton
                  className="rounded-xl text-base transition-all duration-300 hover:bg-chart-1/10 data-active:bg-chart-1/15 data-active:text-chart-1 [&_svg]:size-5 data-active:[&_svg]:text-chart-1"
                  isActive={activeItem === "anime"}
                  onClick={() => setActiveItem("anime")}
                  tooltip="动漫"
                >
                  <IconDeviceTv />
                  <span>动漫</span>
                </SidebarMenuButton>
              </SidebarMenuItem>
              <SidebarMenuItem>
                <SidebarMenuButton
                  className="rounded-xl text-base transition-all duration-300 hover:bg-chart-1/10 data-active:bg-chart-1/15 data-active:text-chart-1 [&_svg]:size-5 data-active:[&_svg]:text-chart-1"
                  isActive={activeItem === "schedule"}
                  onClick={() => setActiveItem("schedule")}
                  tooltip="每日放送"
                >
                  <IconCalendarWeek />
                  <span>每日放送</span>
                </SidebarMenuButton>
              </SidebarMenuItem>
              <SidebarMenuItem>
                <SidebarMenuButton
                  className="rounded-xl text-base transition-all duration-300 hover:bg-chart-1/10 data-active:bg-chart-1/15 data-active:text-chart-1 [&_svg]:size-5 data-active:[&_svg]:text-chart-1"
                  isActive={activeItem === "settings"}
                  onClick={() => setActiveItem("settings")}
                  tooltip="设置"
                >
                  <IconSettings />
                  <span>设置</span>
                </SidebarMenuButton>
              </SidebarMenuItem>
            </SidebarMenu>
          </SidebarGroup>
        </SidebarContent>
      </Sidebar>

      <SidebarInset className="bg-background">
        <header className="relative flex h-14 shrink-0 items-center justify-end border-b border-border/50 px-4">
          <div className="absolute inset-x-0 top-0 h-px bg-linear-to-r from-transparent via-chart-2/30 to-transparent" />

          <div className="flex items-center gap-2">
            <AddBangumiButton onClick={() => setSearchModalOpen(true)} />
            <ThemeColorSelector />
            <ThemeToggleButton />
          </div>
        </header>

        <SearchBangumiModal
          open={searchModalOpen}
          onOpenChange={setSearchModalOpen}
          onSelect={handleSelectBangumi}
        />

        {selectedSubject && (
          <AddBangumiModal
            open={addModalOpen}
            onOpenChange={setAddModalOpen}
            subject={selectedSubject}
            onSuccess={handleAddSuccess}
          />
        )}

        <main className="flex-1 overflow-auto">{children}</main>
      </SidebarInset>
    </SidebarProvider>
  );
}
