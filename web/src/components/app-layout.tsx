import * as React from "react";
import { SidebarInset, SidebarProvider } from "@/components/ui/sidebar";
import { Button } from "@/components/ui/button";
import { IconPlus } from "@tabler/icons-react";
import { SearchBangumiModal, BangumiModal } from "@/features/bangumi/components";
import { type BgmtvSearchItem } from "@/lib/api";
import { ThemeColorSelector } from "@/components/theme-color-selector";
import { ThemeToggleButton } from "@/components/theme-toggle-button";
import { AppSidebar } from "@/components/app-sidebar";
import { subjectToModalData } from "@/lib/converters";

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
    </Button>
  );
}


export function AppLayout({ children }: AppLayoutProps) {
  const [searchModalOpen, setSearchModalOpen] = React.useState(false);
  const [addModalOpen, setAddModalOpen] = React.useState(false);
  const [selectedSubject, setSelectedSubject] = React.useState<BgmtvSearchItem | null>(null);

  const handleSelectBangumi = (subject: BgmtvSearchItem) => {
    setSelectedSubject(subject);
    setAddModalOpen(true);
  };

  const handleAddSuccess = () => {
    setSelectedSubject(null);
  };

  return (
    <SidebarProvider style={{ "--sidebar-width": "14rem" } as React.CSSProperties}>
      <AppSidebar />

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
          <BangumiModal
            open={addModalOpen}
            onOpenChange={setAddModalOpen}
            mode="add"
            data={subjectToModalData(selectedSubject)}
            onSuccess={handleAddSuccess}
          />
        )}

        <main className="flex-1 overflow-auto">{children}</main>
      </SidebarInset>
    </SidebarProvider>
  );
}
