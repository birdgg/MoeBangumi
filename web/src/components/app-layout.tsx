import * as React from "react";
import { SidebarInset, SidebarProvider } from "@/components/ui/sidebar";
import { Button } from "@/components/ui/button";
import { IconPlus, IconRefresh } from "@tabler/icons-react";
import { SearchBangumiModal, BangumiModal, type BangumiModalData } from "@/features/bangumi/components";
import { type Subject, triggerRssFetchMutation } from "@/lib/api";
import { ThemeColorSelector } from "@/components/theme-color-selector";
import { ThemeToggleButton } from "@/components/theme-toggle-button";
import { AppSidebar } from "@/components/app-sidebar";
import { useMutation } from "@tanstack/react-query";
import { parseBgmtvName } from "@/lib/parser";

// Convert Subject to BangumiModalData
function subjectToModalData(subject: Subject): BangumiModalData {
  const parsedChinese = parseBgmtvName(subject.name_cn || subject.name || "");
  const parsedJapanese = parseBgmtvName(subject.name || "");
  const season = parsedChinese.season ?? parsedJapanese.season ?? 1;

  return {
    bgmtvId: subject.id,
    titleChinese: parsedChinese.name,
    titleJapanese: subject.name_cn ? parsedJapanese.name : null,
    titleOriginalChinese: subject.name_cn,
    titleOriginalJapanese: subject.name,
    posterUrl: subject.image,
    year: subject.date ? parseInt(subject.date.split("-")[0]) : null,
    season,
    totalEpisodes: subject.eps,
    platform: subject.platform,
    airDate: subject.date,
    airWeek: subject.date ? new Date(subject.date).getDay() : null,
  };
}

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

function RefreshRssButton() {
  const mutation = useMutation(triggerRssFetchMutation());

  return (
    <Button
      variant="ghost"
      size="icon"
      className="size-9 rounded-lg text-muted-foreground transition-colors hover:bg-accent hover:text-accent-foreground"
      onClick={() => mutation.mutate({})}
      disabled={mutation.isPending}
      title="刷新 RSS"
    >
      <IconRefresh
        className={`size-4 ${mutation.isPending ? "animate-spin" : ""}`}
      />
    </Button>
  );
}

export function AppLayout({ children }: AppLayoutProps) {
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
    <SidebarProvider style={{ "--sidebar-width": "14rem" } as React.CSSProperties}>
      <AppSidebar />

      <SidebarInset className="bg-background">
        <header className="relative flex h-14 shrink-0 items-center justify-end border-b border-border/50 px-4">
          <div className="absolute inset-x-0 top-0 h-px bg-linear-to-r from-transparent via-chart-2/30 to-transparent" />

          <div className="flex items-center gap-2">
            <AddBangumiButton onClick={() => setSearchModalOpen(true)} />
            <RefreshRssButton />
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
