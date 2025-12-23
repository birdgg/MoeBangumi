import { createFileRoute } from "@tanstack/react-router";
import { Input } from "@/components/ui/input";
import {
  BangumiCard,
  BangumiGrid,
  demoBangumiList,
} from "@/features/bangumi/components";
import { IconSearch, IconSparkles, IconFilter } from "@tabler/icons-react";
import { Button } from "@/components/ui/button";

export const Route = createFileRoute("/")({
  component: Index,
});

function Index() {
  return (
    <div className="min-h-full bg-gradient-to-br from-pink-50/50 via-background to-purple-50/30 dark:from-zinc-950 dark:via-background dark:to-purple-950/20">
      {/* Decorative background elements */}
      <div className="pointer-events-none fixed inset-0 overflow-hidden">
        <div className="absolute -left-40 -top-40 size-80 rounded-full bg-pink-200/20 blur-3xl dark:bg-pink-900/10" />
        <div className="absolute -right-40 top-1/3 size-96 rounded-full bg-purple-200/20 blur-3xl dark:bg-purple-900/10" />
        <div className="absolute -bottom-40 left-1/3 size-80 rounded-full bg-cyan-200/20 blur-3xl dark:bg-cyan-900/10" />
      </div>

      {/* Content */}
      <div className="relative">
        {/* Header section */}
        <section className="border-b border-pink-200/30 dark:border-purple-500/10 px-6 py-8 md:px-8">

          {/* Search and filter bar */}
          <div className="flex flex-col gap-3 sm:flex-row sm:items-center">
            <div className="relative flex-1 max-w-md">
              <IconSearch className="absolute left-3 top-1/2 size-4 -translate-y-1/2 text-pink-400/60 dark:text-purple-400/60" />
              <Input
                placeholder="搜索番剧名称..."
                className="pl-10 border-pink-200/50 dark:border-purple-500/20 bg-white/70 dark:bg-zinc-900/70 backdrop-blur-sm focus:border-pink-400 dark:focus:border-purple-400 focus:ring-pink-400/20 dark:focus:ring-purple-400/20"
              />
            </div>
            <div className="flex items-center gap-2">
              <Button
                variant="outline"
                size="sm"
                className="gap-2 border-pink-200/50 dark:border-purple-500/20 hover:bg-pink-50 dark:hover:bg-purple-900/20"
              >
                <IconFilter className="size-4" />
                筛选
              </Button>
              <div className="flex rounded-lg border border-pink-200/50 dark:border-purple-500/20 p-1 bg-white/50 dark:bg-zinc-900/50 backdrop-blur-sm">
                <button className="rounded-md px-3 py-1 text-xs font-medium bg-gradient-to-r from-pink-400 to-purple-400 text-white shadow-sm">
                  全部
                </button>
                <button className="rounded-md px-3 py-1 text-xs font-medium text-muted-foreground hover:text-foreground transition-colors">
                  在看
                </button>
                <button className="rounded-md px-3 py-1 text-xs font-medium text-muted-foreground hover:text-foreground transition-colors">
                  完结
                </button>
              </div>
            </div>
          </div>
        </section>

        {/* Bangumi cards grid */}
        <section className="px-6 py-8 md:px-8">
          <BangumiGrid>
            {demoBangumiList.map((bangumi, index) => (
              <BangumiCard
                key={bangumi.id}
                bangumi={bangumi}
                style={{
                  animationDelay: `${index * 100}ms`,
                }}
              />
            ))}
          </BangumiGrid>
        </section>

        {/* Empty state (hidden by default, shown when no results) */}
        <div className="hidden px-6 py-16 md:px-8">
          <div className="flex flex-col items-center justify-center text-center">
            <div className="mb-4 flex size-20 items-center justify-center rounded-full bg-gradient-to-br from-pink-100 to-purple-100 dark:from-pink-900/30 dark:to-purple-900/30">
              <IconSparkles className="size-10 text-pink-400 dark:text-purple-400" />
            </div>
            <h3 className="mb-2 text-lg font-semibold text-foreground">
              还没有番剧哦
            </h3>
            <p className="mb-6 max-w-sm text-sm text-muted-foreground">
              点击右上角的「添加番剧」按钮，开始追踪你喜爱的动漫作品吧！
            </p>
            <Button className="gap-2 bg-linear-to-r from-pink-400 to-purple-500 text-white shadow-lg shadow-purple-500/30 hover:opacity-90">
              <IconSparkles className="size-4" />
              添加第一部番剧
            </Button>
          </div>
        </div>
      </div>
    </div>
  );
}
