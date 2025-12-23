import {
  BangumiCard,
  BangumiGrid,
  demoBangumiList,
} from "@/features/bangumi/components";
import { IconSparkles } from "@tabler/icons-react";
import { Button } from "@/components/ui/button";

export function BangumiPage() {
  return (
    <div className="min-h-full bg-linear-to-br from-chart-1/5 via-background to-chart-3/5 dark:from-zinc-950 dark:via-background dark:to-chart-3/10">
      {/* Decorative background elements */}
      <div className="pointer-events-none fixed inset-0 overflow-hidden">
        <div className="absolute -left-40 -top-40 size-80 rounded-full bg-chart-1/20 blur-3xl dark:bg-chart-1/10" />
        <div className="absolute -right-40 top-1/3 size-96 rounded-full bg-chart-3/20 blur-3xl dark:bg-chart-3/10" />
        <div className="absolute -bottom-40 left-1/3 size-80 rounded-full bg-chart-5/20 blur-3xl dark:bg-chart-5/10" />
      </div>

      {/* Content */}
      <div className="relative">
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
            <div className="mb-4 flex size-20 items-center justify-center rounded-full bg-linear-to-br from-chart-1/20 to-chart-3/20 dark:from-chart-1/30 dark:to-chart-3/30">
              <IconSparkles className="size-10 text-chart-1 dark:text-chart-3" />
            </div>
            <h3 className="mb-2 text-lg font-semibold text-foreground">
              还没有番剧哦
            </h3>
            <p className="mb-6 max-w-sm text-sm text-muted-foreground">
              点击右上角的「添加番剧」按钮，开始追踪你喜爱的动漫作品吧！
            </p>
            <Button className="gap-2 bg-linear-to-r from-chart-1 to-chart-3 text-white shadow-lg shadow-chart-3/30 hover:opacity-90">
              <IconSparkles className="size-4" />
              添加第一部番剧
            </Button>
          </div>
        </div>
      </div>
    </div>
  );
}
