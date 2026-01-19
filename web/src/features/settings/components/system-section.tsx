// System section - disabled until version API is implemented
// The following APIs are not yet available: getVersion, checkUpdate, performUpdate

export function SystemSection() {
  return (
    <section className="space-y-5">
      <div className="rounded-lg border border-border/50 bg-muted/20 p-4">
        <div className="flex items-center gap-2 pb-3">
          <span className="text-sm font-medium">版本信息</span>
        </div>
        <p className="text-sm text-muted-foreground">
          版本检测功能暂未实现
        </p>
      </div>
    </section>
  );
}
