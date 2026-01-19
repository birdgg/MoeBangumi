// Proxy section - disabled since backend API doesn't support proxy settings yet

export interface ProxySectionProps {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  form: any;
}

export function ProxySection(_props: ProxySectionProps) {
  return (
    <section className="space-y-5">
      <div className="rounded-lg border border-border/50 bg-muted/20 p-4">
        <p className="text-sm text-muted-foreground">
          代理设置功能暂未实现
        </p>
      </div>
    </section>
  );
}
