import { createFileRoute } from "@tanstack/react-router";
import { BangumiPage } from "@/features/bangumi/page";

export const Route = createFileRoute("/")({
  component: BangumiPage,
});
